##
## Analises de janela por incidente de fatalidade -- disponibilidade do
## sistema e resposta a curtailments, restritas a [incident_date -
## days_before, incident_date] e as unidades IDF/turbina relevantes
##
## Complementa R/fatality_track_investigation.R (que olha para os tracks da
## especie envolvida): aqui olhamos para o DESEMPENHO DO SISTEMA nessa mesma
## janela -- se as unidades IDF que cobrem a turbina estiveram operacionais,
## e se os curtailments dessa turbina, nesses dias, responderam a tempo.
##
## Reutiliza os thresholds ja definidos noutras seccoes do userSettings_BSH.R
## -- nao introduz limiares novos:
##   - disponibilidade (3.1): heartbeat_offline_gap_min, heartbeat_interval_min
##   - resposta a curtailments (3.5): curtailment_start_end_gap_sec,
##     curtailment_max_next_gap_sec, curtailment_drop_pct_threshold,
##     safe_shutdown_rpm
##   - tempo de resposta (3.6): shutdown_time_thresholds, shutdown_time_high_cut
##
## Unidades IDF por turbina: resolvidas a partir da matriz manual
## (ACWA_IDF_Coverage_Matrix.xlsx, colunas Primary IDF + Secondary IDF(s)) --
## ver R/turbine_idf_coverage.R. Se a matriz nao estiver disponivel para essa
## turbina, cai em fallback_idf_units (ex: heartbeat_idf_units).
##
## Depende de: data.table, lubridate, suncalc, ggplot2,
## R/availability_daylight.R, R/curtailment_response.R,
## R/curtailment_shutdown_time.R (fazer source destes 3 antes)
##
## Uso:
##   source("R/fatality_window_analysis.R")
##
##   idf_units <- resolve_incident_idf_units("BSH54", turbine_idf_manual_dt)
##
##   avail_i <- summarise_availability_window(
##     heartb_dt, idf_units, window_from, window_to, proj_lat, proj_lon, proj_timezone,
##     offline_gap_min = heartbeat_offline_gap_min, online_grace_min = heartbeat_interval_min
##   )
##
##   response_i <- summarise_curtailment_response_window(
##     curtl_dt, scada_dt, turbine_id = "BSH54", window_from, window_to,
##     start_end_gap_sec = curtailment_start_end_gap_sec,
##     max_next_gap_sec = curtailment_max_next_gap_sec,
##     drop_pct_threshold = curtailment_drop_pct_threshold,
##     rpm_threshold = safe_shutdown_rpm,
##     shutdown_thresholds = shutdown_time_thresholds,
##     shutdown_high_cut_sec = shutdown_time_high_cut
##   )
##
##   # todos os incidentes de uma vez -- ver fatality_incidents em userSettings_BSH.R
##   all_windows <- summarise_fatality_windows(
##     fatality_incidents, heartb_dt, curtl_dt, scada_dt, turbine_idf_manual_dt,
##     proj_lat, proj_lon, proj_timezone,
##     offline_gap_min = heartbeat_offline_gap_min, online_grace_min = heartbeat_interval_min,
##     start_end_gap_sec = curtailment_start_end_gap_sec,
##     max_next_gap_sec = curtailment_max_next_gap_sec,
##     drop_pct_threshold = curtailment_drop_pct_threshold,
##     rpm_threshold = safe_shutdown_rpm,
##     shutdown_thresholds = shutdown_time_thresholds,
##     shutdown_high_cut_sec = shutdown_time_high_cut,
##     fallback_idf_units = heartbeat_idf_units
##   )
##


## 1. Unidades IDF relevantes para uma turbina, a partir da matriz manual ----

resolve_incident_idf_units <- function(turbine_id, manual_matrix_dt) {

  manual <- data.table::as.data.table(manual_matrix_dt)
  data.table::setnames(
    manual,
    old = c("Turbine ID", "Primary IDF", "Secondary IDF(s)"),
    new = c("turbine", "primary", "secondary_raw"),
    skip_absent = TRUE
  )

  row <- manual[turbine == turbine_id]
  if (nrow(row) == 0L) return(character())

  secondary <- character()
  if (!is.na(row$secondary_raw[1]) && row$secondary_raw[1] != "") {
    secondary <- trimws(unlist(strsplit(row$secondary_raw[1], ",")))
  }

  units <- c(row$primary[1], secondary)
  unique(units[!is.na(units) & units != ""])
}


## 2. Disponibilidade do sistema, restrita a uma janela + unidades IDF ----

summarise_availability_window <- function(heartb_dt, idf_units, window_from, window_to,
                                          lat, lon, tz, offline_gap_min = 60, online_grace_min = 30) {

  daylight_cal <- build_daylight_calendar(as.Date(window_from), as.Date(window_to), lat, lon, tz)

  empty <- list(
    daily = data.table::data.table(idf = character(), date = as.Date(character())),
    by_idf = data.table::data.table(idf = character()),
    idf_units = idf_units
  )

  if (length(idf_units) == 0L) return(empty)

  hb <- heartb_dt[idf %in% idf_units & timestamp >= window_from & timestamp <= window_to]
  if (nrow(hb) == 0L) return(empty)

  daily_dt <- daylight_availability(hb, daylight_cal, tz, offline_gap_min, online_grace_min)
  by_idf   <- summarise_availability(daily_dt)$by_idf

  list(daily = daily_dt, by_idf = by_idf, idf_units = idf_units)
}


## 3. Resposta a curtailments, restrita a uma janela + turbina ----
##
## response_flag, por curtailment:
##   "missed"  -- a turbina nao confirmou ter parado (final_status
##                partial_or_no_stop/no_data, ver R/curtailment_response.R)
##   "delayed" -- parou, mas demorou mais que shutdown_high_cut_sec a atingir
##                o 1º limiar de rpm (o maior valor de shutdown_thresholds)
##   "ok"      -- parou dentro do tempo esperado

summarise_curtailment_response_window <- function(curtl_dt, scada_dt, turbine_id, window_from, window_to,
                                                   start_end_gap_sec = 2, max_next_gap_sec = 20,
                                                   drop_pct_threshold = 0.10, rpm_threshold = 1,
                                                   shutdown_thresholds = c(2, 1, 0), shutdown_high_cut_sec = 50) {

  curtl_window <- curtl_dt[turbine == turbine_id & start >= window_from & start <= window_to]

  empty_detail <- data.table::data.table(
    curtailment_id = integer(), turbine = character(), track_id = character(),
    species = character(), start = as.POSIXct(character()), end = as.POSIXct(character()),
    final_status = character(), no_immediate_response = logical(),
    time_to_first_threshold_sec = numeric(), response_flag = character()
  )
  if (nrow(curtl_window) == 0L) {
    return(list(detail = empty_detail, by_flag = data.table::data.table(response_flag = character(), n = integer())))
  }

  assess_dt <- assess_curtailment_response(
    curtl_window, scada_dt, start_end_gap_sec = start_end_gap_sec,
    max_next_gap_sec = max_next_gap_sec, drop_pct_threshold = drop_pct_threshold,
    rpm_threshold = rpm_threshold
  )

  tt_dt <- time_to_rpm_thresholds(
    curtl_window, scada_dt, thresholds = shutdown_thresholds, start_end_gap_sec = start_end_gap_sec
  )
  first_threshold <- max(shutdown_thresholds)
  tt_first <- tt_dt[threshold == first_threshold, .(curtailment_id, time_to_first_threshold_sec = time_to_threshold_sec)]

  out <- merge(assess_dt, tt_first, by = "curtailment_id", all.x = TRUE)

  out[, response_flag := data.table::fcase(
    final_status %in% c("partial_or_no_stop", "no_data"), "missed",
    !is.na(time_to_first_threshold_sec) & time_to_first_threshold_sec > shutdown_high_cut_sec, "delayed",
    default = "ok"
  )]

  by_flag <- out[, .(n = .N), by = response_flag]
  data.table::setorder(by_flag, -n)

  list(detail = out[], by_flag = by_flag[])
}


## 4. Todos os incidentes de uma vez -- disponibilidade + resposta a
##    curtailments, por incidente, na respetiva janela ----

summarise_fatality_windows <- function(fatality_incidents, heartb_dt, curtl_dt, scada_dt,
                                       manual_matrix_dt = NULL, lat, lon, tz,
                                       offline_gap_min = 60, online_grace_min = 30,
                                       start_end_gap_sec = 2, max_next_gap_sec = 20,
                                       drop_pct_threshold = 0.10, rpm_threshold = 1,
                                       shutdown_thresholds = c(2, 1, 0), shutdown_high_cut_sec = 50,
                                       fallback_idf_units = NULL) {

  res <- lapply(seq_len(nrow(fatality_incidents)), function(i) {
    inc <- fatality_incidents[i]
    window_from <- as.POSIXct(paste(inc$incident_date - inc$days_before, "00:00:00"), tz = tz)
    window_to   <- as.POSIXct(paste(inc$incident_date, "23:59:59"), tz = tz)

    idf_units <- if (!is.null(manual_matrix_dt)) resolve_incident_idf_units(inc$turbine, manual_matrix_dt) else character()
    if (length(idf_units) == 0L) idf_units <- fallback_idf_units

    avail <- summarise_availability_window(
      heartb_dt, idf_units, window_from, window_to, lat, lon, tz,
      offline_gap_min = offline_gap_min, online_grace_min = online_grace_min
    )

    response <- summarise_curtailment_response_window(
      curtl_dt, scada_dt, inc$turbine, window_from, window_to,
      start_end_gap_sec = start_end_gap_sec, max_next_gap_sec = max_next_gap_sec,
      drop_pct_threshold = drop_pct_threshold, rpm_threshold = rpm_threshold,
      shutdown_thresholds = shutdown_thresholds, shutdown_high_cut_sec = shutdown_high_cut_sec
    )

    list(
      incident_id = inc$incident_id, turbine = inc$turbine, idf_units = idf_units,
      window_from = window_from, window_to = window_to,
      availability = avail, curtailment_response = response
    )
  })

  names(res) <- fatality_incidents$incident_id
  res
}
