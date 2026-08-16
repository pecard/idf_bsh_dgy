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
## R/curtailment_shutdown_time.R, R/curtailment_response_classify.R
## (fazer source destes 4 antes) -- R/track_min_individuals.R tambem, para a
## comparacao pre/pos-incidente (secao 5 deste ficheiro)
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
##     fallback_idf_units = heartbeat_idf_units,
##     track_dt = track_dt, post_days = fatality_post_incident_days,
##     min_indiv_bin_min = min_individuals_bin_min, min_indiv_merge_dist_m = min_individuals_merge_dist_m,
##     global_avail_from = ini, global_avail_to = end,                # baseline global vs. janela
##     global_response_from = scada_ini, global_response_to = scada_end
##   )
##   all_windows$BSH_0002$abundance # pre/pos-incidente, so' esse incidente
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

  # classificacao missed/delayed/ok -- regra partilhada com a timeline
  # farm-wide, ver R/curtailment_response_classify.R
  out <- classify_response_flag(
    curtl_window, scada_dt, start_end_gap_sec = start_end_gap_sec,
    max_next_gap_sec = max_next_gap_sec, drop_pct_threshold = drop_pct_threshold,
    rpm_threshold = rpm_threshold, shutdown_thresholds = shutdown_thresholds,
    shutdown_high_cut_sec = shutdown_high_cut_sec
  )

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
                                       fallback_idf_units = NULL,
                                       track_dt = NULL, post_days = 3,
                                       min_indiv_bin_min = 2, min_indiv_merge_dist_m = 200,
                                       global_avail_from = NULL, global_avail_to = NULL,
                                       global_response_from = NULL, global_response_to = NULL) {

  # baseline "global" (todo o periodo monitorizado, INCLUINDO a propria
  # janela do incidente -- decisao deliberada, ver R/fatality_window_analysis.R)
  # -- reutiliza as mesmas 2 funcoes de janela, so com bounds mais largos.
  # disponibilidade e resposta a curtailments tem bounds SEPARADOS de proposito
  # -- heartb_dt cobre [ini, end], mas curtl_scada_dt/scada_dt so tem dados
  # fiaveis em [scada_ini, scada_end] (mais curto); usar o mesmo intervalo
  # para os dois inflacionaria "missed" com um "buraco" de SCADA que nao
  # existia ainda, nao com falta real de resposta.
  # *_from/*_to = NULL desliga essa comparacao especifica (fica so a janela).

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

    avail_global <- NULL
    if (!is.null(global_avail_from) && !is.null(global_avail_to)) {
      avail_global <- summarise_availability_window(
        heartb_dt, idf_units, global_avail_from, global_avail_to, lat, lon, tz,
        offline_gap_min = offline_gap_min, online_grace_min = online_grace_min
      )
    }

    response_global <- NULL
    if (!is.null(global_response_from) && !is.null(global_response_to)) {
      response_global <- summarise_curtailment_response_window(
        curtl_dt, scada_dt, inc$turbine, global_response_from, global_response_to,
        start_end_gap_sec = start_end_gap_sec, max_next_gap_sec = max_next_gap_sec,
        drop_pct_threshold = drop_pct_threshold, rpm_threshold = rpm_threshold,
        shutdown_thresholds = shutdown_thresholds, shutdown_high_cut_sec = shutdown_high_cut_sec
      )
    }

    abundance <- if (!is.null(track_dt)) {
      summarise_individuals_pre_post(
        track_dt, inc$species, inc$incident_date, inc$days_before, post_days = post_days,
        bin_min = min_indiv_bin_min, merge_dist_m = min_indiv_merge_dist_m, tz = tz
      )
    } else {
      NULL
    }

    list(
      incident_id = inc$incident_id, turbine = inc$turbine, idf_units = idf_units,
      window_from = window_from, window_to = window_to,
      availability = avail, curtailment_response = response,
      availability_global = avail_global, curtailment_response_global = response_global,
      abundance = abundance
    )
  })

  names(res) <- fatality_incidents$incident_id
  res
}


## 5. Abundancia (min individuals) pre- e pos-incidente ----
##
## Compara, de forma PURAMENTE DESCRITIVA, os individuos minimos estimados
## (ver R/track_min_individuals.R) na janela pre-incidente ([incident_date -
## days_before, incident_date], a mesma das outras analises desta seccao)
## com os post_days dias seguintes ([incident_date + 1, incident_date +
## post_days]). Uma diferenca entre as duas janelas pode refletir o proprio
## incidente OU apenas a fase natural do movimento migratorio da especie a
## passar -- nao se assume nenhuma relacao causal (ver CLAUDE.md: nao
## sobre-interpretar).

summarise_individuals_pre_post <- function(track_dt, species, incident_date, days_before, post_days = 3,
                                           bin_min = 2, merge_dist_m = 200, tz = NULL) {

  if (is.null(tz)) tz <- attr(track_dt$timestamp, "tzone")
  incident_date <- as.Date(incident_date)

  pre_from  <- as.POSIXct(paste(incident_date - days_before, "00:00:00"), tz = tz)
  pre_to    <- as.POSIXct(paste(incident_date, "23:59:59"), tz = tz)
  post_from <- as.POSIXct(paste(incident_date + 1, "00:00:00"), tz = tz)
  post_to   <- as.POSIXct(paste(incident_date + post_days, "23:59:59"), tz = tz)

  pre_bins  <- count_min_individuals_per_bin(track_dt, species, bin_min, merge_dist_m, date_from = pre_from, date_to = pre_to)
  post_bins <- count_min_individuals_per_bin(track_dt, species, bin_min, merge_dist_m, date_from = post_from, date_to = post_to)

  summarise_period <- function(bins_dt, period_label, period_from, period_to) {
    n_days <- as.numeric(difftime(as.Date(period_to), as.Date(period_from), units = "days")) + 1
    if (nrow(bins_dt) == 0L) {
      return(data.table::data.table(
        period = period_label, n_days = n_days, n_bins = 0L,
        peak_individuals = NA_integer_, mean_individuals = NA_real_
      ))
    }
    data.table::data.table(
      period = period_label, n_days = n_days, n_bins = nrow(bins_dt),
      peak_individuals = max(bins_dt$n_individuals_min),
      mean_individuals = round(mean(bins_dt$n_individuals_min), 2)
    )
  }

  data.table::rbindlist(list(
    summarise_period(pre_bins, "pre_incident", pre_from, pre_to),
    summarise_period(post_bins, "post_incident", post_from, post_to)
  ))
}
