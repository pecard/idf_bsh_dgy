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
##   - resposta a curtailments/latencia (3.5-3.6b): curtailment_start_end_gap_sec,
##     curtailment_latency_decline_pct, shutdown_time_buffer_sec, curtailment_cutin_rpm
##
## Unidades IDF por turbina: resolvidas a partir da matriz manual
## (ACWA_IDF_Coverage_Matrix.xlsx, colunas Primary IDF + Secondary IDF(s)) --
## ver R/turbine_idf_coverage.R. Se a matriz nao estiver disponivel para essa
## turbina, cai em fallback_idf_units (ex: heartbeat_idf_units).
##
## Depende de: data.table, lubridate, suncalc, ggplot2,
## R/availability_daylight.R, R/curtailment_response.R,
## R/curtailment_response_latency.R (fazer source destes 3 antes) --
## R/track_min_individuals.R tambem, para a comparacao pre/pos-incidente
## (secao 5 deste ficheiro), e R/offline_curtailment_check.R para a
## evidencia offline (curtailment/SCADA), secao 2b abaixo -- pedido do
## Paulo, 2026-10: trazer para o relatorio de incidente a MESMA metodologia
## de cruzamento heartbeat+curtailments+SCADA ja usada no relatorio mensal
## (R/offline_curtailment_check.R, secção "Unavailability Summary"/"Offline
## Evidence" do relatorio), restrita a janela/baseline do incidente em vez
## de farm-wide.
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
##     decline_pct_threshold = curtailment_latency_decline_pct,
##     buffer_after_end_sec = shutdown_time_buffer_sec,
##     cutin_rpm = curtailment_cutin_rpm
##   )
##
##   # todos os incidentes de uma vez -- ver fatality_incidents em userSettings_BSH.R
##   all_windows <- summarise_fatality_windows(
##     fatality_incidents, heartb_dt, curtl_dt, scada_dt, turbine_idf_manual_dt,
##     proj_lat, proj_lon, proj_timezone,
##     offline_gap_min = heartbeat_offline_gap_min, online_grace_min = heartbeat_interval_min,
##     start_end_gap_sec = curtailment_start_end_gap_sec,
##     decline_pct_threshold = curtailment_latency_decline_pct,
##     buffer_after_end_sec = shutdown_time_buffer_sec,
##     cutin_rpm = curtailment_cutin_rpm,
##     fallback_idf_units = heartbeat_idf_units,
##     track_dt = track_dt, post_days = fatality_post_incident_days,
##     min_indiv_bin_min = min_individuals_bin_min, min_indiv_merge_dist_m = min_individuals_merge_dist_m,
##     global_avail_from = ini, global_avail_to = end,                # baseline global vs. janela
##     global_response_from = scada_ini, global_response_to = scada_end
##   )
##   all_windows$BSH_0002$abundance # pre/pos-incidente, so' esse incidente
##   all_windows$BSH_0002$offline_evidence$overall  # evidencia offline, janela
##   all_windows$BSH_0002$offline_evidence_global$overall  # idem, baseline global
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

## Unidades de idf_units SEM heartbeats em hb que devem contar como offline a
## janela toda: conhecidas (known_units) e com a janela dentro da cobertura
## dos dados (data_range sobrepoe-se a [window_from, window_to]).
absent_known_units <- function(hb, idf_units, known_units, window_from, window_to, data_range = NULL) {
  if (is.null(known_units)) return(character())
  if (!is.null(data_range) && (data_range[2] <= window_from || data_range[1] >= window_to)) return(character())
  intersect(setdiff(idf_units, unique(hb$idf)), known_units)
}

summarise_availability_window <- function(heartb_dt, idf_units, window_from, window_to,
                                          lat, lon, tz, offline_gap_min = 60, online_grace_min = 30,
                                          data_range = NULL, known_units = NULL) {

  # as.Date() sem tz= usa UTC por omissao -- window_from as 00:00:00 local
  # (Asia/Samarkand, UTC+5) cai as 19:00 do dia ANTERIOR em UTC, empurrando
  # daylight_cal para tras 1 dia (janela de N+1 dias em vez de N) -- mesma
  # familia dos bugs de tz ja corrigidos no resto do projeto
  daylight_cal <- build_daylight_calendar(as.Date(window_from, tz = tz), as.Date(window_to, tz = tz), lat, lon, tz)

  # Unidade SEM nenhum heartbeat na janela:
  #  - CONHECIDA (known_units: existe noutro periodo nos dados de heartbeat) e a
  #    janela esta' coberta pelos dados (data_range) -> 100% offline durante a
  #    janela (nao reportou nada enquanto o sistema/dados existiam);
  #  - desconhecida (nunca aparece nos dados: codigo errado? sem dados?) ->
  #    nao avaliada (NA / "Not evaluated"), NUNCA 0% offline: 0% significaria
  #    "avaliada e sempre online".
  not_evaluated_rows <- function(units) {
    data.table::data.table(
      idf = units,
      offline_mins_total = NA_real_, daylight_mins_total = NA_real_, days_covered = 0L,
      monitoring_period_pct = NA_real_, availability_pct = NA_real_,
      n_heartbeats = 0L,
      availability_cat = factor("Not evaluated", levels = c("<60%", "60-80%", "80-90%", "90-96%", ">96%", "Not evaluated"))
    )
  }

  empty <- list(
    daily = data.table::data.table(idf = character(), date = as.Date(character())),
    by_idf = data.table::data.table(idf = character()),
    idf_units = idf_units
  )

  if (length(idf_units) == 0L) return(empty)

  hb <- heartb_dt[idf %in% idf_units & timestamp >= window_from & timestamp <= window_to]
  absent <- absent_known_units(hb, idf_units, known_units, window_from, window_to, data_range)
  if (nrow(hb) == 0L && length(absent) == 0L) {
    empty$by_idf <- not_evaluated_rows(idf_units)
    return(empty)
  }

  daily_dt <- daylight_availability(hb, daylight_cal, tz, offline_gap_min, online_grace_min, data_range = data_range, units = absent)
  by_idf   <- summarise_availability(daily_dt)$by_idf
  by_idf[, availability_cat := factor(availability_cat, levels = c(levels(availability_cat), "Not evaluated"))]
  counts <- hb[, .(n_heartbeats = .N), by = idf]
  by_idf[counts, on = "idf", n_heartbeats := i.n_heartbeats]
  by_idf[is.na(n_heartbeats), n_heartbeats := 0L]

  missing_units <- setdiff(idf_units, by_idf$idf)
  if (length(missing_units) > 0L) by_idf <- data.table::rbindlist(list(by_idf, not_evaluated_rows(missing_units)), use.names = TRUE)

  list(daily = daily_dt, by_idf = by_idf, idf_units = idf_units)
}


## 2b. Evidencia offline (curtailment/SCADA durante os gaps de heartbeat),
## restrita a uma janela + unidades IDF + turbina -- MESMA metodologia de
## R/offline_curtailment_check.R ja usada no relatorio mensal (secção
## "Unavailability Summary"/"Offline Evidence") -- pedido do Paulo, 2026-10.
##
## Ao contrario do uso farm-wide dessas funcoes no relatorio mensal (onde
## idf_turbines_dt precisa de resolve_idf_turbines(), matriz manual OU
## fallback geometrico, porque ha' varias turbinas a atribuir), aqui so' ha'
## 1 turbina (a do proprio incidente) e as unidades IDF ja' resolvidas para
## ela (idf_units, mesmo vetor usado por summarise_availability_window()
## acima) -- idf_turbines_dt e' por isso construido diretamente, sem
## ambiguidade nenhuma a resolver.
##
## Depende de R/offline_curtailment_check.R estar sourced pelo chamador
## (check_offline_curtailment_overlap(), check_offline_scada_presence(),
## classify_offline_evidence(), summarise_offline_evidence()) -- mesmo
## padrao de dependencia "sourced pelo chamador" das restantes funcoes
## deste ficheiro (ver cabecalho).
##
## combined: intervalos offline classificados (idf, off_start, off_end,
## classification) -- usado a jusante pelo calendario categorico
## (offline_evidence_slot_grid()/plot_offline_evidence_slots(),
## R/availability_daylight.R). overall/by_idf: ver summarise_offline_evidence(),
## R/offline_curtailment_check.R.

summarise_offline_evidence_window <- function(heartb_dt, idf_units, turbine_id, window_from, window_to,
                                              curtl_dt, scada_dt, lat, lon, tz,
                                              offline_gap_min = 60, online_grace_min = 30,
                                              track_dt = NULL, idf_turbines_dt = NULL, data_range = NULL, known_units = NULL) {

  empty <- list(
    combined = data.table::data.table(
      idf = character(), off_start = as.POSIXct(character()), off_end = as.POSIXct(character()),
      classification = character()
    ),
    overall = data.table::data.table(
      classification = character(), n_intervals = integer(), total_mins = numeric(), pct_of_total = numeric()
    ),
    by_idf = data.table::data.table(
      idf = character(), classification = character(), n_intervals = integer(), total_mins = numeric()
    )
  )

  if (length(idf_units) == 0L) return(empty)

  # mesmo bug de tz ja documentado em summarise_availability_window() acima
  daylight_cal <- build_daylight_calendar(as.Date(window_from, tz = tz), as.Date(window_to, tz = tz), lat, lon, tz)

  hb <- heartb_dt[idf %in% idf_units & timestamp >= window_from & timestamp <= window_to]
  absent <- absent_known_units(hb, idf_units, known_units, window_from, window_to, data_range)
  if (nrow(hb) == 0L && length(absent) == 0L) return(empty)

  offline_dt <- compute_offline_intervals(hb, offline_gap_min, online_grace_min,
                                          period = calendar_period(daylight_cal, tz), data_range = data_range, units = absent)
  if (nrow(offline_dt) == 0L) return(empty)

  # so' a porcao DIURNA de cada gap -- OBRIGATORIO antes de classificar,
  # mesma razao ja documentada em R/offline_curtailment_check.R (senao o
  # total classificado pode exceder offline_mins_total, dando
  # net_offline_pct negativo em summarise_net_availability())
  offline_dt_daylight <- clip_offline_intervals_to_daylight(offline_dt, daylight_cal, tz)
  if (nrow(offline_dt_daylight) == 0L) return(empty)

  # idf_turbines_dt (idf, turbine): relacao unidade -> turbina(s) que protege;
  # por omissao 1 turbina (a do incidente) para todas as unidades. Varias
  # turbinas/unidades de uma vez: R/idf_availability.R
  if (is.null(idf_turbines_dt)) idf_turbines_dt <- data.table::data.table(idf = idf_units, turbine = turbine_id)

  curtl_checked_dt <- check_offline_curtailment_overlap(offline_dt_daylight, curtl_dt, idf_turbines_dt, track_dt = track_dt)
  scada_checked_dt <- check_offline_scada_presence(offline_dt_daylight, scada_dt, idf_turbines_dt)
  combined_dt       <- classify_offline_evidence(curtl_checked_dt, scada_checked_dt)
  evidence_summary  <- summarise_offline_evidence(combined_dt)

  list(combined = combined_dt, overall = evidence_summary$overall, by_idf = evidence_summary$by_idf)
}


## 3. Resposta a curtailments (latencia/no-response), restrita a uma
##    janela + turbina ----
##
## Mesma classificacao de latencia/no-response da secção "Curtailment
## Response & Latency" do relatorio (R/curtailment_response_latency.R,
## time_to_first_decline()) -- substitui 2026-08 (pedido do Paulo) o antigo
## classify_response_flag() (missed/delayed/ok), que tinha aqui a mesma
## distorcao ja corrigida nas restantes secções: RPM verificado so' no
## instante exato do "end" da ordem, e sem excluir curtailments cuja
## turbina ja estava abaixo da velocidade de cut-in no "start".

summarise_curtailment_response_window <- function(curtl_dt, scada_dt, turbine_id, window_from, window_to,
                                                   start_end_gap_sec = 2, decline_pct_threshold = 0.10,
                                                   buffer_after_end_sec = 0, cutin_rpm = 0) {

  curtl_window <- curtl_dt[turbine == turbine_id & start >= window_from & start <= window_to]

  empty_detail <- data.table::data.table(
    curtailment_id = integer(), turbine = character(), track_id = character(),
    species = character(), start = as.POSIXct(character()), end = as.POSIXct(character()),
    start_rpm = numeric(), no_data = logical(), below_cutin = logical(),
    decline_time = as.POSIXct(character()), latency_sec = numeric()
  )
  if (nrow(curtl_window) == 0L) {
    return(list(detail = empty_detail, summary = summarise_latency(empty_detail)))
  }

  # ver R/curtailment_response_latency.R para o racional completo
  # (decline_pct_threshold relativo ao baseline, buffer_after_end_sec,
  # cutin_rpm)
  out <- time_to_first_decline(
    curtl_window, scada_dt, decline_pct_threshold = decline_pct_threshold,
    start_end_gap_sec = start_end_gap_sec, buffer_after_end_sec = buffer_after_end_sec,
    cutin_rpm = cutin_rpm
  )

  # resumo (1 linha) -- mesmas colunas de summarise_latency(),
  # R/curtailment_response_latency.R
  summary_dt <- summarise_latency(out)

  list(detail = out[], summary = summary_dt)
}


## 4. Todos os incidentes de uma vez -- disponibilidade + resposta a
##    curtailments, por incidente, na respetiva janela ----

summarise_fatality_windows <- function(fatality_incidents, heartb_dt, curtl_dt, scada_dt,
                                       manual_matrix_dt = NULL, lat, lon, tz,
                                       offline_gap_min = 60, online_grace_min = 30,
                                       start_end_gap_sec = 2, decline_pct_threshold = 0.10,
                                       buffer_after_end_sec = 0, cutin_rpm = 0,
                                       fallback_idf_units = NULL,
                                       track_dt = NULL, post_days = 3,
                                       min_indiv_bin_min = 2, min_indiv_merge_dist_m = 200,
                                       global_avail_from = NULL, global_avail_to = NULL,
                                       global_response_from = NULL, global_response_to = NULL,
                                       heartb_data_range = NULL, heartb_known_units = NULL) {

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
    if (length(idf_units) == 0L) {
      idf_units <- fallback_idf_units
    } else if (!is.null(fallback_idf_units) && !any(idf_units %in% heartb_dt$idf)) {
      # a matriz manual da turbina usa rotulos que NAO existem em heartb_dt$idf
      # (ex: "IDF-13" vs "IDF13") -- sem isto a disponibilidade ficava vazia
      # sem aviso; usa as unidades configuradas para o incidente
      message("AVISO: unidades IDF da matriz manual para ", inc$turbine, " (", paste(idf_units, collapse = ", "),
              ") nao existem em heartb_dt$idf -- a usar fallback_idf_units (", paste(fallback_idf_units, collapse = ", "), ").")
      idf_units <- fallback_idf_units
    }
    message("Janela ", inc$incident_id, ": ", format(window_from), " a ", format(window_to),
            " | unidades IDF: ", paste(idf_units, collapse = ", "),
            " | heartbeats na janela para essas unidades: ",
            sum(heartb_dt$idf %in% idf_units & heartb_dt$timestamp >= window_from & heartb_dt$timestamp <= window_to))

    avail <- summarise_availability_window(
      heartb_dt, idf_units, window_from, window_to, lat, lon, tz,
      offline_gap_min = offline_gap_min, online_grace_min = online_grace_min,
      data_range = heartb_data_range, known_units = heartb_known_units
    )

    offline_evidence <- summarise_offline_evidence_window(
      heartb_dt, idf_units, inc$turbine, window_from, window_to,
      curtl_dt, scada_dt, lat, lon, tz,
      offline_gap_min = offline_gap_min, online_grace_min = online_grace_min,
      track_dt = track_dt, data_range = heartb_data_range, known_units = heartb_known_units
    )

    response <- summarise_curtailment_response_window(
      curtl_dt, scada_dt, inc$turbine, window_from, window_to,
      start_end_gap_sec = start_end_gap_sec, decline_pct_threshold = decline_pct_threshold,
      buffer_after_end_sec = buffer_after_end_sec, cutin_rpm = cutin_rpm
    )

    avail_global <- NULL
    offline_evidence_global <- NULL
    if (!is.null(global_avail_from) && !is.null(global_avail_to)) {
      avail_global <- summarise_availability_window(
        heartb_dt, idf_units, global_avail_from, global_avail_to, lat, lon, tz,
        offline_gap_min = offline_gap_min, online_grace_min = online_grace_min,
        data_range = heartb_data_range, known_units = heartb_known_units
      )
      offline_evidence_global <- summarise_offline_evidence_window(
        heartb_dt, idf_units, inc$turbine, global_avail_from, global_avail_to,
        curtl_dt, scada_dt, lat, lon, tz,
        offline_gap_min = offline_gap_min, online_grace_min = online_grace_min,
        track_dt = track_dt, data_range = heartb_data_range, known_units = heartb_known_units
      )
    }

    response_global <- NULL
    if (!is.null(global_response_from) && !is.null(global_response_to)) {
      response_global <- summarise_curtailment_response_window(
        curtl_dt, scada_dt, inc$turbine, global_response_from, global_response_to,
        start_end_gap_sec = start_end_gap_sec, decline_pct_threshold = decline_pct_threshold,
        buffer_after_end_sec = buffer_after_end_sec, cutin_rpm = cutin_rpm
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
      availability = avail, offline_evidence = offline_evidence, curtailment_response = response,
      availability_global = avail_global, offline_evidence_global = offline_evidence_global,
      curtailment_response_global = response_global,
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


## 5. Curtailments da turbina por unidade IDF (importancia relativa de cada
## unidade a disparar curtailments) ----
##
## Cada curtailment e' atribuido a UMA unidade -- a que registava o track que
## o disparou (attribute_curtailment_unit(), R/offline_curtailment_check.R;
## os curtailments nao trazem a unidade). units: nomes completos das
## unidades de interesse, NA ordem em que devem aparecer (ex: por cobertura
## decrescente). Unidades fora de `units` agrupam-se em "Other IDF units";
## curtailments sem track em track_dt ficam "Unattributed". pct = % de TODOS
## os curtailments da turbina nesse periodo.
##
## idf_display_map: R/idf_labels.R (rotulo interno "IDF<NN>" -> nome completo).
summarise_curtailments_by_unit <- function(curtl_dt, track_dt, turbine_id, period_from, period_to, units, idf_display_map) {

  cu <- data.table::as.data.table(curtl_dt)[turbine == turbine_id & start >= period_from & start <= period_to]
  n_total <- nrow(cu)

  if (n_total > 0L) {
    cu <- attribute_curtailment_unit(cu, track_dt)
    internal <- ifelse(is.na(cu$curtl_unit), NA_character_, sprintf("IDF%02d", cu$curtl_unit))
    full <- ifelse(is.na(internal), NA_character_, relabel_idf(internal, idf_display_map))
    group <- data.table::fcase(
      is.na(full), "Unattributed",
      full %in% units, full,
      default = "Other IDF units"
    )
    counts <- table(factor(group, levels = unique(c(units, "Other IDF units", "Unattributed"))))
  } else {
    counts <- table(factor(character(), levels = unique(c(units, "Other IDF units", "Unattributed"))))
  }

  out <- data.table::data.table(unit = names(counts), n = as.integer(counts))
  out[, pct := if (n_total > 0L) round(100 * n / n_total, 1) else NA_real_]
  # "Other"/"Unattributed" so' aparecem quando tem curtailments
  out <- out[unit %in% units | n > 0L]
  out[]
}
