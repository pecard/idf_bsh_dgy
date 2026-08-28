##
## Investigacao de tracks perto da turbina, por incidente de fatalidade
##
## Trata cada track_id como uma rota (o movimento registado pela unidade IDF).
## Para os tracks da especie envolvida numa fatalidade, numa janela de dias
## antes (e incluindo) a data de registo do incidente, calcula:
##   - se o track despoletou um curtailment nessa turbina (presenca em curtl_dt)
##   - a distancia minima e a distancia no ULTIMO ponto registado, a turbina
##     especifica (distancia horizontal/2D, via UTM -- NAO usa o
##     NearestTurbine3d do proprio IdentiFlight, que pode atribuir mal pontos
##     de fronteira -- mesmo raciocinio ja usado em coverage_3d_topography.R)
##   - se alguma vez esteve dentro do limiar de proximidade (por omissao
##     100m, track_proximity_threshold_m em userSettings_BSH.R)
##   - se a ULTIMA posicao registada esta dentro desse limiar -- um track que
##     se aproxima e "desaparece" perto da turbina (deixa de ser seguido) e'
##     um indicador de possivel colisao mais forte do que um registo de
##     curtailment (ver CLAUDE.md)
##
## Nota (CLAUDE.md): a data do incidente e' a data em que a ave foi
## encontrada numa prospecao, NAO a data da morte -- por isso a janela
## analisada inclui varios dias antes (days_before, tipicamente 8, mas pode
## ser maior consoante o caso) E o proprio dia do registo, como um unico
## periodo continuo (nao se trata o dia do registo à parte).
##
## Depende de: data.table, sf, ggplot2 (so' plot_fatality_track_rpm())
##
## Uso:
##   source("R/fatality_track_investigation.R")
##
##   tracks_i <- investigate_fatality_tracks(
##     turbine_id = "BSH54", species = "Steppe-Eagle",
##     incident_date = as.Date("2025-10-31"), days_before = 8,
##     track_dt = track_dt, curtl_dt = curtl_dt, wtg_sf = wtg,
##     proximity_threshold_m = track_proximity_threshold_m
##   )
##
##   # varios incidentes de uma vez -- ver fatality_incidents em userSettings_BSH.R
##   all_tracks_i <- investigate_fatality_incidents(
##     fatality_incidents, track_dt, curtl_dt, wtg,
##     proximity_threshold_m = track_proximity_threshold_m
##   )
##
##   # sumario -- contagens por sinal e candidatos mais provaveis a colisao
##   summary_i <- summarise_fatality_tracks(all_tracks_i, top_n = 10)
##   summary_i$counts_by_signal
##   summary_i$top_candidates
##


## 1. Um incidente -- turbina + especie + janela de dias ----

investigate_fatality_tracks <- function(turbine_id, species, incident_date, days_before,
                                        track_dt, curtl_dt, wtg_sf,
                                        proximity_threshold_m = 100,
                                        height_threshold_m = NULL,
                                        wtg_id_col = "InternalNa", tz = NULL) {

  incident_date <- as.Date(incident_date)
  if (is.null(tz)) tz <- attr(track_dt$timestamp, "tzone")

  date_from <- as.POSIXct(paste(incident_date - days_before, "00:00:00"), tz = tz)
  date_to   <- as.POSIXct(paste(incident_date, "23:59:59"), tz = tz)

  wtg_row <- wtg_sf[wtg_sf[[wtg_id_col]] == turbine_id, ]
  if (nrow(wtg_row) == 0L) {
    stop(sprintf("Turbina '%s' nao encontrada no shapefile (coluna '%s')", turbine_id, wtg_id_col))
  }
  wtg_coords <- sf::st_coordinates(wtg_row)
  wtg_x <- wtg_coords[1, "X"]
  wtg_y <- wtg_coords[1, "Y"]

  empty_result <- function() {
    data.table::data.table(
      track_id = character(), n_points = integer(),
      first_time = as.POSIXct(character()), last_time = as.POSIXct(character()),
      first_dist_m = numeric(), last_dist_m = numeric(), min_dist_m = numeric(),
      last_height_m = numeric(), min_height_m = numeric(),
      within_threshold = logical(), last_within_threshold = logical(),
      triggered_curtailment = logical(), signal = character()
    )
  }

  pts <- track_dt[
    spec == species & timestamp >= date_from & timestamp <= date_to,
    .(track_id, timestamp, utm_x, utm_y, height)
  ]

  if (nrow(pts) == 0L) return(empty_result())

  pts[, dist_m := sqrt((utm_x - wtg_x)^2 + (utm_y - wtg_y)^2)]
  data.table::setorder(pts, track_id, timestamp)

  ## in_risk_zone -- ponto dentro do limiar de proximidade horizontal E (se
  ## height_threshold_m for dado) a uma altura AGL onde um curtailment seria
  ## mesmo disparado -- pedido do Paulo (2026-08, Zarafshan): o sistema so
  ## despoleta curtailment para aves a voar abaixo de height_threshold_m
  ## (ex: 400m AGL), por isso "perto da turbina" para efeitos de
  ## identificacao de candidatos a colisao deve exigir tambem essa altura,
  ## nao so' a distancia horizontal -- um ponto horizontalmente perto mas
  ## muito acima dessa cota nunca estaria em risco real de colisao nem
  ## despoletaria resposta alguma. height_threshold_m = NULL (omissao)
  ## mantem o comportamento antigo (so' distancia horizontal) -- usado por
  ## BSH/DGY, onde esta regra nao foi (ainda) confirmada.
  if (!is.null(height_threshold_m)) {
    pts[, in_risk_zone := dist_m <= proximity_threshold_m &
          !is.na(height) & height <= height_threshold_m]
  } else {
    pts[, in_risk_zone := dist_m <= proximity_threshold_m]
  }

  out <- pts[, .(
    n_points       = .N,
    first_time     = data.table::first(timestamp),
    last_time      = data.table::last(timestamp),
    first_dist_m   = data.table::first(dist_m),
    last_dist_m    = data.table::last(dist_m),
    min_dist_m     = min(dist_m),
    last_height_m  = data.table::last(height),
    min_height_m   = suppressWarnings(min(height, na.rm = TRUE)),
    within_threshold      = any(in_risk_zone),
    last_within_threshold = data.table::last(in_risk_zone)
  ), by = track_id]
  out[is.infinite(min_height_m), min_height_m := NA_real_]

  curtailed_ids <- curtl_dt[turbine == turbine_id, unique(track_id)]
  out[, triggered_curtailment := track_id %in% curtailed_ids]

  ## sinal -- indicador operacional, do mais para o menos preocupante:
  ##   no_curtailment_lost_near_turbine -- nenhum curtailment disparado E o
  ##     track termina perto da turbina -- candidato mais forte a colisao
  ##   curtailment_lost_near_turbine -- houve curtailment, mas o track
  ##     ainda assim termina perto da turbina -- resposta pode nao ter chegado a tempo
  ##   near_turbine_not_last -- chegou perto mas o track continua/afasta-se depois
  ##   far_from_turbine -- nunca esteve dentro do limiar de proximidade
  ##     (e, quando height_threshold_m e' dado, da cota que despoletaria
  ##     curtailment)
  out[, signal := data.table::fcase(
    last_within_threshold & !triggered_curtailment, "no_curtailment_lost_near_turbine",
    last_within_threshold & triggered_curtailment,  "curtailment_lost_near_turbine",
    within_threshold & !last_within_threshold,      "near_turbine_not_last",
    default = "far_from_turbine"
  )]

  data.table::setorder(out, min_dist_m)
  out[]
}


## 2. Varios incidentes de uma vez, a partir da tabela fatality_incidents ----
##    (colunas esperadas: incident_id, turbine, species, incident_date, days_before)

investigate_fatality_incidents <- function(fatality_incidents, track_dt, curtl_dt, wtg_sf,
                                           proximity_threshold_m = 100,
                                           height_threshold_m = NULL,
                                           wtg_id_col = "InternalNa", tz = NULL) {

  res <- lapply(seq_len(nrow(fatality_incidents)), function(i) {
    inc <- fatality_incidents[i]
    dt <- investigate_fatality_tracks(
      turbine_id = inc$turbine, species = inc$species,
      incident_date = inc$incident_date, days_before = inc$days_before,
      track_dt = track_dt, curtl_dt = curtl_dt, wtg_sf = wtg_sf,
      proximity_threshold_m = proximity_threshold_m, height_threshold_m = height_threshold_m,
      wtg_id_col = wtg_id_col, tz = tz
    )
    dt[, `:=`(incident_id = inc$incident_id, turbine = inc$turbine, species = inc$species)]
    dt
  })

  data.table::rbindlist(res)
}


## 3. Sumario -- contagens por sinal e candidatos mais provaveis a colisao ----
##
## "Candidatos" = tracks cujo signal e' no_curtailment_lost_near_turbine ou
## curtailment_lost_near_turbine, i.e. a ULTIMA posicao registada do track
## esta dentro do limiar de proximidade -- o IdentiFlight deixou de seguir a
## ave (sem mais pontos depois desse) enquanto ela ainda estava perto da
## turbina, o indicador mais forte de possivel colisao (ver CLAUDE.md).
## Ordenados por prioridade de sinal (sem curtailment primeiro, por ser o
## cenario mais critico -- nenhuma resposta e a ave desaparece perto da
## turbina) e depois por last_dist_m (mais perto da turbina primeiro).

fatality_signal_priority <- c(
  no_curtailment_lost_near_turbine = 1L,  # mais critico: sem resposta, track termina perto da turbina
  curtailment_lost_near_turbine    = 2L,  # houve curtailment, mas o track ainda assim termina perto
  near_turbine_not_last            = 3L,  # chegou perto, mas o track continua/afasta-se depois
  far_from_turbine                 = 4L   # nunca esteve dentro do limiar de proximidade
)

## Rotulos em linguagem natural para o "signal" -- usados so' na
## APRESENTACAO do relatorio (tabelas do Rmd); os valores internos com "_"
## continuam a ser os usados no codigo e no xlsx de anexo.
fatality_signal_labels <- c(
  no_curtailment_lost_near_turbine = "No curtailment - lost near turbine",
  curtailment_lost_near_turbine    = "Curtailment triggered - lost near turbine",
  near_turbine_not_last            = "Near turbine, not last position",
  far_from_turbine                 = "Far from turbine"
)

summarise_fatality_tracks <- function(fatality_tracks_dt, top_n = 10) {

  empty_counts <- data.table::data.table(
    incident_id = character(), turbine = character(), species = character(),
    signal = character(), n_tracks = integer(), pct_of_incident = numeric()
  )

  if (nrow(fatality_tracks_dt) == 0L) {
    return(list(counts_by_signal = empty_counts, top_candidates = fatality_tracks_dt))
  }

  counts_by_signal <- fatality_tracks_dt[, .(n_tracks = .N), by = .(incident_id, turbine, species, signal)]
  counts_by_signal[, pct_of_incident := round(100 * n_tracks / sum(n_tracks), 1), by = incident_id]
  counts_by_signal[, signal_ord := fatality_signal_priority[signal]]
  data.table::setorder(counts_by_signal, incident_id, signal_ord)
  counts_by_signal[, signal_ord := NULL]

  candidate_signals <- names(fatality_signal_priority)[1:2]
  candidates <- fatality_tracks_dt[signal %in% candidate_signals]
  candidates[, signal_ord := fatality_signal_priority[signal]]
  data.table::setorder(candidates, incident_id, signal_ord, last_dist_m)
  candidates[, signal_ord := NULL]

  top_candidates <- candidates[, .SD[seq_len(min(.N, top_n))], by = incident_id]

  list(counts_by_signal = counts_by_signal[], top_candidates = top_candidates[])
}


## 4. Perfil de RPM a volta da janela do track candidato ao incidente --
##    mesma linguagem visual de plot_curtailment_events_rpm()
##    (R/curtailment_forensic_trace.R), mas ancorado no last_time do track
##    (nao no start/end de um curtailment escolhido a priori), com
##    marcadores para first_time ("Track first position") e last_time
##    ("Track last position") do proprio track, mais o(s) curtailment(s)
##    REAL(is) desse track_id/turbina sobrepostos quando existirem --
##    pedido do Paulo, 2026-08 (relatorio de incidente, secção "Top
##    Candidate Tracks"): um exemplo do 1º track SEM curtailment e um do 1º
##    track COM curtailment, com a posicao inicial do track tambem
##    marcada "para completude". Quando o track nao despoletou nenhum
##    curtailment, so as 2 linhas do track (first/last position) sao
##    desenhadas -- nao ha inicio/fim de curtailment para marcar.
##
## track_row: 1 linha de fatality_tracks_dt/top_candidates (precisa de
## track_id, turbine, last_time). Devolve NULL se nao houver leituras de
## SCADA (RPM) dessa turbina na janela a volta de last_time.

plot_fatality_track_rpm <- function(track_row, scada_dt, curtl_dt,
                                    window_before_min = 3, window_after_min = 3, title = NULL) {

  t_ref <- track_row$last_time
  t_ini <- t_ref - window_before_min * 60
  t_end <- t_ref + window_after_min * 60

  rpm_dt <- scada_dt[
    turbinelabel == track_row$turbine & readingname == "RPM" & datetime >= t_ini & datetime <= t_end,
    .(datetime, value)
  ]
  if (nrow(rpm_dt) == 0L) return(NULL)

  events_curtl <- curtl_dt[track_id == track_row$track_id & turbine == track_row$turbine]

  events_long <- data.table::rbindlist(list(
    data.table::data.table(event_time = track_row$first_time, event_type = "Track first position"),
    data.table::data.table(event_time = t_ref, event_type = "Track last position"),
    if (nrow(events_curtl) > 0L) events_curtl[, .(event_time = start, event_type = "Curtailment start")] else NULL,
    if (nrow(events_curtl) > 0L) events_curtl[, .(event_time = end, event_type = "Curtailment stop")] else NULL
  ))

  y_max <- max(rpm_dt$value, na.rm = TRUE) + 1

  ggplot2::ggplot() +
    ggplot2::geom_line(data = rpm_dt, ggplot2::aes(x = datetime, y = value, colour = "RPM", linetype = "RPM")) +
    ggplot2::geom_point(data = rpm_dt, ggplot2::aes(x = datetime, y = value, colour = "RPM"), size = 1.2) +
    ggplot2::geom_vline(
      data = events_long,
      ggplot2::aes(xintercept = event_time, colour = event_type, linetype = event_type),
      linewidth = 0.7
    ) +
    ggplot2::scale_colour_manual(
      values = c("RPM" = "#e8792f", "Track first position" = "darkgreen", "Track last position" = "purple",
                "Curtailment start" = "steelblue", "Curtailment stop" = "darkred"),
      name = NULL
    ) +
    ggplot2::scale_linetype_manual(
      values = c("RPM" = "solid", "Track first position" = "dotted", "Track last position" = "dotted",
                "Curtailment start" = "solid", "Curtailment stop" = "dashed"),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
    ggplot2::scale_x_datetime(date_breaks = "30 sec", date_labels = "%H:%M:%S") +
    ggplot2::labs(x = NULL, y = "RPM", title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
}
