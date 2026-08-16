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
## Depende de: data.table, sf
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
      within_threshold = logical(), last_within_threshold = logical(),
      triggered_curtailment = logical(), signal = character()
    )
  }

  pts <- track_dt[
    spec == species & timestamp >= date_from & timestamp <= date_to,
    .(track_id, timestamp, utm_x, utm_y)
  ]

  if (nrow(pts) == 0L) return(empty_result())

  pts[, dist_m := sqrt((utm_x - wtg_x)^2 + (utm_y - wtg_y)^2)]
  data.table::setorder(pts, track_id, timestamp)

  out <- pts[, .(
    n_points     = .N,
    first_time   = data.table::first(timestamp),
    last_time    = data.table::last(timestamp),
    first_dist_m = data.table::first(dist_m),
    last_dist_m  = data.table::last(dist_m),
    min_dist_m   = min(dist_m)
  ), by = track_id]

  out[, within_threshold := min_dist_m <= proximity_threshold_m]
  out[, last_within_threshold := last_dist_m <= proximity_threshold_m]

  curtailed_ids <- curtl_dt[turbine == turbine_id, unique(track_id)]
  out[, triggered_curtailment := track_id %in% curtailed_ids]

  ## sinal -- indicador operacional, do mais para o menos preocupante:
  ##   no_curtailment_lost_near_turbine -- nenhum curtailment disparado E o
  ##     track termina perto da turbina -- candidato mais forte a colisao
  ##   curtailment_lost_near_turbine -- houve curtailment, mas o track
  ##     ainda assim termina perto da turbina -- resposta pode nao ter chegado a tempo
  ##   near_turbine_not_last -- chegou perto mas o track continua/afasta-se depois
  ##   far_from_turbine -- nunca esteve dentro do limiar de proximidade
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
                                           wtg_id_col = "InternalNa", tz = NULL) {

  res <- lapply(seq_len(nrow(fatality_incidents)), function(i) {
    inc <- fatality_incidents[i]
    dt <- investigate_fatality_tracks(
      turbine_id = inc$turbine, species = inc$species,
      incident_date = inc$incident_date, days_before = inc$days_before,
      track_dt = track_dt, curtl_dt = curtl_dt, wtg_sf = wtg_sf,
      proximity_threshold_m = proximity_threshold_m, wtg_id_col = wtg_id_col, tz = tz
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
