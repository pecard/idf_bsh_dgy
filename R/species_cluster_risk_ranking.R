##
## Ranking de clusters por atividade/exposicao ao risco, para as especies
## dos incidentes de fatalidade conhecidos (fatality_incidents$species)
##
## Complementa a secção 7 (Fatality Investigation & Risk Clusters): em vez
## de olhar so' para a janela de dias a volta de cada incidente (secção 4)
## ou so' para curtailments (10.1/10.3), aqui olhamos para a atividade DE
## VOO da propria especie, no historico completo do projeto, por cluster --
## 2 metricas independentes:
##   1) n_tracks -- quantos tracks distintos dessa especie tem a turbina
##      mais proxima dentro de cada cluster (atividade geral)
##   2) pct_in_risk_zone -- de entre os pontos de voo fiaveis dessa especie
##      (mesmo filtro de flight_metrics_base(), R/bio_flight_metrics.R),
##      que % caem na faixa de risco (rotor-swept, riskHeight_lower/upper)
## Cada cluster e' depois ordenado (rank) em cada metrica, e o cluster de
## CADA turbina de incidente e' localizado nesse ranking -- da' para dizer
## se esse cluster tem atividade/exposicao alta, media ou baixa
## COMPARATIVAMENTE aos outros clusters da mesma especie, nao um valor
## absoluto de risco (nao ha um limiar validado para isso).
##
## Depende de: data.table, R/track_species_clusters.R (assign_tracks_to_nearest_turbine),
## R/bio_flight_metrics.R (flight_metrics_base)
##


## 1. Atribui cada track das especies de interesse ao cluster da turbina
##    mais proxima (mesma logica de assign_tracks_to_nearest_turbine(),
##    so' que aqui guardamos TODAS as especies pedidas, nao so' 1) ----

assign_species_to_clusters <- function(track_dt, wtg_sf, cluster_dt, species_sel, wtg_id_col = "InternalNa") {

  tracks_assigned <- assign_tracks_to_nearest_turbine(track_dt, wtg_sf, species_sel = species_sel, wtg_id_col = wtg_id_col)
  if (nrow(tracks_assigned) == 0L) {
    return(data.table::data.table(
      track_id = character(), spec = character(), start_time = as.POSIXct(character()),
      turbine = character(), closest_dist_m = numeric(), cluster_id = character()
    ))
  }

  out <- merge(tracks_assigned, cluster_dt[, .(turbine, cluster_id)], by = "turbine", all.x = TRUE)
  out[]
}


## 2. Atividade (contagem de tracks distintos) por especie e cluster ----
##    rank 1 = cluster com MAIS tracks dessa especie (mais atividade)

summarise_species_cluster_activity <- function(tracks_assigned_dt) {

  empty <- data.table::data.table(
    spec = character(), cluster_id = character(), n_tracks = integer(),
    pct_of_total = numeric(), activity_rank = integer(), n_clusters = integer()
  )
  dt <- tracks_assigned_dt[!is.na(cluster_id)]
  if (nrow(dt) == 0L) return(empty)

  out <- dt[, .(n_tracks = data.table::uniqueN(track_id)), by = .(spec, cluster_id)]
  out[, pct_of_total := round(100 * n_tracks / sum(n_tracks), 1), by = spec]
  data.table::setorder(out, spec, -n_tracks)
  out[, activity_rank := seq_len(.N), by = spec]
  out[, n_clusters := .N, by = spec]
  out[]
}


## 3. % de pontos de voo fiaveis na faixa de risco, por especie e cluster ----
##    Reaproveita flight_metrics_base() (mesmo filtro de min_track_points e
##    speed_ms_min/max ja usado no relatorio mensal, secção 10) -- so' que
##    aqui cada PONTO fica associado ao cluster da turbina mais proxima do
##    seu track (via tracks_assigned_dt, secção 1 acima), nao ao cluster
##    calculado a partir so' do ponto de maior aproximacao.
##    rank 1 = cluster com MAIOR % de tempo de voo na faixa de risco

summarise_species_cluster_risk_height <- function(flight_base_dt, tracks_assigned_dt, risk_height_lower, risk_height_upper) {

  empty <- data.table::data.table(
    spec = character(), cluster_id = character(), n_points = integer(),
    pct_in_risk_zone = numeric(), risk_rank = integer(), n_clusters = integer()
  )
  if (nrow(flight_base_dt) == 0L) return(empty)

  dt <- merge(
    flight_base_dt[, .(track_id, spec, height)],
    tracks_assigned_dt[!is.na(cluster_id), .(track_id, cluster_id)],
    by = "track_id"
  )
  if (nrow(dt) == 0L) return(empty)

  out <- dt[, .(
    n_points         = .N,
    pct_in_risk_zone = round(100 * mean(height >= risk_height_lower & height <= risk_height_upper), 1)
  ), by = .(spec, cluster_id)]
  data.table::setorder(out, spec, -pct_in_risk_zone)
  out[, risk_rank := seq_len(.N), by = spec]
  out[, n_clusters := .N, by = spec]
  out[]
}


## 4. Sintese por incidente -- localiza o cluster de CADA turbina de
##    incidente nos 2 rankings acima, e classifica-o como "below_median"
##    (rank > metade dos clusters, nessa metrica) ou nao. below_median em
##    AMBAS as metricas e' o unico caso em que os dados desta analise
##    (atividade/exposicao de base, historico completo) nao contradizem uma
##    classificacao de risco relativamente baixo para esse cluster --
##    NAO prova que o risco seja baixo (nao ha causalidade nem limiar
##    absoluto aqui, ver CLAUDE.md), so' descreve onde esse cluster fica
##    face aos outros ----

summarise_incident_cluster_risk_context <- function(fatality_incidents, cluster_dt, activity_dt, risk_height_dt) {

  incidents <- cluster_dt[fatality_incidents, on = "turbine", .(
    incident_id = i.incident_id, turbine = i.turbine, species = i.species, cluster_id
  )]

  out <- data.table::rbindlist(lapply(seq_len(nrow(incidents)), function(i) {
    row <- incidents[i]
    act <- activity_dt[spec == row$species & cluster_id == row$cluster_id]
    rsk <- risk_height_dt[spec == row$species & cluster_id == row$cluster_id]

    data.table::data.table(
      incident_id       = row$incident_id,
      turbine           = row$turbine,
      species           = row$species,
      cluster_id        = row$cluster_id,
      n_clusters        = if (nrow(act) > 0) act$n_clusters else NA_integer_,
      track_activity_rank = if (nrow(act) > 0) act$activity_rank else NA_integer_,
      track_pct_of_total  = if (nrow(act) > 0) act$pct_of_total else NA_real_,
      risk_height_rank    = if (nrow(rsk) > 0) rsk$risk_rank else NA_integer_,
      pct_in_risk_zone    = if (nrow(rsk) > 0) rsk$pct_in_risk_zone else NA_real_
    )
  }))

  out[, below_median_activity   := !is.na(track_activity_rank) & !is.na(n_clusters) & track_activity_rank > n_clusters / 2]
  out[, below_median_risk_height := !is.na(risk_height_rank) & !is.na(n_clusters) & risk_height_rank > n_clusters / 2]
  out[, low_baseline_risk_supported := below_median_activity & below_median_risk_height]
  out[]
}
