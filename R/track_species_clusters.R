##
## 10.2 -- Ocorrencia temporal de tracks de uma especie (kestrel por
## omissao, parametrizavel) por turbina e por cluster de turbinas
##
## Trata TRACKS, nao registos individuais (um track = 1 ocorrencia, mesmo
## que tenha dezenas de pontos). Cada track e' atribuido a UMA turbina: a
## mais proxima no ponto de MAIOR APROXIMACAO (distancia minima entre TODOS
## os pontos do track e TODAS as turbinas), recalculada a partir de
## utm_x/utm_y -- NAO usa o NearestTurbine3d do proprio IdentiFlight nem
## track_dt$dist (mesmo raciocinio ja documentado em
## R/fatality_track_investigation.R e R/coverage_3d_topography.R: pode
## atribuir mal pontos de fronteira). Usa RANN::nn2 (KD-tree), mesma tecnica
## ja usada em R/coverage_3d_topography.R -- precisa de ser eficiente porque
## corre sobre TODO o historico de track_dt_unfilt, nao so' a janela do
## relatorio.
##
## O cluster de turbinas usado aqui e' o MESMO objeto (turbine, cluster_id)
## produzido em R/turbine_spatial_clusters.R para a seccao 10.1 (estatistico
## OU manual) -- nao ha uma clusterizacao propria para especies, para os 2
## componentes da seccao 10 falarem da mesma "geografia" e serem
## comparaveis entre si.
##
## Especie e' parametro (species_sel), por omissao "Kestrel" -- pensado
## para reutilizar com outras especies no futuro sem alterar o codigo.
##
## Um track e' contado na semana do seu PRIMEIRO ponto (start_time) -- na
## pratica nao ha tracks noturnos neste projeto, por isso um track quase
## nunca atravessa a fronteira de uma semana.
##
## Depende de: data.table, sf, RANN, lubridate, ggplot2
##
## Uso:
##   source("R/track_species_clusters.R")
##
##   tracks_assigned_dt <- assign_tracks_to_nearest_turbine(track_dt_unfilt, wtg, species_sel = "Kestrel")
##   weekly_dt           <- summarise_track_occurrence_weekly(tracks_assigned_dt)
##   by_turbine_dt        <- summarise_track_occurrence_by_turbine(tracks_assigned_dt)
##   by_cluster_out       <- summarise_track_occurrence_by_cluster(tracks_assigned_dt, cluster_dt)
##   rank_dt              <- summarise_turbine_species_cluster_rank(tracks_assigned_dt, cluster_dt, fatality_incidents$turbine)
##
##   p1 <- plot_species_occurrence_weekly(weekly_dt)
##   p2 <- plot_cluster_species_total(by_cluster_out, highlight_clusters = rank_dt$cluster_id)
##   p3 <- plot_cluster_species_weekly(by_cluster_out, highlight_clusters = rank_dt$cluster_id)
##

## 1. Atribuir cada track a UMA turbina (mais proxima no ponto de maior
##    aproximacao) ----

assign_tracks_to_nearest_turbine <- function(track_dt, wtg_sf, species_sel = "Kestrel", wtg_id_col = "InternalNa") {

  turbines   <- wtg_sf[[wtg_id_col]]
  # st_coordinates() devolve X,Y,Z se a geometria de wtg_sf for 3D (ver nota
  # em R/turbine_spatial_clusters.R) -- RANN::nn2() exige que data/query
  # tenham o mesmo nº de colunas, e pts so' tem utm_x/utm_y (2D), por isso
  # selecionar so' X,Y aqui explicitamente
  wtg_coords <- sf::st_coordinates(wtg_sf)[, c("X", "Y")]

  empty <- data.table::data.table(
    track_id = character(), spec = character(), start_time = as.POSIXct(character()),
    turbine = character(), closest_dist_m = numeric()
  )

  pts <- track_dt[spec %in% species_sel & !is.na(utm_x) & !is.na(utm_y), .(track_id, spec, timestamp, utm_x, utm_y)]
  if (nrow(pts) == 0L) return(empty)

  nn <- RANN::nn2(data = wtg_coords, query = as.matrix(pts[, .(utm_x, utm_y)]), k = 1)
  pts[, `:=`(
    nearest_turbine = turbines[as.integer(nn$nn.idx[, 1])],
    nearest_dist_m  = as.numeric(nn$nn.dists[, 1])
  )]

  ## ponto de maior aproximacao = menor nearest_dist_m dentro do track;
  ## setorder e' estavel, por isso o 1º registo de cada track_id apos
  ## ordenar por nearest_dist_m crescente e' esse ponto
  data.table::setorder(pts, track_id, nearest_dist_m)
  closest <- pts[, .SD[1], by = track_id]
  start_time_dt <- pts[, .(start_time = min(timestamp)), by = track_id]

  out <- merge(
    closest[, .(track_id, spec, turbine = nearest_turbine, closest_dist_m = nearest_dist_m)],
    start_time_dt, by = "track_id"
  )
  data.table::setcolorder(out, c("track_id", "spec", "start_time", "turbine", "closest_dist_m"))
  out[]
}


## 2. Serie semanal farm-wide (todas as turbinas juntas) ----

summarise_track_occurrence_weekly <- function(tracks_assigned_dt, unit = "week") {

  if (nrow(tracks_assigned_dt) == 0L) {
    return(data.table::data.table(period = as.Date(character()), n_tracks = integer()))
  }

  dt <- data.table::copy(tracks_assigned_dt)
  dt[, period := lubridate::floor_date(as.Date(start_time, tz = attr(start_time, "tzone")), unit = unit)]

  out <- dt[, .(n_tracks = .N), by = period]
  data.table::setorder(out, period)
  out[]
}


## 3. Totais por turbina (periodo completo) -- onde e' que a especie mais
##    se movimenta, farm-wide ----

summarise_track_occurrence_by_turbine <- function(tracks_assigned_dt) {

  if (nrow(tracks_assigned_dt) == 0L) {
    return(data.table::data.table(turbine = character(), n_tracks = integer(), pct_of_total = numeric()))
  }

  n_total <- nrow(tracks_assigned_dt)
  out <- tracks_assigned_dt[, .(n_tracks = .N), by = turbine]
  out[, pct_of_total := round(100 * n_tracks / n_total, 1)]
  data.table::setorder(out, -n_tracks)
  out[]
}


## 4. Totais e serie semanal por cluster de turbinas ----

summarise_track_occurrence_by_cluster <- function(tracks_assigned_dt, cluster_dt, unit = "week") {

  empty_out <- list(
    by_cluster = data.table::data.table(cluster_id = character(), n_tracks = integer(),
                                         pct_of_total = numeric(), cluster_rank = integer()),
    weekly     = data.table::data.table(cluster_id = character(), period = as.Date(character()), n_tracks = integer())
  )
  if (nrow(tracks_assigned_dt) == 0L) return(empty_out)

  dt <- data.table::copy(tracks_assigned_dt)
  dt[cluster_dt, on = "turbine", cluster_id := i.cluster_id]
  dt <- dt[!is.na(cluster_id)]
  if (nrow(dt) == 0L) return(empty_out)

  n_total <- nrow(dt)
  by_cluster <- dt[, .(n_tracks = .N), by = cluster_id]
  by_cluster[, pct_of_total := round(100 * n_tracks / n_total, 1)]
  data.table::setorder(by_cluster, -n_tracks)
  by_cluster[, cluster_rank := seq_len(.N)]

  dt[, period := lubridate::floor_date(as.Date(start_time, tz = attr(start_time, "tzone")), unit = unit)]
  weekly <- dt[, .(n_tracks = .N), by = .(cluster_id, period)]
  data.table::setorder(weekly, cluster_id, period)

  list(by_cluster = by_cluster[], weekly = weekly[])
}


## 5. Ranking do cluster de atividade da especie para turbinas de interesse
##    -- em que posicao fica o cluster de cada turbina de fatalidade, no
##    ranking de atividade da especie entre todos os clusters (nao e' o
##    nº de tracks atribuidos a ESSA turbina especificamente, e' a
##    atividade do CLUSTER inteiro em que ela esta -- a pergunta e' "esta
##    turbina fica numa zona de muito movimento desta especie?")
summarise_turbine_species_cluster_rank <- function(tracks_assigned_dt, cluster_dt, turbines_of_interest) {

  by_cluster_out <- summarise_track_occurrence_by_cluster(tracks_assigned_dt, cluster_dt)
  by_cluster <- by_cluster_out$by_cluster

  turbine_cluster <- cluster_dt[turbine %in% turbines_of_interest]

  res <- lapply(turbines_of_interest, function(tb) {
    cl_row <- turbine_cluster[turbine == tb]
    if (nrow(cl_row) == 0L) {
      return(data.table::data.table(
        turbine = tb, cluster_id = NA_character_, track_n_tracks = NA_integer_,
        track_pct_of_total = NA_real_, track_cluster_rank = NA_integer_, track_n_clusters = NA_integer_
      ))
    }
    cl_id <- cl_row$cluster_id[1]
    cl_summary <- by_cluster[cluster_id == cl_id]
    if (nrow(cl_summary) == 0L) {
      # cluster existe mas sem NENHUM track atribuido -- zona sem atividade
      # registada da especie, nao e' um erro
      return(data.table::data.table(
        turbine = tb, cluster_id = cl_id, track_n_tracks = 0L,
        track_pct_of_total = 0, track_cluster_rank = NA_integer_, track_n_clusters = nrow(by_cluster)
      ))
    }
    data.table::data.table(
      turbine = tb, cluster_id = cl_id, track_n_tracks = cl_summary$n_tracks,
      track_pct_of_total = cl_summary$pct_of_total, track_cluster_rank = cl_summary$cluster_rank,
      track_n_clusters = nrow(by_cluster)
    )
  })

  data.table::rbindlist(res)
}


## 6. Plots -- ocorrencia semanal farm-wide, totais por cluster (periodo
##    completo) e evolucao semanal por cluster, com os clusters das
##    turbinas de interesse destacados

plot_species_occurrence_weekly <- function(weekly_dt, species_label = "Kestrel") {

  ggplot2::ggplot(weekly_dt, ggplot2::aes(x = period, y = n_tracks)) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::geom_point(color = "steelblue") +
    # expand = c(0,0) + breaks mensais -- mesma correcao ja aplicada aos
    # plots de cobertura diaria (R/data_coverage.R), aqui adaptada ao
    # historico completo (varios meses) do relatorio anual
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", expand = c(0, 0)) +
    ggplot2::labs(x = "Week", y = "Number of tracks",
                  title = sprintf("Weekly track occurrence -- %s (farm-wide)", species_label)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}


plot_cluster_species_total <- function(by_cluster_out, highlight_clusters = NULL, species_label = "Kestrel") {

  dt <- data.table::copy(by_cluster_out$by_cluster)
  data.table::setorder(dt, -n_tracks)
  dt[, cluster_id := factor(cluster_id, levels = cluster_id)]
  dt[, highlight := cluster_id %in% highlight_clusters]

  ggplot2::ggplot(dt, ggplot2::aes(x = cluster_id, y = n_tracks, fill = highlight)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "firebrick", `FALSE` = "darkgreen"), guide = "none") +
    ggplot2::labs(x = "Cluster", y = "Number of tracks",
                  title = sprintf("Tracks by cluster -- %s (full period)", species_label)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}


plot_cluster_species_weekly <- function(by_cluster_out, highlight_clusters = NULL, species_label = "Kestrel") {

  dt <- data.table::copy(by_cluster_out$weekly)
  dt[, is_highlight := cluster_id %in% highlight_clusters]

  ggplot2::ggplot(dt, ggplot2::aes(x = period, y = n_tracks, color = cluster_id, linewidth = is_highlight, group = cluster_id)) +
    ggplot2::geom_line() +
    ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.3, `FALSE` = 0.5), guide = "none") +
    # expand = c(0,0) + breaks mensais -- mesma correcao ja aplicada aos
    # plots de cobertura diaria (R/data_coverage.R), aqui adaptada ao
    # historico completo (varios meses) do relatorio anual
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", expand = c(0, 0)) +
    ggplot2::labs(x = "Week", y = "Number of tracks", color = "Cluster",
                  title = sprintf("Weekly tracks by cluster -- %s", species_label)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
