##
## Padrao temporal de uso do espaco por classe de terreno (bins de 7 dias)
##
## Pergunta do Paulo (2026-08): alem de ver se a distancia/velocidade dos
## curtailments varia por classe de terreno (R/curtailment_bearing_sectors.R,
## R/turbine_terrain_classification.R), tambem interessa ver se o PROPRIO
## padrao de uso do espaco (TODOS os tracks, nao so' os que dispararam
## curtailment) muda ao longo do tempo consoante a classe de terreno -- ex:
## mais tracks perto de cristas nalguma epoca do ano (migracao?).
##
## Cada track e' associado a' turbina geometricamente mais proxima (via UTM,
## KD-tree RANN::nn2 -- NAO o campo NearestTurbine3d/turbine do proprio
## track_dt, que pode atribuir mal pontos de fronteira -- mesma razao de
## R/fatality_track_investigation.R e R/coverage_3d_topography.R) no seu 1o
## registo -- mesma aproximacao "1o ponto = momento de referencia" ja usada
## em R/curtailment_bearing_sectors.R. Um track conta 1 vez so', no bin de 7
## dias onde caiu esse 1o registo -- NAO conta 1 vez por semana em que
## esteve presente (evitaria duplicar tracks longos, mas nao e' isso que
## foi pedido -- "total de tracks por bin", nao "presenca-semanas").
##
## Bins de 7 dias ancorados ao 1o dia com dados (nao ao calendario) -- ver
## week_start em summarise_tracks_by_week_terrain() -- da' janelas exatas de
## 7 dias desde o inicio do periodo analisado, nao semanas ISO.
##
## n_tracks_per_turbine (so' calculado se turbine_terrain_dt for passado a
## summarise_tracks_by_week_terrain()) -- as classes de terreno tipicamente
## NAO tem o mesmo numero de turbinas (ex: caso real do Bash, 2026-08: 1
## turbina "ridge" para ~79 "flat") -- comparar SO' n_tracks absoluto
## sobrestima sempre a classe com mais turbinas, mesmo que a atividade POR
## TURBINA seja identica. n_tracks_per_turbine divide pelo numero de
## turbinas da classe, dando uma base de comparacao mais justa.
##
## Depende de: data.table, sf, RANN, ggplot2
##
## Uso:
##   source("R/track_terrain_temporal.R")
##   track_terrain_dt <- assign_track_terrain_class(track_dt, wtg, terrain_dt)
##   weekly_dt <- summarise_tracks_by_week_terrain(track_terrain_dt, terrain_dt)
##   plot_tracks_by_week_terrain(weekly_dt) # n_tracks (omissao)
##   plot_tracks_by_week_terrain(weekly_dt, metric = "n_tracks_per_turbine") # ajustado ao nº de turbinas
##   plot_tracks_by_week_terrain(weekly_dt, facet = TRUE) # 1 painel por classe, escala Y livre
##


## 1. Turbina mais proxima (geometrica) + classe de terreno, por track ----

assign_track_terrain_class <- function(track_dt, wtg_sf, turbine_terrain_dt, wtg_id_col = "InternalNa") {

  wtg_coords <- sf::st_coordinates(wtg_sf)
  wtg_ids    <- wtg_sf[[wtg_id_col]]

  ## 1o registo de cada track (ordenado por tempo) -- mesma aproximacao "1o
  ## ponto = momento de referencia" de compute_curtailment_bearing()
  pts <- track_dt[, .(track_id, timestamp, utm_x, utm_y)]
  data.table::setorder(pts, track_id, timestamp)
  first_pt_dt <- pts[, .(
    first_timestamp = data.table::first(timestamp),
    utm_x = data.table::first(utm_x), utm_y = data.table::first(utm_y)
  ), by = track_id]

  ## turbina geometricamente mais proxima (KD-tree) -- NAO usa o campo
  ## turbine/NearestTurbine3d do proprio track_dt
  nn <- RANN::nn2(
    data  = as.matrix(data.table::data.table(x = wtg_coords[, "X"], y = wtg_coords[, "Y"])),
    query = as.matrix(first_pt_dt[, .(utm_x, utm_y)]),
    k = 1
  )
  first_pt_dt[, nearest_turbine        := wtg_ids[as.integer(nn$nn.idx[, 1])]]
  first_pt_dt[, nearest_turbine_dist_m := round(as.numeric(nn$nn.dists[, 1]), 1)]

  out <- merge(
    first_pt_dt, turbine_terrain_dt[, .(nearest_turbine = wtg_id, terrain_class)],
    by = "nearest_turbine", all.x = TRUE
  )

  out[, .(track_id, first_timestamp, nearest_turbine, nearest_turbine_dist_m, terrain_class)]
}


## 2. Contagem de tracks por bin de 7 dias x classe de terreno ----

summarise_tracks_by_week_terrain <- function(track_terrain_dt, turbine_terrain_dt = NULL) {

  dt <- track_terrain_dt[!is.na(terrain_class)]
  if (nrow(dt) == 0L) {
    message("summarise_tracks_by_week_terrain(): sem tracks com terrain_class conhecida -- tabela vazia devolvida.")
    return(dt[, .(week_start = as.Date(character()), terrain_class = character(), n_tracks = integer())])
  }

  min_date <- min(as.Date(dt$first_timestamp))
  dt[, week_start := min_date + (as.integer(as.Date(first_timestamp) - min_date) %/% 7L) * 7L]

  counts <- dt[, .(n_tracks = .N), by = .(week_start, terrain_class)]
  counts[, terrain_class := as.character(terrain_class)] # evita conflito factor/character no merge abaixo

  ## completa todas as combinacoes semana x classe -- sem isto, uma
  ## semana/classe sem NENHUM track fica ausente da tabela (0 implicito), o
  ## que faz o grafico de linhas "saltar" essa semana em vez de mostrar 0
  all_weeks   <- seq(min(counts$week_start), max(counts$week_start), by = 7)
  all_classes <- levels(dt$terrain_class)
  if (is.null(all_classes)) all_classes <- sort(unique(as.character(dt$terrain_class)))
  full_grid <- data.table::CJ(week_start = all_weeks, terrain_class = all_classes)

  out <- merge(full_grid, counts, by = c("week_start", "terrain_class"), all.x = TRUE)
  out[is.na(n_tracks), n_tracks := 0L]

  ## n_tracks_per_turbine -- so' se turbine_terrain_dt for dado (ver nota no
  ## topo do ficheiro sobre porque n_tracks sozinho pode enganar quando as
  ## classes tem numeros de turbinas muito diferentes)
  if (!is.null(turbine_terrain_dt)) {
    n_turbines_by_class <- turbine_terrain_dt[, .(n_turbines = .N), by = terrain_class]
    n_turbines_by_class[, terrain_class := as.character(terrain_class)] # evita conflito factor/character no merge
    out <- merge(out, n_turbines_by_class, by = "terrain_class", all.x = TRUE)
    out[, n_tracks_per_turbine := round(n_tracks / n_turbines, 2)]
  }

  out[, terrain_class := factor(terrain_class, levels = c("flat", "complex", "ridge"))]
  data.table::setorder(out, week_start, terrain_class)
  out[]
}


## 3. Grafico de linhas -- 1 linha por classe de terreno, ao longo do tempo --

plot_tracks_by_week_terrain <- function(weekly_dt, metric = c("n_tracks", "n_tracks_per_turbine"), facet = FALSE) {

  metric <- match.arg(metric)

  if (metric == "n_tracks_per_turbine" && !"n_tracks_per_turbine" %in% names(weekly_dt)) {
    message(paste(
      "plot_tracks_by_week_terrain(): weekly_dt nao tem 'n_tracks_per_turbine' --",
      "corre summarise_tracks_by_week_terrain(..., turbine_terrain_dt = ...) para a obter. NULL devolvido."
    ))
    return(NULL)
  }

  if (nrow(weekly_dt) == 0L) {
    message("plot_tracks_by_week_terrain(): sem dados -- NULL devolvido.")
    return(NULL)
  }

  y_lab <- if (metric == "n_tracks") "Number of tracks" else "Tracks per turbine"

  p <- ggplot(weekly_dt, aes(x = week_start, y = .data[[metric]], colour = terrain_class)) +
    geom_line() +
    geom_point(size = 1) +
    scale_colour_manual(values = c(flat = "#4daf4a", complex = "#ff7f00", ridge = "#e41a1c")) +
    labs(
      x = "Week starting", y = y_lab, colour = "Terrain class",
      title = sprintf("Weekly %s by terrain class", tolower(y_lab))
    ) +
    theme_bw()

  # facet = TRUE -- separa em 3 paineis com escala Y livre -- util quando o
  # numero de turbinas por classe e' muito desigual (ex: 1 "ridge" vs ~79
  # "flat"), que faz essa linha ficar quase invisivel numa escala Y partilhada
  if (facet) p <- p + facet_wrap(~terrain_class, ncol = 1, scales = "free_y")

  p
}
