##
## Atividade recente de aves por turbina (proximidade), farm-wide
##
## Para cada turbina, conta quantos tracks de uma especie (ou lista de
## especies) estiveram dentro de um limiar de proximidade num periodo
## recente. Generaliza, para TODAS as turbinas de uma vez, o mesmo calculo
## de distancia UTM-turbina ja usado em investigate_fatality_tracks() (ver
## R/fatality_track_investigation.R) -- ali e' por incidente de fatalidade
## (1 turbina, janela fixa em torno de uma data), aqui e' farm-wide e para
## qualquer janela recente.
##
## Pensado para alimentar a matriz de decisao do protocolo de resposta a
## outages do IdentiFlight (contexto de "atividade recente de aves
## prioritarias" por turbina, ao decidir a urgencia relativa entre turbinas
## afetadas em simultaneo) -- ver tambem R/turbine_idf_coverage.R
## (cobertura) e R/fatality_window_analysis.R (historico de resposta) para
## os outros dois eixos dessa matriz.
##
## Depende de: data.table, sf
##
## Uso:
##   source("R/turbine_recent_activity.R")
##
##   recent_dt <- summarise_turbine_recent_activity(
##     track_dt, wtg, species = prioritysp,
##     date_from = end - lubridate::days(14), date_to = end,
##     proximity_threshold_m = track_proximity_threshold_m
##   )
##

summarise_turbine_recent_activity <- function(track_dt, wtg_sf, species, date_from, date_to,
                                              proximity_threshold_m = 100, wtg_id_col = "InternalNa") {

  turbines <- wtg_sf[[wtg_id_col]]
  wtg_coords <- sf::st_coordinates(wtg_sf)

  empty <- data.table::data.table(
    turbine = turbines, n_tracks_near = integer(length(turbines)),
    n_points_near = integer(length(turbines)), min_dist_m = NA_real_
  )

  pts <- track_dt[
    spec %in% species & timestamp >= date_from & timestamp <= date_to,
    .(track_id, spec, timestamp, utm_x, utm_y)
  ]
  if (nrow(pts) == 0L) return(empty)

  res <- lapply(seq_along(turbines), function(i) {
    wx <- wtg_coords[i, "X"]
    wy <- wtg_coords[i, "Y"]
    d <- sqrt((pts$utm_x - wx)^2 + (pts$utm_y - wy)^2)
    near <- pts[d <= proximity_threshold_m]

    data.table::data.table(
      turbine       = turbines[i],
      n_tracks_near = data.table::uniqueN(near$track_id),
      n_points_near = nrow(near),
      min_dist_m    = if (nrow(near) > 0L) round(min(d), 1) else NA_real_
    )
  })

  out <- data.table::rbindlist(res)
  data.table::setorder(out, -n_tracks_near)
  out[]
}
