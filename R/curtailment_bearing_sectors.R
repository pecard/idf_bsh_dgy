##
## Setor de aproximacao (bearing, 8 direcoes) de cada curtailment -- influencia
## na distancia a que o curtailment e' disparado e na velocidade de voo da ave
##
## Pergunta do Paulo (2026-08, exploratorio -- nao faz ainda parte do
## relatorio): o setor de onde a ave se aproxima da turbina no momento do
## disparo de um curtailment influencia (a) a distancia a que o curtailment e'
## disparado e (b) a velocidade a que a ave voa? Objetivo futuro: cruzar com a
## cobertura 3D (R/coverage_3d_topography.R) para identificar setores/areas
## mais expostas a aproximacoes rapidas (potencialmente mais arriscadas).
##
## Bearing calculado do ponto de vista da TURBINA que disparou o curtailment
## (de onde vem a ave, nao para onde ela vai) -- vetor (posicao da ave no 1o
## registo do track) menos (posicao da turbina), em coordenadas UTM planares
## (mesma projecao de track_dt$utm_x/utm_y), convertido para bearing de
## bussola (0=Norte, sentido horario) e discretizado em 8 setores de 45 graus
## (N/NE/E/SE/S/SW/W/NW, cada um centrado no seu ponto cardinal -- ex: N cobre
## [337.5, 360) U [0, 22.5)).
##
## NAO usa o TowerNumber/NearestTurbine3d do proprio IdentiFlight (pode
## atribuir mal pontos de fronteira, ver R/coverage_3d_topography.R e
## R/fatality_track_investigation.R) -- usa a posicao real da turbina no
## shapefile wtg, tal como essas duas analises.
##
## So' inclui tracks com >= min_points registos (por omissao 5) -- garante
## uma estimativa de velocidade media minimamente fiavel. avg_speed_ms usa a
## mesma logica de corte de outliers (percentil 95, so' valores > 0) de
## compute_safe_distance() (R/curtailment_safe_distance.R).
##
## Independente de SCADA -- usa so' curtl_dt (log de curtailments) e
## track_dt, ao contrario de compute_safe_distance() (que precisa de SCADA
## para o tempo de resposta) -- corre em qualquer parque/periodo com log de
## curtailments, mesmo sem SCADA fiavel.
##
## Depende de: data.table, sf, ggplot2
##
## Uso:
##   source("R/curtailment_bearing_sectors.R")
##   bearing_dt    <- compute_curtailment_bearing(curtl_dt, track_dt, wtg, min_points = 5)
##   summary_bearing <- summarise_bearing_sectors(bearing_dt)
##   plot_bearing_by_sector(summary_bearing, metric = "mean_trigger_dist_m")
##   plot_bearing_by_sector(summary_bearing, metric = "mean_speed_ms")
##


## 1. Bearing/setor de cada curtailment (por track/turbina) ----

compass_sectors <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")

# bearing_deg: 0-360, 0=Norte, sentido horario -- devolve o setor de 45 graus
# correspondente, centrado no ponto cardinal (ex: N = [337.5,360) U [0,22.5))
bearing_to_sector <- function(bearing_deg) {
  idx <- (floor((bearing_deg + 22.5) / 45) %% 8) + 1
  compass_sectors[idx]
}

compute_curtailment_bearing <- function(curtl_dt, track_dt, wtg_sf,
                                        min_points = 5, speed_trim_q = 0.95,
                                        wtg_id_col = "InternalNa") {

  wtg_coords <- sf::st_coordinates(wtg_sf)
  wtg_pos <- data.table::data.table(
    turbine = wtg_sf[[wtg_id_col]],
    wtg_x   = wtg_coords[, "X"],
    wtg_y   = wtg_coords[, "Y"]
  )

  n_points_dt   <- track_dt[, .(n_points = .N), by = track_id]
  qualifying_ids <- n_points_dt[n_points >= min_points, track_id]

  ## posicao do 1o registo do track -- mesma aproximacao ao momento do
  ## disparo ja usada em compute_safe_distance() (x2d = dist[1])
  pts <- track_dt[track_id %in% qualifying_ids, .(track_id, timestamp, utm_x, utm_y)]
  data.table::setorder(pts, track_id, timestamp)
  first_pt_dt <- pts[, .(
    utm_x = data.table::first(utm_x), utm_y = data.table::first(utm_y)
  ), by = track_id]

  ## velocidade media de voo do track (corte outliers > percentil 95, so
  ## valores > 0) -- mesma logica de compute_safe_distance()
  speed_dt <- track_dt[!is.na(speed_ms), {
    q <- as.numeric(quantile(speed_ms, probs = speed_trim_q, na.rm = TRUE))
    .(avg_speed_ms = mean(speed_ms[speed_ms > 0 & speed_ms < q], na.rm = TRUE))
  }, by = track_id]

  ## unidade de analise = cada EVENTO de curtailment (nao deduplicado por
  ## track) -- mesma convencao de R/curtailment_removal_risk.R
  events <- curtl_dt[, .(turbine, track_id, species, start)]
  events <- merge(events, first_pt_dt, by = "track_id")         # so' tracks com >= min_points
  events <- merge(events, speed_dt, by = "track_id", all.x = TRUE)
  events <- merge(events, wtg_pos, by = "turbine")               # so' turbinas presentes no shapefile

  events[, `:=`(
    trigger_dist_m = sqrt((utm_x - wtg_x)^2 + (utm_y - wtg_y)^2),
    bearing_deg    = (atan2(utm_x - wtg_x, utm_y - wtg_y) * 180 / pi) %% 360
  )]
  events[, sector := factor(bearing_to_sector(bearing_deg), levels = compass_sectors)]

  events[, .(track_id, turbine, species, start, bearing_deg, sector, trigger_dist_m, avg_speed_ms)]
}


## 2. Resumo por setor -- n, distancia media/mediana, velocidade media/mediana ----

summarise_bearing_sectors <- function(bearing_dt) {

  dt <- bearing_dt[!is.na(sector)]

  out <- dt[, .(
    n                      = .N,
    n_speed_known          = sum(!is.na(avg_speed_ms)),
    mean_trigger_dist_m    = round(mean(trigger_dist_m, na.rm = TRUE), 1),
    median_trigger_dist_m  = round(median(trigger_dist_m, na.rm = TRUE), 1),
    mean_speed_ms          = round(mean(avg_speed_ms, na.rm = TRUE), 1),
    median_speed_ms        = round(median(avg_speed_ms, na.rm = TRUE), 1)
  ), by = sector]

  # setores sem nenhum evento ficam de fora do agrupamento acima -- completa
  # a tabela com n=0 para os 8 setores aparecerem sempre, mesmo sem dados
  missing_sectors <- setdiff(compass_sectors, as.character(out$sector))
  if (length(missing_sectors) > 0) {
    out <- data.table::rbindlist(list(
      out,
      data.table::data.table(
        sector = missing_sectors, n = 0L, n_speed_known = 0L,
        mean_trigger_dist_m = NA_real_, median_trigger_dist_m = NA_real_,
        mean_speed_ms = NA_real_, median_speed_ms = NA_real_
      )
    ))
  }
  out[, sector := factor(sector, levels = compass_sectors)]
  data.table::setorder(out, sector)
  out[]
}


## 3. Grafico de barras (ordenado por setor de bussola) de uma metrica ----

plot_bearing_by_sector <- function(summary_dt, metric = c("mean_trigger_dist_m", "mean_speed_ms")) {

  metric <- match.arg(metric)
  dt <- summary_dt[!is.na(get(metric))]

  if (nrow(dt) == 0L) {
    message(sprintf("plot_bearing_by_sector(): sem setores com '%s' calculavel -- NULL devolvido.", metric))
    return(NULL)
  }

  y_lab <- if (metric == "mean_trigger_dist_m") "Mean trigger distance (m)" else "Mean flight speed (m/s)"

  ggplot(dt, aes(x = sector, y = .data[[metric]])) +
    geom_col(fill = "#17aeb0") +
    geom_text(aes(label = sprintf("n=%d", n)), vjust = -0.3, size = 3) +
    scale_x_discrete(limits = compass_sectors, drop = FALSE) +
    labs(x = "Approach sector (from turbine)", y = y_lab, title = sprintf("%s by approach sector", y_lab)) +
    theme_bw()
}
