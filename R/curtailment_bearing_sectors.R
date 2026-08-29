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
## trigger_height_m (altura AGL da ave no 1o registo do track, mesma
## aproximacao ao momento do disparo que trigger_dist_m/avg_speed_ms) e o
## cruzamento opcional com a classe de terreno da turbina
## (R/turbine_terrain_classification.R, pedido do Paulo 2026-08 depois de o
## setor de bussola sozinho nao mostrar padrao claro) foram adicionados a
## seguir a 1a versao deste ficheiro -- ver turbine_terrain_dt abaixo.
##
## Uso:
##   source("R/curtailment_bearing_sectors.R")
##
##   # setor de bussola sozinho
##   bearing_dt      <- compute_curtailment_bearing(curtl_dt, track_dt, wtg, min_points = 5)
##   summary_bearing <- summarise_bearing_sectors(bearing_dt)
##   plot_bearing_boxplot(bearing_dt, metric = "trigger_dist_m") # ou "avg_speed_ms"/"trigger_height_m"
##   plot_bearing_hist(bearing_dt, metric = "trigger_dist_m")
##
##   # + classe de terreno (ver R/turbine_terrain_classification.R) --
##   # terrain_dt tem de ja' ter passado por classify_terrain()
##   bearing_dt2 <- compute_curtailment_bearing(curtl_dt, track_dt, wtg, turbine_terrain_dt = terrain_dt)
##   summarise_bearing_sectors(bearing_dt2, group_cols = "terrain_class") # so' por terreno
##   summarise_bearing_sectors(bearing_dt2, group_cols = c("sector", "terrain_class")) # cruzado
##   plot_bearing_boxplot(bearing_dt2, metric = "trigger_dist_m") # cores automaticas por terrain_class
##   plot_terrain_class_hist(bearing_dt2, metric = "trigger_height_m")
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
                                        wtg_id_col = "InternalNa",
                                        turbine_terrain_dt = NULL) {

  wtg_coords <- sf::st_coordinates(wtg_sf)
  wtg_pos <- data.table::data.table(
    turbine = wtg_sf[[wtg_id_col]],
    wtg_x   = wtg_coords[, "X"],
    wtg_y   = wtg_coords[, "Y"]
  )

  n_points_dt   <- track_dt[, .(n_points = .N), by = track_id]
  qualifying_ids <- n_points_dt[n_points >= min_points, track_id]

  ## posicao/altura no 1o registo do track -- mesma aproximacao ao momento
  ## do disparo ja usada em compute_safe_distance() (x2d = dist[1])
  pts <- track_dt[track_id %in% qualifying_ids, .(track_id, timestamp, utm_x, utm_y, height)]
  data.table::setorder(pts, track_id, timestamp)
  first_pt_dt <- pts[, .(
    utm_x = data.table::first(utm_x), utm_y = data.table::first(utm_y),
    trigger_height_m = data.table::first(height)
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

  out_cols <- c(
    "track_id", "turbine", "species", "start", "bearing_deg", "sector",
    "trigger_dist_m", "trigger_height_m", "avg_speed_ms"
  )

  ## classe de terreno da turbina (ver R/turbine_terrain_classification.R,
  ## classify_terrain()) -- opcional, so' anexada se fornecida; turbinas sem
  ## classe conhecida (ex: fora de turbine_terrain_dt) ficam com NA, nao sao
  ## excluidas
  if (!is.null(turbine_terrain_dt)) {
    events <- merge(
      events, turbine_terrain_dt[, .(turbine = wtg_id, terrain_class)],
      by = "turbine", all.x = TRUE
    )
    out_cols <- c(out_cols, "terrain_class")
  }

  events[, ..out_cols]
}


## 2. Resumo por grupo -- distribuicao (quantis), nao so' a media ----
##
## Medias ficam facilmente parecidas entre grupos mesmo quando a FORMA da
## distribuicao difere (ex: um grupo com cauda longa a direita -- poucas
## aproximacoes muito rapidas/muito perto -- pode ter a mesma media que um
## grupo uniforme) -- pedido do Paulo, 2026-08: olhar para a distribuicao
## completa (quantis), nao so' a media/mediana, para detetar caudas longas
## ou outros padroes que a media escode.
##
## group_cols -- por omissao agrupa so' por "sector" (comportamento
## original, com preenchimento automatico dos 8 setores mesmo sem eventos).
## Pode agrupar por outra coluna presente em bearing_dt (ex: "terrain_class",
## ver R/turbine_terrain_classification.R) ou por varias em conjunto (ex:
## c("sector", "terrain_class")) -- nesses casos o preenchimento automatico
## de grupos vazios NAO e' feito (so' definido para o caso classico de
## "sector" sozinho), grupos sem eventos simplesmente nao aparecem na tabela.
##
## dist_skew_ratio/speed_skew_ratio/height_skew_ratio = (p90 - mediana) /
## (mediana - p10) -- heuristica simples (nao uma medida formal de
## assimetria): >1 sugere cauda mais longa acima da mediana (ex: poucas
## aproximacoes a distancia/velocidade/altura muito acima do tipico), <1
## sugere cauda mais longa abaixo, ~1 sugere distribuicao aproximadamente
## simetrica. NaN quando n<=1 ou sem variabilidade (p90==mediana==p10) --
## nesse caso o racio nao e' informativo, ver antes o grafico
## (plot_bearing_boxplot()/plot_bearing_hist()/plot_terrain_class_hist()).

summarise_bearing_sectors <- function(bearing_dt, group_cols = "sector") {

  dt <- bearing_dt[!is.na(sector)]
  probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)

  ## quantis + max de x, com o prefixo dado -- devolve uma lista pronta a
  ## ser combinada (c()) dentro do agrupamento abaixo; NA_real_ em tudo se x
  ## ficar vazio depois de remover NAs (ex: trigger_height_m desconhecido)
  quantile_block <- function(x, prefix) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) {
      vals <- rep(NA_real_, length(probs) + 1L)
    } else {
      vals <- c(stats::quantile(x, probs = probs, names = FALSE), max(x))
    }
    out <- as.list(round(vals, 1))
    names(out) <- paste0(prefix, c("_p10", "_p25", "_median", "_p75", "_p90", "_max"))
    out
  }

  out <- dt[, c(
    list(
      n              = .N,
      n_speed_known  = sum(!is.na(avg_speed_ms)),
      n_height_known = sum(!is.na(trigger_height_m))
    ),
    quantile_block(trigger_dist_m, "dist"),
    quantile_block(avg_speed_ms, "speed"),
    quantile_block(trigger_height_m, "height")
  ), by = group_cols]

  out[, dist_skew_ratio   := round((dist_p90 - dist_median) / (dist_median - dist_p10), 2)]
  out[, speed_skew_ratio  := round((speed_p90 - speed_median) / (speed_median - speed_p10), 2)]
  out[, height_skew_ratio := round((height_p90 - height_median) / (height_median - height_p10), 2)]

  if (identical(group_cols, "sector")) {
    # setores sem nenhum evento ficam de fora do agrupamento acima --
    # completa a tabela com n=0 para os 8 setores aparecerem sempre, mesmo
    # sem dados
    missing_sectors <- setdiff(compass_sectors, as.character(out$sector))
    if (length(missing_sectors) > 0) {
      filler <- data.table::data.table(sector = missing_sectors, n = 0L, n_speed_known = 0L, n_height_known = 0L)
      na_cols <- setdiff(names(out), c("sector", "n", "n_speed_known", "n_height_known"))
      filler[, (na_cols) := NA_real_]
      data.table::setcolorder(filler, names(out))
      out <- data.table::rbindlist(list(out, filler), use.names = TRUE)
    }
    out[, sector := factor(sector, levels = compass_sectors)]
    data.table::setorder(out, sector)
  }
  out[]
}


## 3. Boxplot + histograma -- forma da distribuicao, nao so' um numero
## resumo por setor/grupo (mesmo motivo da seccao 2 acima) ----

.bearing_metric_label <- function(metric) {
  switch(metric,
    trigger_dist_m   = "Trigger distance (m)",
    avg_speed_ms     = "Flight speed (m/s)",
    trigger_height_m = "Flight height AGL (m)"
  )
}

## Boxplot por setor -- se bearing_dt tiver uma coluna terrain_class (ver
## R/turbine_terrain_classification.R e compute_curtailment_bearing(),
## argumento turbine_terrain_dt), colore/agrupa automaticamente por essa
## classe dentro de cada setor; caso contrario, comportamento original
## (1 boxplot por setor, sem cor).
plot_bearing_boxplot <- function(bearing_dt, metric = c("trigger_dist_m", "avg_speed_ms", "trigger_height_m")) {

  metric <- match.arg(metric)
  dt <- bearing_dt[!is.na(sector) & !is.na(get(metric))]

  if (nrow(dt) == 0L) {
    message(sprintf("plot_bearing_boxplot(): sem eventos com '%s' calculavel -- NULL devolvido.", metric))
    return(NULL)
  }

  y_lab <- .bearing_metric_label(metric)
  has_terrain <- "terrain_class" %in% names(dt) && any(!is.na(dt$terrain_class))

  # geom_jitter por cima do boxplot -- com amostras pequenas por setor (caso
  # tipico aqui), ver os pontos individuais e' mais informativo do que
  # confiar so' nos whiskers/outliers do boxplot
  if (has_terrain) {
    ggplot(dt, aes(x = sector, y = .data[[metric]], fill = terrain_class)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.5, position = ggplot2::position_dodge2(preserve = "single")) +
      geom_point(
        position = ggplot2::position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75),
        alpha = 0.5, size = 1
      ) +
      scale_x_discrete(limits = compass_sectors, drop = FALSE) +
      labs(
        x = "Approach sector (from turbine)", y = y_lab, fill = "Terrain class",
        title = sprintf("Distribution of %s by approach sector and terrain class", tolower(y_lab))
      ) +
      theme_bw()
  } else {
    ggplot(dt, aes(x = sector, y = .data[[metric]])) +
      geom_boxplot(outlier.shape = NA, fill = "#17aeb0", alpha = 0.3) +
      geom_jitter(width = 0.15, height = 0, alpha = 0.6, colour = "#0d6e70") +
      scale_x_discrete(limits = compass_sectors, drop = FALSE) +
      labs(
        x = "Approach sector (from turbine)", y = y_lab,
        title = sprintf("Distribution of %s by approach sector", tolower(y_lab))
      ) +
      theme_bw()
  }
}

plot_bearing_hist <- function(bearing_dt, metric = c("trigger_dist_m", "avg_speed_ms", "trigger_height_m"), bins = 15) {

  metric <- match.arg(metric)
  dt <- bearing_dt[!is.na(sector) & !is.na(get(metric))]

  if (nrow(dt) == 0L) {
    message(sprintf("plot_bearing_hist(): sem eventos com '%s' calculavel -- NULL devolvido.", metric))
    return(NULL)
  }

  x_lab <- .bearing_metric_label(metric)

  # scales="free_y" -- setores com poucos eventos ficam com uma barra
  # ilegivel se partilharem escala com o setor mais frequente (mesma razao
  # de plot_safe_distance_hist(), R/curtailment_safe_distance.R); drop=FALSE
  # mostra os 8 setores mesmo os sem eventos (paineis vazios)
  ggplot(dt, aes(x = .data[[metric]])) +
    geom_histogram(bins = bins, colour = "grey", fill = "#17aeb0") +
    facet_wrap(~sector, ncol = 4, scales = "free_y", drop = FALSE) +
    labs(
      x = x_lab, y = "Count",
      title = sprintf("Distribution of %s by approach sector", tolower(x_lab))
    ) +
    theme_bw()
}

## Histograma faceado so' por terrain_class (3 paineis) -- comparacao direta
## entre classes de terreno, sem cruzar com o setor de bussola. Requer
## bearing_dt vindo de compute_curtailment_bearing(..., turbine_terrain_dt = ...)
plot_terrain_class_hist <- function(bearing_dt, metric = c("trigger_dist_m", "avg_speed_ms", "trigger_height_m"), bins = 15) {

  metric <- match.arg(metric)

  if (!"terrain_class" %in% names(bearing_dt)) {
    message(paste(
      "plot_terrain_class_hist(): bearing_dt nao tem coluna 'terrain_class' --",
      "corre compute_curtailment_bearing(..., turbine_terrain_dt = ...) para a obter. NULL devolvido."
    ))
    return(NULL)
  }

  dt <- bearing_dt[!is.na(terrain_class) & !is.na(get(metric))]
  if (nrow(dt) == 0L) {
    message(sprintf("plot_terrain_class_hist(): sem eventos com '%s' calculavel -- NULL devolvido.", metric))
    return(NULL)
  }

  x_lab <- .bearing_metric_label(metric)

  ggplot(dt, aes(x = .data[[metric]])) +
    geom_histogram(bins = bins, colour = "grey", fill = "#17aeb0") +
    facet_wrap(~terrain_class, ncol = 3, scales = "free_y") +
    labs(
      x = x_lab, y = "Count",
      title = sprintf("Distribution of %s by terrain class", tolower(x_lab))
    ) +
    theme_bw()
}
