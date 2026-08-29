##
## Classificacao do terreno a volta de cada turbina (crista/ridge, complexo,
## plano), a partir do DEM -- pedido do Paulo (2026-08): a analise de setor de
## aproximacao (R/curtailment_bearing_sectors.R) nao mostrou padrao claro a
## 8 setores; o proximo passo e' testar se a proximidade a uma crista/relevo
## complexo (fator conhecido de influenciar velocidade e altura de voo) e' um
## fator melhor do que a direcao de bussola.
##
## Metodologia (simples/exploratoria, thresholds ajustaveis -- ver
## classify_terrain()): para cada turbina, recorta o DEM a um buffer circular
## de raio radius_m a volta da turbina (mesma logica de crop/mask/reprojetar
## para uma CRS local planar centrada na turbina de
## R/coverage_3d_topography.R, build_terrain_mesh()) e calcula:
##   elev_m           -- cota da propria turbina
##   mean_elev_m       -- cota media do buffer a' volta
##   relative_elev_m   -- elev_m - mean_elev_m (positivo = turbina mais alta
##                        que a vizinhanca -- indicador de posicao de crista/
##                        topo de relevo)
##   mean_slope_deg    -- inclinacao media do terreno no buffer (terra::terrain, "slope")
##   mean_tri_m        -- Terrain Ruggedness Index medio no buffer (terra::terrain,
##                        "TRI" -- diferenca media de cota entre cada celula e
##                        as suas 8 vizinhas; mais alto = terreno mais acidentado)
##
## classify_terrain() classifica em 3 classes, por esta ordem de prioridade:
##   "ridge"   -- relative_elev_m >= ridge_relelev_m (turbina claramente
##                acima da vizinhanca, independentemente da inclinacao)
##   "complex" -- (nao ridge) e mean_slope_deg >= complex_slope_deg
##   "flat"    -- resto (vizinhanca pouco acidentada e turbina nao elevada)
##
## Limiares POR OMISSAO SAO RELATIVOS AO PROPRIO PARQUE, nao valores fixos:
## ridge_relelev_m/complex_slope_deg (omissao NULL) sao calculados como
## quantis (ridge_relelev_q=0.90, complex_slope_q=0.75 por omissao) da
## distribuicao observada nas turbinas passadas a classify_terrain() --
## ridge = top ~10% por relative_elev_m, complex = top ~25% das restantes
## por mean_slope_deg. Um limiar FIXO (ex: 15m/8 graus, a 1a versao desta
## funcao) e' demasiado rigido -- um relevo suave onde a maior "crista"
## real do parque so' tem 8m de desnivel nunca teria nenhuma turbina
## classificada como "ridge", e um parque muito acidentado podia classificar
## quase tudo como "ridge"/"complex" (caso real do Bash, 2026-08: so' 1
## turbina em 80 ficou "ridge" e nenhuma "complex" com os limiares fixos
## originais). A versao por quantil garante sempre uma divisao nao-trivial
## nas 3 classes, calibrada ao relevo real de CADA parque -- passar valores
## absolutos a ridge_relelev_m/complex_slope_deg desliga o calculo por
## quantil e volta ao comportamento antigo (limiar fixo), se preferires um
## valor absoluto conhecido em vez de relativo ao parque.
## classify_terrain() imprime os limiares efetivamente usados (mensagem) e
## guarda-os tambem no atributo "thresholds" do resultado (attr(dt, "thresholds")).
##
## Depende de: data.table, sf, terra, ggplot2
##
## Uso:
##   source("R/turbine_terrain_classification.R")
##   terrain_dt <- compute_turbine_terrain_metrics(wtg, dem_file, radius_m = 500)
##   terrain_dt <- classify_terrain(terrain_dt) # limiares por quantil, calibrados a este parque
##   attr(terrain_dt, "thresholds") # ver os valores (m/graus) que os quantis deram
##   plot_turbine_terrain_map(wtg, terrain_dt) # confirma visualmente antes de usar
##
##   # limiares absolutos, se preferires um valor conhecido em vez de relativo:
##   terrain_dt <- classify_terrain(terrain_dt, ridge_relelev_m = 15, complex_slope_deg = 8)
##
##   # cruzar com a analise de setor -- ver R/curtailment_bearing_sectors.R
##   bearing_dt <- compute_curtailment_bearing(curtl_dt, track_dt, wtg, turbine_terrain_dt = terrain_dt)
##


## 1. Metricas de terreno a volta de cada turbina (a partir do DEM) ----

compute_turbine_terrain_metrics <- function(wtg_sf, dem_file, radius_m = 500, wtg_id_col = "InternalNa") {

  wtg_wgs84 <- sf::st_transform(wtg_sf, 4326)
  coords    <- sf::st_coordinates(wtg_wgs84)

  wtg_list <- data.table::data.table(
    wtg_id  = wtg_sf[[wtg_id_col]],
    wtg_lon = coords[, "X"],
    wtg_lat = coords[, "Y"]
  )

  dem <- terra::rast(dem_file)

  results <- lapply(seq_len(nrow(wtg_list)), function(i) {

    wtg_id  <- wtg_list$wtg_id[i]
    wtg_lon <- wtg_list$wtg_lon[i]
    wtg_lat <- wtg_list$wtg_lat[i]

    ## CRS local planar centrada nesta turbina -- mesma abordagem de
    ## build_terrain_mesh() (R/coverage_3d_topography.R)
    crs_local <- sprintf(
      "+proj=aeqd +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs",
      wtg_lat, wtg_lon
    )

    wtg_pt_sf    <- sf::st_as_sf(data.frame(id = wtg_id, lon = wtg_lon, lat = wtg_lat), coords = c("lon", "lat"), crs = 4326)
    wtg_pt_local <- sf::st_transform(wtg_pt_sf, crs_local)

    wtg_buffer_local  <- sf::st_buffer(wtg_pt_local, dist = radius_m)
    wtg_buffer_demcrs <- sf::st_transform(wtg_buffer_local, sf::st_crs(dem)$wkt)

    dem_crop  <- terra::crop(dem, terra::vect(wtg_buffer_demcrs))
    dem_crop  <- terra::mask(dem_crop, terra::vect(wtg_buffer_demcrs))
    dem_local <- terra::project(dem_crop, crs_local)

    wtg_elev <- as.numeric(terra::extract(dem_local, terra::vect(wtg_pt_local))[[2]])

    slope_local <- terra::terrain(dem_local, v = "slope", unit = "degrees")
    tri_local   <- terra::terrain(dem_local, v = "TRI")

    mean_elev  <- as.numeric(terra::global(dem_local, "mean", na.rm = TRUE)[1, 1])
    mean_slope <- as.numeric(terra::global(slope_local, "mean", na.rm = TRUE)[1, 1])
    mean_tri   <- as.numeric(terra::global(tri_local, "mean", na.rm = TRUE)[1, 1])

    data.table::data.table(
      wtg_id          = wtg_id,
      elev_m          = wtg_elev,
      mean_elev_m     = round(mean_elev, 1),
      relative_elev_m = round(wtg_elev - mean_elev, 1),
      mean_slope_deg  = round(mean_slope, 1),
      mean_tri_m      = round(mean_tri, 1)
    )
  })

  data.table::rbindlist(results)
}


## 2. Classificacao em 3 classes (ver limiares na nota do topo do ficheiro) ----
##
## ridge_relelev_m/complex_slope_deg = NULL (omissao) -- limiar calculado
## como quantil (ridge_relelev_q/complex_slope_q) da distribuicao das
## turbinas em turbine_terrain_dt; um valor numerico explicito desliga o
## calculo por quantil correspondente e usa esse valor fixo.

classify_terrain <- function(turbine_terrain_dt,
                             ridge_relelev_m = NULL, ridge_relelev_q = 0.90,
                             complex_slope_deg = NULL, complex_slope_q = 0.75) {

  dt <- data.table::copy(turbine_terrain_dt)

  ridge_cutoff <- if (!is.null(ridge_relelev_m)) {
    ridge_relelev_m
  } else {
    as.numeric(stats::quantile(dt$relative_elev_m, probs = ridge_relelev_q, na.rm = TRUE))
  }
  dt[, is_ridge := relative_elev_m >= ridge_cutoff]

  ## quantil de mean_slope_deg SO' entre as nao-ridge -- uma turbina de
  ## crista tipicamente ja tem slope alto so' por estar no topo do relevo;
  ## sem este filtro o quantil de complex ficava inflacionado pelas proprias
  ## ridges, deixando poucas (ou nenhumas) turbinas acima do limiar de "complex"
  non_ridge_slope <- dt[is_ridge == FALSE, mean_slope_deg]
  complex_cutoff <- if (!is.null(complex_slope_deg)) {
    complex_slope_deg
  } else if (length(non_ridge_slope) == 0L) {
    Inf # todas as turbinas ficaram "ridge" -- nao ha nao-ridge para calcular o quantil de "complex"
  } else {
    as.numeric(stats::quantile(non_ridge_slope, probs = complex_slope_q, na.rm = TRUE))
  }

  dt[, terrain_class := data.table::fcase(
    is_ridge, "ridge",
    mean_slope_deg >= complex_cutoff, "complex",
    default = "flat"
  )]
  dt[, terrain_class := factor(terrain_class, levels = c("flat", "complex", "ridge"))]
  dt[, is_ridge := NULL]

  message(sprintf(
    "classify_terrain(): ridge se relative_elev_m >= %.1f m%s; complex se mean_slope_deg >= %.1f graus%s.",
    ridge_cutoff, if (is.null(ridge_relelev_m)) sprintf(" (quantil %.2f)", ridge_relelev_q) else " (valor fixo)",
    complex_cutoff, if (is.null(complex_slope_deg)) sprintf(" (quantil %.2f, so' nao-ridge)", complex_slope_q) else " (valor fixo)"
  ))
  data.table::setattr(dt, "thresholds", list(ridge_relelev_m = ridge_cutoff, complex_slope_deg = complex_cutoff))

  dt[]
}


## 3. Mapa de confirmacao visual (turbinas coloridas pela classe) ----

plot_turbine_terrain_map <- function(wtg_sf, turbine_terrain_dt, wtg_id_col = "InternalNa") {

  class_lookup <- stats::setNames(as.character(turbine_terrain_dt$terrain_class), turbine_terrain_dt$wtg_id)

  map_sf <- wtg_sf
  map_sf$wtg_id <- wtg_sf[[wtg_id_col]]
  map_sf$terrain_class <- class_lookup[map_sf$wtg_id]

  ggplot(map_sf) +
    geom_sf(aes(colour = terrain_class), size = 3) +
    geom_sf_text(aes(label = wtg_id), nudge_y = 50, size = 3) +
    scale_colour_manual(values = c(flat = "#4daf4a", complex = "#ff7f00", ridge = "#e41a1c")) +
    labs(colour = "Terrain class", title = "Turbine terrain classification") +
    theme_bw()
}
