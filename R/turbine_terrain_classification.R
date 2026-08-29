##
## Classificacao do terreno a volta de cada turbina (crista/ridge, complexo,
## plano), a partir do DEM -- pedido do Paulo (2026-08): a analise de setor de
## aproximacao (R/curtailment_bearing_sectors.R) nao mostrou padrao claro a
## 8 setores; o proximo passo e' testar se a proximidade a uma crista/relevo
## complexo (fator conhecido de influenciar velocidade e altura de voo) e' um
## fator melhor do que a direcao de bussola.
##
## Metodologia (simples/exploratoria, thresholds ajustaveis -- ver
## classify_terrain()): para cada turbina, recorta o DEM a 2 buffers
## circulares CONCENTRICOS a volta da turbina -- radius_inner_m (250m por
## omissao) e radius_outer_m (500m por omissao) -- mesma logica de
## crop/mask/reprojetar para uma CRS local planar centrada na turbina de
## R/coverage_3d_topography.R, build_terrain_mesh(). 2 buffers em vez de 1
## (pedido do Paulo, 2026-08): 1 buffer so' nao apanha o caso de uma turbina
## num pequeno plateau, com uma crista real a algumas centenas de metros --
## como essa crista ja fica DENTRO de um buffer unico largo o suficiente
## para a incluir, a media desse buffer tambem sobe, e a diferenca
## (elev_m menos essa media) fica pequena mesmo a turbina estando
## claramente perto de relevo. Com 2 buffers separamos:
##   elev_m             -- cota da propria turbina
##   mean_elev_inner_m   -- cota media do buffer PEQUENO (o footing imediato
##                          da turbina -- ex: o proprio plateau onde assenta)
##   mean_elev_outer_m   -- cota media do buffer GRANDE (footing + vizinhanca
##                          mais larga -- ja apanha uma crista a algumas
##                          centenas de metros, se existir)
##   relative_elev_m     -- elev_m - mean_elev_outer_m (definicao original,
##                          1 buffer so' -- mantida para comparacao)
##   elev_gradient_m     -- mean_elev_outer_m - mean_elev_inner_m (positivo =
##                          terreno a subir nalgum ponto entre os 2 raios --
##                          sinal de crista PERTO mas fora do footing imediato)
##   ridge_proximity_m   -- max(elev_m - mean_elev_inner_m, elev_gradient_m)
##                          -- combina "a turbina esta em cima de algo" com
##                          "ha relevo mais alto perto dela" num so' numero
##   mean_slope_deg      -- inclinacao media do terreno no buffer outer (terra::terrain, "slope")
##   mean_tri_m          -- Terrain Ruggedness Index medio no buffer outer (terra::terrain,
##                          "TRI" -- diferenca media de cota entre cada celula e
##                          as suas 8 vizinhas; mais alto = terreno mais acidentado)
##
## classify_terrain() classifica em 3 classes, por esta ordem de prioridade:
##   "ridge"   -- <ridge_metric> >= ridge_relelev_m (por omissao ridge_metric
##                = "ridge_proximity_m", o combinado dos 2 buffers acima;
##                pode ser trocado para "relative_elev_m", a definicao
##                original de 1 buffer so', para comparar os 2 criterios)
##   "complex" -- (nao ridge) e mean_slope_deg >= complex_slope_deg
##   "flat"    -- resto (vizinhanca pouco acidentada e turbina nao elevada)
##
## Limiares POR OMISSAO SAO RELATIVOS AO PROPRIO PARQUE, nao valores fixos:
## ridge_relelev_m/complex_slope_deg (omissao NULL) sao calculados como
## quantis (ridge_relelev_q=0.90, complex_slope_q=0.75 por omissao) da
## distribuicao observada nas turbinas passadas a classify_terrain() --
## ridge = top ~10% por ridge_metric, complex = top ~25% das restantes
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
##   terrain_dt <- compute_turbine_terrain_metrics(wtg, dem_file, radius_inner_m = 250, radius_outer_m = 500)
##   terrain_dt <- classify_terrain(terrain_dt) # ridge_proximity_m + limiares por quantil, calibrados a este parque
##   attr(terrain_dt, "thresholds") # ver os valores (m/graus) que os quantis deram
##   plot_turbine_terrain_map(wtg, terrain_dt) # confirma visualmente antes de usar
##   summarise_terrain_class_counts(terrain_dt) # n e % de turbinas por classe (tabela do relatorio)
##
##   # comparar com o criterio original (1 buffer so'):
##   terrain_dt_v1 <- classify_terrain(terrain_dt, ridge_metric = "relative_elev_m")
##
##   # limiares absolutos, se preferires um valor conhecido em vez de relativo:
##   terrain_dt <- classify_terrain(terrain_dt, ridge_relelev_m = 15, complex_slope_deg = 8)
##
##   # cruzar com a analise de setor -- ver R/curtailment_bearing_sectors.R
##   bearing_dt <- compute_curtailment_bearing(curtl_dt, track_dt, wtg, turbine_terrain_dt = terrain_dt)
##


## 1. Metricas de terreno a volta de cada turbina (a partir do DEM) ----
##
## 2 buffers concentricos (radius_inner_m/radius_outer_m, 250m/500m por
## omissao) em vez de 1 -- pedido do Paulo (2026-08): um so' buffer nao
## apanha o caso de uma turbina num pequeno plateau, com uma crista real a
## algumas centenas de metros -- o buffer unico (500m) ja inclui essa
## crista, por isso a media dele tambem sobe e relative_elev_m (elev_m
## menos essa media) fica pequeno, mesmo a turbina estando claramente perto
## de relevo. Com 2 buffers conseguimos separar "a turbina esta em cima de
## algo" (elev_m vs mean_elev_inner_m, footing imediato) de "ha relevo mais
## alto perto, so' que fora do footing imediato" (mean_elev_outer_m acima de
## mean_elev_inner_m -- o buffer maior, ao incluir mais area, apanha essa
## crista e a media sobe em relacao ao buffer pequeno).
##
## elev_gradient_m = mean_elev_outer_m - mean_elev_inner_m -- positivo
## sugere terreno a subir algures entre radius_inner_m e radius_outer_m
## (crista fora do footing imediato, mas ainda perto).
## ridge_proximity_m = pmax(elev_m - mean_elev_inner_m, elev_gradient_m) --
## combina os 2 sinais (turbina elevada OU relevo a subir perto dela) num
## unico numero, usado por omissao em classify_terrain() (ver ridge_metric).
## relative_elev_m mantido (elev_m - mean_elev_outer_m, definicao original,
## 1 buffer so') para comparacao lado-a-lado.

compute_turbine_terrain_metrics <- function(wtg_sf, dem_file,
                                            radius_inner_m = 250, radius_outer_m = 500,
                                            wtg_id_col = "InternalNa") {

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

    ## Recorta/reprojeta o DEM UMA SO' VEZ, ao buffer MAIOR (outer) -- o
    ## buffer inner e' so' uma mascara adicional sobre esse raster ja local,
    ## nao precisa de recortar/reprojetar o DEM outra vez
    wtg_buffer_outer_local <- sf::st_buffer(wtg_pt_local, dist = radius_outer_m)
    wtg_buffer_demcrs      <- sf::st_transform(wtg_buffer_outer_local, sf::st_crs(dem)$wkt)

    dem_crop  <- terra::crop(dem, terra::vect(wtg_buffer_demcrs))
    dem_crop  <- terra::mask(dem_crop, terra::vect(wtg_buffer_demcrs))
    dem_local <- terra::project(dem_crop, crs_local) # extensao = buffer outer

    wtg_buffer_inner_local <- sf::st_buffer(wtg_pt_local, dist = radius_inner_m)
    dem_local_inner        <- terra::mask(dem_local, terra::vect(wtg_buffer_inner_local))

    wtg_elev <- as.numeric(terra::extract(dem_local, terra::vect(wtg_pt_local))[[2]])

    slope_local <- terra::terrain(dem_local, v = "slope", unit = "degrees")
    tri_local   <- terra::terrain(dem_local, v = "TRI")

    mean_elev_inner <- as.numeric(terra::global(dem_local_inner, "mean", na.rm = TRUE)[1, 1])
    mean_elev_outer <- as.numeric(terra::global(dem_local, "mean", na.rm = TRUE)[1, 1])
    mean_slope      <- as.numeric(terra::global(slope_local, "mean", na.rm = TRUE)[1, 1])
    mean_tri        <- as.numeric(terra::global(tri_local, "mean", na.rm = TRUE)[1, 1])

    elev_gradient   <- mean_elev_outer - mean_elev_inner
    ridge_proximity <- max(wtg_elev - mean_elev_inner, elev_gradient)

    data.table::data.table(
      wtg_id             = wtg_id,
      elev_m             = wtg_elev,
      mean_elev_inner_m  = round(mean_elev_inner, 1),
      mean_elev_outer_m  = round(mean_elev_outer, 1),
      relative_elev_m    = round(wtg_elev - mean_elev_outer, 1), # definicao original (1 buffer), para comparacao
      elev_gradient_m    = round(elev_gradient, 1),
      ridge_proximity_m  = round(ridge_proximity, 1),
      mean_slope_deg     = round(mean_slope, 1),
      mean_tri_m         = round(mean_tri, 1)
    )
  })

  data.table::rbindlist(results)
}


## 2. Classificacao em 3 classes (ver limiares na nota do topo do ficheiro) ----
##
## ridge_metric -- qual coluna de turbine_terrain_dt usar para o criterio de
## "ridge": "ridge_proximity_m" (omissao, o combinado de 2 buffers -- ver
## compute_turbine_terrain_metrics()) ou "relative_elev_m" (definicao
## original, 1 buffer so', para comparar os 2 criterios lado-a-lado).
##
## ridge_relelev_m/complex_slope_deg = NULL (omissao) -- limiar calculado
## como quantil (ridge_relelev_q/complex_slope_q) da distribuicao das
## turbinas em turbine_terrain_dt; um valor numerico explicito desliga o
## calculo por quantil correspondente e usa esse valor fixo.

classify_terrain <- function(turbine_terrain_dt, ridge_metric = "ridge_proximity_m",
                             ridge_relelev_m = NULL, ridge_relelev_q = 0.90,
                             complex_slope_deg = NULL, complex_slope_q = 0.75) {

  dt <- data.table::copy(turbine_terrain_dt)
  ridge_values <- dt[[ridge_metric]]

  ridge_cutoff <- if (!is.null(ridge_relelev_m)) {
    ridge_relelev_m
  } else {
    as.numeric(stats::quantile(ridge_values, probs = ridge_relelev_q, na.rm = TRUE))
  }
  dt[, is_ridge := ridge_values >= ridge_cutoff]

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
    "classify_terrain(): ridge se %s >= %.1f m%s; complex se mean_slope_deg >= %.1f graus%s.",
    ridge_metric, ridge_cutoff, if (is.null(ridge_relelev_m)) sprintf(" (quantil %.2f)", ridge_relelev_q) else " (valor fixo)",
    complex_cutoff, if (is.null(complex_slope_deg)) sprintf(" (quantil %.2f, so' nao-ridge)", complex_slope_q) else " (valor fixo)"
  ))
  data.table::setattr(dt, "thresholds", list(ridge_metric = ridge_metric, ridge_cutoff = ridge_cutoff, complex_slope_deg = complex_cutoff))

  dt[]
}


## 3. Resumo (n e % de turbinas) por classe -- tabela para o relatorio ----
##
## Completa classes sem nenhuma turbina com n=0/pct=0 (ex: se o parque nao
## tiver nenhuma turbina "complex") -- mesma convencao de preenchimento
## usada noutras seccoes deste projeto (ex: summarise_bearing_sectors(),
## R/curtailment_bearing_sectors.R).

summarise_terrain_class_counts <- function(turbine_terrain_dt) {

  out <- turbine_terrain_dt[, .(n_turbines = .N), by = terrain_class]
  out[, terrain_class := as.character(terrain_class)]

  missing_classes <- setdiff(c("flat", "complex", "ridge"), out$terrain_class)
  if (length(missing_classes) > 0) {
    out <- data.table::rbindlist(list(
      out, data.table::data.table(terrain_class = missing_classes, n_turbines = 0L)
    ))
  }

  out[, terrain_class := factor(terrain_class, levels = c("flat", "complex", "ridge"))]
  data.table::setorder(out, terrain_class)
  out[, pct_turbines := round(100 * n_turbines / sum(n_turbines), 1)]
  out[]
}


## 4. Mapa de confirmacao visual (turbinas coloridas pela classe) ----

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
