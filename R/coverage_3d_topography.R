##
## Cobertura 3D corrigida a topografia (DEM), por turbina
##
## Adaptado dos scripts originais scripts_IDF/IDF_3Dcylinder_mesh_coverage_with_topography02.R
## e scripts_IDF/IDF_Plot_3Dcylinder_mesh_coverage_with_topography.R.
##
## Constroi uma malha 3D num cilindro centrado em cada turbina, corrigida a
## elevacao real do terreno (via DEM), classifica cada no da malha como
## "terrain" ou "air" e por banda de risco (risk_band, limites parametrizaveis),
## e usa as deteções de aves (Track Report) para marcar que nos da malha "air"
## tiveram cobertura de deteção (nearest neighbour 3D dentro de prox_thresh_m).
##
## dem_file: GeoTIFF cobrindo toda a area do parque (ex: Copernicus GLO-30),
## descarregado manualmente e colocado em databases_dir -- ver dem_filename em
## userSettings_BSH.R. O crop/mask ao raio da turbina e feito aqui, nao e
## preciso pre-recortar o ficheiro.
##
## z_floor -- a malha ja NAO fica limitada a comecar na cota da propria
## turbina (z relativo = 0): estende-se para baixo ate a cota mais baixa do
## relevo dentro do raio de analise (arredondada ao multiplo de step_z mais
## proximo, para baixo). Sem isto, deteções de aves a voar perto do solo em
## zonas de relevo mais baixo que a base da turbina (ex: um vale) ficavam de
## fora da analise (mesh e tracks), porque a malha simplesmente nao tinha nos
## nessa gama de altura. A logica de classificacao "terrain"/"air" por coluna
## (x,y) e a mesma de sempre, so a gama de z_rel_turbine testada e que cresce.
## A banda de risco "at risk" (risk_band_breaks[1], 200m por omissao) passa a
## cobrir tambem toda a gama abaixo de 0 ate z_floor.
##
## low_sample -- coluna em metrics/by_turbine (compute_mesh_coverage() /
## summarise_mesh_coverage()) a assinalar turbinas com menos de
## min_sample_records (500 mil por omissao) registos de track candidatos --
## o % de coverage dessas turbinas deve ser interpretado com cautela (amostra
## pode ser demasiado pequena para uma estimativa fiavel).
##
## pct_low_terrain_covered -- coluna extra em metrics/by_turbine: % de cobertura
## dos nos de baixo relevo (os low_terrain_levels = 2 niveis de malha mais
## baixos acima do terreno em cada coluna x/y, ver .flag_low_terrain()),
## independente da altura AGL e das bandas de risco/distancia.
##
## dist_band_breaks (por omissao NULL, desliga esta classificacao -- ainda
## nao usada por BSH/DGY/IDF_analysis.R): banda de distancia HORIZONTAL a
## turbina, ex: c(600) para "inner"/"outer" a 600m -- pedido do Paulo,
## 2026-08 (Zarafshan), a partir de scripts_IDF/coverage_analysis_WTG.R
## (que corria a analise 2x, uma por raio, para uma "inner cylinder
## coverage" separada) -- aqui cruzado com risk_band NA MESMA malha/corrida
## (by_risk_dist_band, compute_mesh_coverage()/summarise_mesh_coverage()),
## sem repetir o calculo caro da malha/KD-tree para cada distancia.
##
## Depende de: data.table, sf, terra, RANN, plotly, htmlwidgets;
## webshot2 so' se save_coverage_3d_plots(..., screenshot = TRUE) for usado
## (precisa tambem de um Chrome/Edge instalado no sistema)
##
## Uso:
##   source("R/coverage_3d_topography.R")
##   dem_file <- file.path(databases_dir, dem_filename)
##   cov_all  <- run_coverage_3d_all_turbines(
##     wtg, track_dt, dem_file,
##     radius = coverage_cylinder_wider_radius, cyl_height = coverage_cylinder_height,
##     step_xy = coverage_mesh_step_xy, step_z = coverage_mesh_step_z,
##     prox_thresh_m = coverage_prox_thresh_m,
##     risk_band_breaks = c(200), risk_band_labels = c("at risk", "above"),
##     dist_band_breaks = coverage_cylinder_inner_radius, dist_band_labels = c("inner", "outer"), # opcional
##     wtg_sel = wtg_3d_coverage # vetor de nomes (coluna InternalNa), ou "all"/NULL para todas
##   )
##   summary_cov <- summarise_mesh_coverage(lapply(cov_all, `[[`, "coverage"))
##   summary_cov$by_turbine_risk_dist_band # so' nao-vazio se dist_band_breaks foi usado
##   plot_mesh_coverage_3d(cov_all$BSH54$terrain_mesh, cov_all$BSH54$coverage,
##                        radius = coverage_cylinder_wider_radius, cyl_height = coverage_cylinder_height)
##


## 1. Malha 3D corrigida ao terreno, para UMA turbina ----

## 1.a Logica pura (sem DEM/CRS) de construcao da malha z, dado mesh_xy (x, y,
## terrain_elev) ja calculado -- separada de build_terrain_mesh() para poder
## ser testada com dados sinteticos, sem precisar de um DEM real.
##
## z_floor: estende a malha para baixo da cota da turbina (z relativo = 0) ate
## a cota mais baixa do terreno em mesh_xy, arredondada para baixo ao
## multiplo de step_z mais proximo -- nunca sobe acima de 0 (min(0, ...)),
## turbinas em zonas planas/elevadas mantêm o comportamento antigo (zs comeca em 0).
##
## dist_band_breaks (por omissao NULL -- desliga esta classificacao, mantendo
## o comportamento antigo/farm-wide de BSH/DGY inalterado): banda de
## distancia HORIZONTAL a turbina (raio no plano x/y, nao 3D), ex: c(600)
## para separar "inner"/"outer" a 600m -- pedido do Paulo, 2026-08 (Zarafshan):
## alem da altura de risco (curtailment so' dispara abaixo de
## curtailment_trigger_height_m), a IDF tambem usa um raio horizontal de
## 600m para despoletar uma resposta IMEDIATA -- por isso a cobertura
## interessa cruzada nas 2 dimensoes (altura x distancia), nao so' cada uma
## em separado. Reutiliza coverage_cylinder_inner_radius (userSettings) como
## valor tipico de dist_band_breaks -- ver run_coverage_3d_all_turbines().
.build_mesh_from_terrain <- function(mesh_xy, wtg_elev, cyl_height, step_z,
                                     risk_band_breaks = c(200),
                                     risk_band_labels = c("at risk", "above"),
                                     dist_band_breaks = NULL,
                                     dist_band_labels = c("inner", "outer")) {

  z_floor <- min(0, floor((min(mesh_xy$terrain_elev, na.rm = TRUE) - wtg_elev) / step_z) * step_z)
  zs <- seq(z_floor, cyl_height, by = step_z)

  mesh <- mesh_xy[, .(x, y)][, .(z_rel_turbine = zs), by = .(x, y)]
  mesh[, z_abs := wtg_elev + z_rel_turbine]
  mesh <- merge(mesh, mesh_xy, by = c("x", "y"), all.x = TRUE)

  mesh[, vertical_clearance := z_abs - terrain_elev]
  mesh[, medium := data.table::fifelse(
    is.na(terrain_elev), NA_character_,
    data.table::fifelse(vertical_clearance <= 0, "terrain", "air")
  )]

  mesh[, risk_band := cut(
    z_rel_turbine,
    breaks = c(z_floor, risk_band_breaks, cyl_height),
    labels = risk_band_labels,
    include.lowest = TRUE, right = FALSE
  )]

  if (!is.null(dist_band_breaks)) {
    mesh[, dist_band := cut(
      sqrt(x^2 + y^2),
      breaks = c(0, dist_band_breaks, Inf),
      labels = dist_band_labels,
      include.lowest = TRUE, right = FALSE
    )]
  }

  list(z_floor = z_floor, mesh = mesh)
}

build_terrain_mesh <- function(wtg_id, wtg_lat, wtg_lon, dem_file,
                               radius, cyl_height, step_xy, step_z,
                               risk_band_breaks = c(200),
                               risk_band_labels = c("at risk", "above"),
                               dist_band_breaks = NULL,
                               dist_band_labels = c("inner", "outer")) {

  crs_local <- sprintf(
    "+proj=aeqd +lat_0=%f +lon_0=%f +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs",
    wtg_lat, wtg_lon
  )

  wtg_sf    <- sf::st_as_sf(data.frame(id = wtg_id, lon = wtg_lon, lat = wtg_lat),
                            coords = c("lon", "lat"), crs = 4326)
  wtg_local <- sf::st_transform(wtg_sf, crs_local)

  ## DEM: crop/mask ao buffer da turbina, reprojetar para o CRS local
  dem <- terra::rast(dem_file)

  wtg_buffer_local  <- sf::st_buffer(wtg_local, dist = radius + 200)
  wtg_buffer_demcrs <- sf::st_transform(wtg_buffer_local, sf::st_crs(dem)$wkt)

  dem_crop  <- terra::crop(dem, terra::vect(wtg_buffer_demcrs))
  dem_crop  <- terra::mask(dem_crop, terra::vect(wtg_buffer_demcrs))
  dem_local <- terra::project(dem_crop, crs_local)

  wtg_elev <- as.numeric(terra::extract(dem_local, terra::vect(wtg_local))[[2]])

  ## Malha x/y dentro do raio (z: ver .build_mesh_from_terrain())
  xs <- seq(-radius, radius, by = step_xy)
  ys <- seq(-radius, radius, by = step_xy)

  grid_xy <- data.table::CJ(x = xs, y = ys)
  grid_xy <- grid_xy[(x^2 + y^2) <= radius^2]

  ## Elevacao do terreno em cada no x/y (para classificar a malha e desenhar a superficie)
  mesh_xy    <- data.table::copy(grid_xy)
  mesh_xy_sf <- sf::st_as_sf(mesh_xy, coords = c("x", "y"), crs = crs_local)

  terrain_vals <- terra::extract(dem_local, terra::vect(mesh_xy_sf))
  mesh_xy[, terrain_elev := as.numeric(terrain_vals[[2]])]

  built <- .build_mesh_from_terrain(
    mesh_xy, wtg_elev, cyl_height, step_z, risk_band_breaks, risk_band_labels,
    dist_band_breaks, dist_band_labels
  )
  mesh <- built$mesh

  list(
    wtg_id       = wtg_id,
    crs_local    = crs_local,
    wtg_elev     = wtg_elev,
    z_floor      = built$z_floor,
    dem_local    = dem_local,
    mesh_air     = mesh[medium == "air" & !is.na(risk_band)],
    mesh_terrain = mesh[medium == "terrain"],
    mesh_xy      = mesh_xy # elevacao do terreno por no x/y -- usado no plot da superficie
  )
}


## 2. Cobertura da malha "air" pelas deteções de aves, para UMA turbina ----
##    track_wtg: subconjunto de track_dt para esta turbina (colunas lat, lon, height)

## Marca os nos "air" de BAIXO RELEVO: em cada coluna (x, y), os
## n_levels niveis de malha mais baixos acima do terreno (os primeiros
## n_levels valores de z_rel_turbine com medium == "air" nessa coluna),
## qualquer que seja a altura AGL ou a banda de risco -- altura relativa ao
## terreno local, nao uma banda de altura fixa (pedido do Paulo, 2026-10:
## zonas de baixo relevo podem estar mal cobertas, porque as aves nao voam
## la ou a IDF nao as ve). mesh_air so' tem nos "air", por isso o rank
## dentro da coluna e' o rank dos niveis acima do terreno. Logica pura
## (sem DEM/CRS), testavel com dados sinteticos.
.flag_low_terrain <- function(mesh_air, n_levels = 2L) {
  if (nrow(mesh_air) == 0L) {
    mesh_air[, low_terrain := logical()]
    return(mesh_air[])
  }
  mesh_air[, low_terrain := data.table::frank(z_rel_turbine, ties.method = "dense") <= n_levels,
           by = .(x, y)]
  mesh_air[]
}

compute_mesh_coverage <- function(terrain_mesh, track_wtg, radius, cyl_height, prox_thresh_m,
                                  min_sample_records = 500000, low_terrain_levels = 2L) {

  wtg_id    <- terrain_mesh$wtg_id
  mesh_air  <- data.table::copy(terrain_mesh$mesh_air)
  crs_local <- terrain_mesh$crs_local
  wtg_elev  <- terrain_mesh$wtg_elev
  dem_local <- terrain_mesh$dem_local
  z_floor   <- terrain_mesh$z_floor

  has_dist_band <- "dist_band" %in% names(mesh_air)

  .flag_low_terrain(mesh_air, low_terrain_levels)

  empty_result <- function(n_records, track_wtg_valid) {
    mesh_air[, `:=`(hits = 0L, covered = FALSE)]
    list(
      wtg_id = wtg_id, mesh_air = mesh_air, track_wtg = track_wtg_valid,
      metrics = data.table::data.table(
        wtg_id = wtg_id, n_records = n_records, n_valid = nrow(track_wtg_valid),
        n_air_mesh = nrow(mesh_air), n_covered = 0L, pct_covered = NA_real_,
        n_low_terrain_mesh = sum(mesh_air$low_terrain), n_low_terrain_covered = 0L,
        pct_low_terrain_covered = NA_real_,
        low_sample = n_records < min_sample_records
      ),
      by_risk_band = mesh_air[, .(n_mesh = .N, n_covered = 0L, pct_covered = 0), by = risk_band][, wtg_id := wtg_id][],
      by_risk_dist_band = if (has_dist_band) {
        mesh_air[, .(n_mesh = .N, n_covered = 0L, pct_covered = 0), by = .(risk_band, dist_band)][, wtg_id := wtg_id][]
      } else NULL
    )
  }

  if (nrow(track_wtg) == 0L || nrow(mesh_air) == 0L) return(empty_result(nrow(track_wtg), track_wtg[0]))

  track_wtg    <- data.table::copy(track_wtg)
  track_wtg_sf <- sf::st_as_sf(track_wtg, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  track_wtg_m  <- sf::st_transform(track_wtg_sf, crs_local)
  coords_bird  <- sf::st_coordinates(track_wtg_m)

  track_wtg[, `:=`(x = coords_bird[, 1], y = coords_bird[, 2])]

  bird_terrain_vals <- terra::extract(dem_local, terra::vect(track_wtg_m))
  track_wtg[, terrain_elev := as.numeric(bird_terrain_vals[[2]])]

  track_wtg[, z_abs := terrain_elev + height]
  track_wtg[, z_rel_turbine := z_abs - wtg_elev]

  track_wtg_valid <- track_wtg[
    !is.na(x) & !is.na(y) & !is.na(z_rel_turbine) &
      (x^2 + y^2) <= radius^2 &
      z_rel_turbine >= z_floor & z_rel_turbine <= cyl_height
  ]

  if (nrow(track_wtg_valid) == 0L) return(empty_result(nrow(track_wtg), track_wtg_valid))

  M_air <- as.matrix(mesh_air[, .(x, y, z_rel_turbine)])
  B     <- as.matrix(track_wtg_valid[, .(x, y, z_rel_turbine)])

  nn <- RANN::nn2(data = M_air, query = B, k = 1)
  idx_air <- as.integer(nn$nn.idx[, 1])
  dist3d  <- as.numeric(nn$nn.dists[, 1])

  idx_air_use <- idx_air[dist3d <= prox_thresh_m]
  counts_air  <- tabulate(idx_air_use, nbins = nrow(mesh_air))

  mesh_air[, hits := counts_air]
  mesh_air[, covered := hits > 0L]

  n_total <- nrow(mesh_air)
  n_cov   <- sum(mesh_air$covered)
  n_low     <- sum(mesh_air$low_terrain)
  n_low_cov <- sum(mesh_air$low_terrain & mesh_air$covered)

  by_risk_band <- mesh_air[, .(n_mesh = .N, n_covered = sum(covered)), by = risk_band]
  by_risk_band[, pct_covered := round(100 * n_covered / n_mesh, 1)]
  by_risk_band[, wtg_id := wtg_id]

  ## Cruzamento altura x distancia -- so' calculado se dist_band existir
  ## (dist_band_breaks foi dado a build_terrain_mesh()/.build_mesh_from_terrain());
  ## NULL mantem o comportamento antigo para quem nao pediu esta classificacao
  ## (ex: IDF_analysis.R/BSH/DGY, que ainda nao usa esta regra).
  by_risk_dist_band <- if (has_dist_band) {
    tmp <- mesh_air[, .(n_mesh = .N, n_covered = sum(covered)), by = .(risk_band, dist_band)]
    tmp[, pct_covered := round(100 * n_covered / n_mesh, 1)]
    tmp[, wtg_id := wtg_id]
    tmp[]
  } else {
    NULL
  }

  metrics <- data.table::data.table(
    wtg_id      = wtg_id,
    n_records   = nrow(track_wtg),
    n_valid     = nrow(track_wtg_valid),
    n_air_mesh  = n_total,
    n_covered   = n_cov,
    pct_covered = round(100 * n_cov / n_total, 1),
    n_low_terrain_mesh      = n_low,
    n_low_terrain_covered   = n_low_cov,
    pct_low_terrain_covered = if (n_low == 0L) NA_real_ else round(100 * n_low_cov / n_low, 1),
    low_sample  = nrow(track_wtg) < min_sample_records
  )

  list(wtg_id = wtg_id, mesh_air = mesh_air, track_wtg = track_wtg_valid,
       metrics = metrics, by_risk_band = by_risk_band[], by_risk_dist_band = by_risk_dist_band)
}


## 3. Corre a analise completa (malha + cobertura) para todas as turbinas de um shapefile wtg ----
##    Devolve uma lista nomeada por wtg_id, cada uma com $terrain_mesh e $coverage
##    -- guarda os objetos completos para se poder desenhar o plot 3D de cada
##    turbina a posteriori, sem repetir o calculo da malha/DEM.

run_coverage_3d_all_turbines <- function(wtg_sf, track_dt, dem_file,
                                         radius, cyl_height, step_xy, step_z,
                                         prox_thresh_m,
                                         risk_band_breaks = c(200),
                                         risk_band_labels = c("at risk", "above"),
                                         dist_band_breaks = NULL,
                                         dist_band_labels = c("inner", "outer"),
                                         wtg_id_col = "InternalNa",
                                         track_dist_buffer_m = 200,
                                         wtg_sel = NULL,
                                         min_sample_records = 500000,
                                         low_terrain_levels = 2L) {

  # wtg_sel: subconjunto de nomes de turbina (coluna wtg_id_col) a analisar --
  # analise 3D completa (DEM + malha + KD-tree) e cara, por isso NULL = todas
  # as turbinas do shapefile so deve ser usado deliberadamente. wtg_sel = "all"
  # e equivalente a NULL (mais explicito no userSettings do que deixar em branco).
  # Nomes que nao existirem no shapefile sao avisados explicitamente, nao
  # ignorados em silencio.
  if (identical(wtg_sel, "all")) wtg_sel <- NULL

  if (!is.null(wtg_sel)) {
    missing_wtg <- setdiff(wtg_sel, wtg_sf[[wtg_id_col]])
    if (length(missing_wtg) > 0) {
      warning(sprintf(
        "wtg_sel: %d nome(s) nao encontrados no shapefile (coluna '%s'): %s",
        length(missing_wtg), wtg_id_col, paste(missing_wtg, collapse = ", ")
      ))
    }
    wtg_sf <- wtg_sf[wtg_sf[[wtg_id_col]] %in% wtg_sel, ]
  }

  wtg_wgs84 <- sf::st_transform(wtg_sf, 4326)
  coords    <- sf::st_coordinates(wtg_wgs84)
  coords_utm <- sf::st_coordinates(wtg_sf) # assume-se wtg_sf ja na mesma projecao planar que track_dt$utm_x/utm_y

  wtg_list <- data.table::data.table(
    wtg_id    = wtg_sf[[wtg_id_col]],
    wtg_lon   = coords[, "X"],
    wtg_lat   = coords[, "Y"],
    wtg_utm_x = coords_utm[, "X"],
    wtg_utm_y = coords_utm[, "Y"]
  )

  results <- lapply(seq_len(nrow(wtg_list)), function(i) {

    wtg_id <- wtg_list$wtg_id[i]

    # candidatos por distancia real (UTM) a esta turbina -- NAO usa a
    # classificacao "NearestTurbine3d" do IdentiFlight (coluna `turbine`),
    # que pode excluir pontos geometricamente dentro do raio de analise se
    # o IdentiFlight considerou outra turbina vizinha mais proxima. O corte
    # exato final continua a ser feito dentro de compute_mesh_coverage()
    # (projecao local centrada na turbina, x^2+y^2 <= radius^2).
    dist_to_wtg <- sqrt(
      (track_dt$utm_x - wtg_list$wtg_utm_x[i])^2 +
        (track_dt$utm_y - wtg_list$wtg_utm_y[i])^2
    )
    track_wtg <- track_dt[dist_to_wtg <= radius + track_dist_buffer_m]

    terrain_mesh <- build_terrain_mesh(
      wtg_id, wtg_list$wtg_lat[i], wtg_list$wtg_lon[i], dem_file,
      radius, cyl_height, step_xy, step_z, risk_band_breaks, risk_band_labels,
      dist_band_breaks, dist_band_labels
    )
    coverage <- compute_mesh_coverage(terrain_mesh, track_wtg, radius, cyl_height, prox_thresh_m, min_sample_records, low_terrain_levels)

    list(terrain_mesh = terrain_mesh, coverage = coverage)
  })

  names(results) <- wtg_list$wtg_id
  results
}


## 4. Resumo de cobertura, todas as turbinas ----

summarise_mesh_coverage <- function(coverage_list) {

  by_turbine <- data.table::rbindlist(lapply(coverage_list, `[[`, "metrics"))
  data.table::setorder(by_turbine, pct_covered)

  by_turbine_risk_band <- data.table::rbindlist(lapply(coverage_list, `[[`, "by_risk_band"))

  ## Cruzamento altura x distancia -- so' presente se dist_band_breaks foi
  ## usado (ver run_coverage_3d_all_turbines()); rbindlist(list(NULL, NULL))
  ## devolve um data.table vazio, nao erro, quando nenhuma turbina tem esta
  ## classificacao.
  by_turbine_risk_dist_band <- data.table::rbindlist(lapply(coverage_list, `[[`, "by_risk_dist_band"))

  list(
    by_turbine = by_turbine[], by_turbine_risk_band = by_turbine_risk_band[],
    by_turbine_risk_dist_band = by_turbine_risk_dist_band[]
  )
}


## 4b. Resumo do quadrante prioritario (altura de risco x banda interior) ----

## Filtra by_turbine_risk_dist_band (ver summarise_mesh_coverage()) para a
## combinacao de maior prioridade -- banda de altura de risco x banda de
## distancia interior -- e classifica cada turbina em 2 classes de cobertura
## (< coverage_cutoff% / >= coverage_cutoff%). risk_label/dist_label tem de
## corresponder aos labels usados em risk_band_labels/dist_band_labels na
## chamada a run_coverage_3d_all_turbines() (omissao: os labels usados em
## IDF_analysis.R para BSH/DGY).
summarise_priority_quadrant_coverage <- function(by_turbine_risk_dist_band,
                                                  risk_label = "at risk", dist_label = "inner",
                                                  coverage_cutoff = 90) {

  if (is.null(by_turbine_risk_dist_band) || nrow(by_turbine_risk_dist_band) == 0L) {
    return(list(by_turbine = NULL, class_summary = NULL))
  }

  by_turbine <- by_turbine_risk_dist_band[risk_band == risk_label & dist_band == dist_label]
  if (nrow(by_turbine) == 0L) {
    return(list(by_turbine = NULL, class_summary = NULL))
  }

  levels_lab <- c(sprintf("<%d%%", coverage_cutoff), sprintf(">=%d%%", coverage_cutoff))
  by_turbine[, coverage_class := factor(
    data.table::fifelse(pct_covered < coverage_cutoff, levels_lab[1], levels_lab[2]),
    levels = levels_lab
  )]

  class_summary <- by_turbine[, .(n_units = .N), by = coverage_class]
  missing <- setdiff(levels_lab, as.character(class_summary$coverage_class))
  if (length(missing) > 0) {
    class_summary <- data.table::rbindlist(list(
      class_summary,
      data.table::data.table(coverage_class = missing, n_units = 0L)
    ))
  }
  class_summary[, coverage_class := factor(coverage_class, levels = levels_lab)]
  data.table::setorder(class_summary, coverage_class)
  class_summary[, pct_units := round(100 * n_units / sum(n_units), 1)]

  list(by_turbine = by_turbine[], class_summary = class_summary[])
}


## -- Helpers internos para os plots 3D (Plotly) ----

## Camara orientada por um azimute (bearing), em graus
.camera_from_bearing <- function(bearing_deg = 135, distance = 2.5, z = 0.7) {
  theta <- bearing_deg * pi / 180
  list(
    eye = list(x = distance * sin(theta), y = distance * cos(theta), z = z),
    center = list(x = 0, y = 0, z = 0),
    up = list(x = 0, y = 0, z = 1),
    projection = list(type = "orthographic")
  )
}

## Linha de referencia N-S/E-W... consoante o bearing, com etiquetas de direcao
.make_bearing_line <- function(bearing_from_deg = 135, radius = 1000, z = 980) {

  b_from <- bearing_from_deg %% 360
  b_to   <- (b_from + 180) %% 360

  bearing_to_xy <- function(bearing_deg, radius) {
    theta <- bearing_deg * pi / 180
    c(x = radius * sin(theta), y = radius * cos(theta))
  }
  bearing_label <- function(bearing_deg) {
    dirs <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")
    dirs[round((bearing_deg %% 360) / 45) + 1]
  }

  p_from <- bearing_to_xy(b_from, radius)
  p_to   <- bearing_to_xy(b_to, radius)

  data.table::data.table(
    x = c(p_from["x"], p_to["x"]), y = c(p_from["y"], p_to["y"]), z = c(z, z),
    bearing = c(b_from, b_to), label = c(bearing_label(b_from), bearing_label(b_to))
  )
}

## Malha triangulada (mesh3d) do cilindro fronteira -- em vez de superficie
## preenchida (add_surface): uma superficie semitransparente que ENVOLVE
## outras traces (terreno, marcadores) pode continuar a escrever no
## depth-buffer do WebGL e escondê-las por completo, mesmo com opacidade
## baixa (bug conhecido do plotly.js com add_surface). mesh3d nao tem esse
## problema, e mantem o aspeto de parede solida semitransparente.
.build_cylinder_mesh3d <- function(radius, z_min, z_max, n_theta = 48) {

  theta <- seq(0, 2 * pi, length.out = n_theta + 1)[-(n_theta + 1)]

  x_bottom <- radius * cos(theta); y_bottom <- radius * sin(theta); z_bottom <- rep(z_min, n_theta)
  x_top    <- radius * cos(theta); y_top    <- radius * sin(theta); z_top    <- rep(z_max, n_theta)

  i_idx <- integer(2 * n_theta); j_idx <- integer(2 * n_theta); k_idx <- integer(2 * n_theta)
  for (idx in 0:(n_theta - 1)) {
    nxt <- (idx + 1) %% n_theta
    b_i <- idx; b_nxt <- nxt
    t_i <- idx + n_theta; t_nxt <- nxt + n_theta
    slot <- 2 * idx
    i_idx[slot + 1] <- b_i; j_idx[slot + 1] <- b_nxt; k_idx[slot + 1] <- t_i
    i_idx[slot + 2] <- t_i; j_idx[slot + 2] <- b_nxt; k_idx[slot + 2] <- t_nxt
  }

  list(
    x = c(x_bottom, x_top), y = c(y_bottom, y_top), z = c(z_bottom, z_top),
    i = i_idx, j = j_idx, k = k_idx
  )
}


## Superficies/linhas partilhadas pelos 2 plots (terreno + cilindro fronteira) --
## mesh_z_values: valores de z (m, relativos a turbina) a considerar no
## calculo do limite inferior do cilindro (ex: mesh_cov$z_rel_turbine)
.build_plot_surfaces <- function(terrain_mesh, mesh_z_values, radius, cyl_height, step_z, z_pad_lower, z_pad_upper = 50) {

  mesh_xy  <- terrain_mesh$mesh_xy
  wtg_elev <- terrain_mesh$wtg_elev

  terrain_surface <- data.table::dcast(mesh_xy, y ~ x, value.var = "terrain_elev")
  ys_surf <- terrain_surface$y
  xs_surf <- as.numeric(names(terrain_surface)[-1])

  Zterrain_abs <- as.matrix(terrain_surface[, -1, with = FALSE])
  Zterrain_rel <- Zterrain_abs - wtg_elev

  # z_pad_upper: margem acima de cyl_height para a bearing_line/labels ficarem
  # visiveis sem se misturarem com os pontos da malha -- tem de ficar dentro
  # do range do eixo (zaxis usa este z_max_cyl), senao volta o problema do
  # plotly a nao renderizar nada quando ha dados fora do range com autorange=FALSE
  z_min_cyl <- floor(min(Zterrain_rel, mesh_z_values, na.rm = TRUE) / step_z) * step_z - z_pad_lower
  z_max_cyl <- cyl_height + z_pad_upper

  list(
    xs_surf = xs_surf, ys_surf = ys_surf, Zterrain_rel = Zterrain_rel,
    cyl_mesh = .build_cylinder_mesh3d(radius, z_min_cyl, z_max_cyl),
    z_min_cyl = z_min_cyl, z_max_cyl = z_max_cyl
  )
}


## Caixa de texto (dentro do titulo) com tamanho de amostra e % de cobertura
## por banda de risco -- usada nos 2 plots 3D (coverage + debug) para nao
## depender so da % agregada do titulo antigo.
.coverage_title_text <- function(wtg_id, metrics, by_risk_band) {

  sample_note <- if (isTRUE(metrics$low_sample)) " -- AMOSTRA BAIXA, interpretar com cautela" else ""

  risk_lines <- paste(
    sprintf("%s: %.1f%%", by_risk_band$risk_band, by_risk_band$pct_covered),
    collapse = "<br>"
  )

  sprintf(
    "WTG: %s<br>Sample size: %d track records (%d valid)%s<br>Covered air mesh: %d / %d (%.1f%%)<br>%s",
    wtg_id, metrics$n_records, metrics$n_valid, sample_note,
    metrics$n_covered, metrics$n_air_mesh, metrics$pct_covered,
    risk_lines
  )
}


## Mesma caixa de texto, mas para o plot "debug"/not-covered -- apanhado
## pelo Paulo, 2026-10: esse plot mostra os pontos NAO cobertos, mas
## reutilizava tal e qual o texto do plot covered (cobertura, nao
## indisponibilidade) -- confuso, já que os numeros (cobertura) nao
## batiam certo com o que o plot em si mostra (os pontos SEM cobertura).
## Sample size fica igual (nao e' especifico de cobertura); os restantes 3
## campos (mesh total, overall, por banda de risco) invertidos para
## "uncovered" (100% - pct_covered / n_air_mesh - n_covered).
.coverage_title_text_uncovered <- function(wtg_id, metrics, by_risk_band) {

  sample_note <- if (isTRUE(metrics$low_sample)) " -- AMOSTRA BAIXA, interpretar com cautela" else ""

  n_not_covered   <- metrics$n_air_mesh - metrics$n_covered
  pct_not_covered <- 100 - metrics$pct_covered

  risk_lines <- paste(
    sprintf("Uncovered %s: %.1f%%", by_risk_band$risk_band, 100 - by_risk_band$pct_covered),
    collapse = "<br>"
  )

  sprintf(
    "WTG: %s<br>Sample size: %d track records (%d valid)%s<br>Uncovered air mesh: %d / %d (%.1f%%)<br>%s",
    wtg_id, metrics$n_records, metrics$n_valid, sample_note,
    n_not_covered, metrics$n_air_mesh, pct_not_covered,
    risk_lines
  )
}


## 4b. Posicao das unidades IDF dentro de um raio de interesse, no
## referencial LOCAL (AEQD centrado na turbina) de um terrain_mesh -- para
## marcar essas unidades nos 2 plots 3D (coverage + debug), pedido do
## Paulo (2026-10): "a representacao da torre da unidade [IDF], com 10m de
## altura", restrita as unidades dentro do limite de deteção (omissao:
## 1000m, idf_op_detection_range) -- NAO o raio mais largo do proprio
## cilindro/mesh (coverage_cylinder_wider_radius, tipicamente 1100m), que e'
## so' a margem de desenho, nao um limite operacional.
##
## idf_sf: shapefile de unidades IDF (coluna idf_id_col com o rotulo
## "IDF<NN>" ja' normalizado, mesma convencao do resto do projeto) --
## qualquer CRS, e' reprojetado aqui para crs_local. crs_local:
## terrain_mesh$crs_local (AEQD, origem = a propria turbina -- ver
## build_terrain_mesh()), por isso x/y saem directamente no mesmo
## referencial "X/Y to WTG (m)" dos 2 plots, sem calculo extra do lado do
## chamador.
idf_units_in_local_frame <- function(idf_sf, crs_local, max_dist_m = 1000, idf_id_col = "imaging_he") {

  idf_local <- sf::st_transform(idf_sf, crs_local)
  coords <- sf::st_coordinates(idf_local)

  out <- data.table::data.table(
    idf = idf_local[[idf_id_col]],
    x   = coords[, "X"],
    y   = coords[, "Y"]
  )
  out[, dist := sqrt(x^2 + y^2)]
  out[dist <= max_dist_m][]
}


## 4c. Elementos estruturais comuns aos 2 plots 3D (torre+nacelle da
## turbina, seta+rotulos de orientacao, torres das unidades IDF dentro do
## raio de interesse) -- extraido para deixar de duplicar parcialmente esta
## logica entre plot_mesh_coverage_3d() (cobertura) e
## plot_mesh_coverage_debug() (inverso/debug): ate agora cada um so' tinha
## um subconjunto destes elementos (seta+rotulos so' no 1º, torre+nacelle
## so' no 2º) -- pedido do Paulo, 2026-10, para os 2 ficarem simetricos.
##
## idf_units_local: data.table (idf, x, y[, dist]) ja' no referencial local
## da turbina (ver idf_units_in_local_frame() acima) -- NULL/0 linhas
## desliga esta camada (comportamento antigo, sem unidades IDF marcadas).
## As torres das unidades IDF usam quebras NA entre segmentos (1 unica
## trace "lines", em vez de 1 trace por unidade) -- tecnica padrao do
## plotly para desenhar varios segmentos DESLIGADOS entre si numa so' trace.
.add_structure_traces <- function(p, wtg_tower_height, bearing_line, bearing_labels,
                                  idf_units_local = NULL, idf_tower_height = 10) {

  wtg_line    <- data.table::data.table(x = c(0, 0), y = c(0, 0), z = c(0, wtg_tower_height))
  wtg_nacelle <- data.table::data.table(x = 0, y = 0, z = wtg_tower_height)

  p <- p %>%
    plotly::add_trace(
      data = wtg_line, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "lines",
      line = list(color = "red", width = 10), name = "WTG tower"
    ) %>%
    plotly::add_markers(
      data = wtg_nacelle, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "markers",
      marker = list(size = 5, color = "darkred"), name = "WTG nacelle"
    ) %>%
    plotly::add_trace(
      data = bearing_line, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "lines",
      line = list(color = "black", width = 4),
      name = paste0("View axis ", bearing_line$label[1], "->", bearing_line$label[2]),
      showlegend = FALSE
    ) %>%
    plotly::add_trace(
      data = bearing_labels, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "text",
      text = ~label, textposition = "middle center", textfont = list(size = 16, color = "black"),
      showlegend = FALSE, hoverinfo = "skip"
    )

  if (!is.null(idf_units_local) && nrow(idf_units_local) > 0) {

    idf_lines <- data.table::rbindlist(lapply(seq_len(nrow(idf_units_local)), function(i) {
      u <- idf_units_local[i]
      data.table::data.table(x = c(u$x, u$x, NA_real_), y = c(u$y, u$y, NA_real_), z = c(0, idf_tower_height, NA_real_))
    }))

    idf_labels <- data.table::copy(idf_units_local)
    idf_labels[, z := idf_tower_height + 15]

    p <- p %>%
      plotly::add_trace(
        data = idf_lines, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "lines",
        line = list(color = "blue", width = 8),
        name = "IDF unit tower", showlegend = TRUE
      ) %>%
      plotly::add_trace(
        data = idf_labels, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "text",
        text = ~idf, textposition = "top center", textfont = list(size = 12, color = "blue"),
        showlegend = FALSE, hoverinfo = "text"
      )
  }

  p
}


## 5. Plot 3D (Plotly) da malha "air" coberta, terreno e cilindro, para UMA turbina ----

plot_mesh_coverage_3d <- function(terrain_mesh, coverage, radius, cyl_height,
                                  step_z = 50, bearing_deg = 135, z_pad_lower = 50,
                                  wtg_tower_height = 90, idf_units_local = NULL) {

  wtg_id  <- terrain_mesh$wtg_id
  metrics <- coverage$metrics

  mesh_cov <- coverage$mesh_air[covered == TRUE]
  mesh_cov[, z_plot := z_rel_turbine]

  surf <- .build_plot_surfaces(terrain_mesh, mesh_cov$z_rel_turbine, radius, cyl_height, step_z, z_pad_lower)

  # bearing_line/labels ficam ACIMA de cyl_height (dentro da margem
  # z_pad_upper de .build_plot_surfaces()) para nao se misturarem com os
  # pontos da malha coberta, que vao no maximo ate cyl_height
  bearing_line <- .make_bearing_line(bearing_from_deg = bearing_deg, radius = radius, z = cyl_height + 30)
  bearing_labels <- data.table::copy(bearing_line)
  bearing_labels[, `:=`(x = x * 0.90, y = y * 0.90, z = z - 15)]

  plot_title <- sprintf(
    "Terrain-corrected WTG mesh coverage<br>%s",
    .coverage_title_text(wtg_id, metrics, coverage$by_risk_band)
  )

  # usar o range do proprio cilindro (nao so do terreno) -- o cilindro e a
  # bearing_line estendem-se para alem do que so o terreno cobre; com
  # autorange=FALSE, dados fora do range declarado podem fazer a cena 3D
  # nao renderizar nada
  z_min <- surf$z_min_cyl
  z_max <- surf$z_max_cyl

  p <- plotly::plot_ly() %>%
    plotly::add_surface(
      x = surf$xs_surf, y = surf$ys_surf, z = surf$Zterrain_rel, opacity = 0.95, showscale = FALSE,
      colorscale = "Viridis",
      name = "Terrain"
    ) %>%
    plotly::add_markers(
      data = mesh_cov, x = ~x, y = ~y, z = ~z_plot, type = "scatter3d", mode = "markers",
      color = ~risk_band, marker = list(size = 1.2, opacity = 0.85), name = ~risk_band
    ) %>%
    plotly::add_trace(
      x = surf$cyl_mesh$x, y = surf$cyl_mesh$y, z = surf$cyl_mesh$z,
      i = surf$cyl_mesh$i, j = surf$cyl_mesh$j, k = surf$cyl_mesh$k,
      type = "mesh3d", opacity = 0.02, color = I("grey"),
      name = paste0(radius, " m cylinder boundary"), showlegend = FALSE, hoverinfo = "skip"
    )

  p <- .add_structure_traces(p, wtg_tower_height, bearing_line, bearing_labels, idf_units_local)

  p %>%
    plotly::layout(
      title = list(text = plot_title, x = 0.05, y = 0.95, font = list(size = 12)),
      legend = list(x = 0.02, y = 0.85, xanchor = "left", yanchor = "top",
                    bgcolor = "rgba(255,255,255,0.65)", bordercolor = "rgba(0,0,0,0.2)", borderwidth = 1),
      margin = list(l = 20, r = 20, t = 70, b = 20),
      scene = list(
        xaxis = list(title = "X to WTG (m)", range = c(-radius, radius), autorange = FALSE),
        yaxis = list(title = "Y to WTG (m)", range = c(-radius, radius), autorange = FALSE),
        zaxis = list(title = "Height relative to WTG ground (m)", range = c(z_min, z_max), autorange = FALSE),
        aspectmode = "manual",
        aspectratio = list(x = 1, y = 1, z = (z_max - z_min) / (2 * radius)),
        camera = .camera_from_bearing(bearing_deg = bearing_deg, distance = 2.5, z = 0.7)
      )
    )
}


## 6. Plot 3D "debug" -- pontos da malha "air" NAO cobertos + torre WTG ----

plot_mesh_coverage_debug <- function(terrain_mesh, coverage, radius, cyl_height,
                                     step_z = 50, bearing_deg = 135, z_pad_lower = 50,
                                     wtg_tower_height = 90, idf_units_local = NULL) {

  wtg_id  <- terrain_mesh$wtg_id
  metrics <- coverage$metrics

  mesh_not_cov <- coverage$mesh_air[covered == FALSE]
  mesh_not_cov[, z_plot := z_rel_turbine]

  surf <- .build_plot_surfaces(terrain_mesh, coverage$mesh_air$z_rel_turbine, radius, cyl_height, step_z, z_pad_lower)

  # bearing_line/labels -- mesma logica/posicionamento de plot_mesh_coverage_3d()
  # (secção 5 acima), extraida para .add_structure_traces() -- em falta aqui
  # ate 2026-10 (apanhado pelo Paulo): este plot "debug"/not-covered nunca
  # teve a seta/rotulos de orientacao, so' o plot "coverage" os tinha.
  bearing_line <- .make_bearing_line(bearing_from_deg = bearing_deg, radius = radius, z = cyl_height + 30)
  bearing_labels <- data.table::copy(bearing_line)
  bearing_labels[, `:=`(x = x * 0.90, y = y * 0.90, z = z - 15)]

  p <- plotly::plot_ly() %>%
    plotly::add_surface(
      x = surf$xs_surf, y = surf$ys_surf, z = surf$Zterrain_rel, opacity = 0.95, showscale = FALSE,
      colorscale = "Viridis",
      name = "Terrain"
    ) %>%
    plotly::add_markers(
      data = mesh_not_cov, x = ~x, y = ~y, z = ~z_plot, type = "scatter3d", mode = "markers",
      marker = list(size = 1.5, opacity = 0.7, color = "orange"),
      name = "Mesh not covered"
    ) %>%
    plotly::add_trace(
      x = surf$cyl_mesh$x, y = surf$cyl_mesh$y, z = surf$cyl_mesh$z,
      i = surf$cyl_mesh$i, j = surf$cyl_mesh$j, k = surf$cyl_mesh$k,
      type = "mesh3d", opacity = 0.03, color = I("grey"), hoverinfo = "skip",
      name = "Cylinder boundary", showlegend = FALSE
    )

  p <- .add_structure_traces(p, wtg_tower_height, bearing_line, bearing_labels, idf_units_local)

  p %>%
    plotly::layout(
      title = list(
        text = sprintf("Debug View - full air mesh and uncovered points<br>%s",
                       .coverage_title_text_uncovered(wtg_id, metrics, coverage$by_risk_band)),
        x = 0.05, y = 0.95, font = list(size = 12)
      ),
      legend = list(x = 0.01, y = 0.85, xanchor = "left", yanchor = "top",
                    bgcolor = "rgba(255,255,255,0.65)", bordercolor = "rgba(0,0,0,0.2)", borderwidth = 1),
      scene = list(
        xaxis = list(title = "X to WTG (m)", range = c(-radius, radius), autorange = FALSE),
        yaxis = list(title = "Y to WTG (m)", range = c(-radius, radius), autorange = FALSE),
        zaxis = list(title = "Height relative to WTG ground (m)", range = c(surf$z_min_cyl, surf$z_max_cyl), autorange = FALSE),
        aspectmode = "manual",
        aspectratio = list(x = 1, y = 1, z = (surf$z_max_cyl - surf$z_min_cyl) / (2 * radius)),
        camera = .camera_from_bearing(bearing_deg = bearing_deg, distance = 2.5, z = 0.7)
      )
    )
}


## 7. Guarda os plots 3D (cobertura + inverso) de cada turbina, em HTML autonomo ----
##    cov_all: resultado de run_coverage_3d_all_turbines()
##
## screenshot = TRUE (por omissao FALSE, para nao alterar o comportamento
## dos relatorios existentes que ja chamam esta funcao): usa webshot2
## (Chrome/Edge headless) para gravar tambem uma versao .png estatica de
## cada plot, ao lado do .html interativo -- pedido do Paulo, 2026-08, para
## poder embeber uma imagem da cobertura 3D no .docx do relatorio de
## incidente (Word nao suporta plotly interativo). Se webshot2 nao
## estiver instalado, ou nao encontrar um Chrome/Edge no sistema, avisa e
## continua sem PNG (o HTML interativo fica sempre disponivel de qualquer forma).
## Devolve, para cada turbina, os caminhos dos PNG gravados (NULL se a
## captura falhou/nao foi pedida).
##
## idf_sf (opcional, omissao NULL -- comportamento antigo, sem unidades IDF
## marcadas): shapefile de unidades IDF, para marcar nos 2 plots (coverage +
## debug) de CADA turbina as unidades dentro de idf_max_dist_m (omissao
## 1000m, idf_op_detection_range) -- pedido do Paulo, 2026-10, "a
## representacao da torre da unidade [IDF], com 10m de altura". Recalculado
## por turbina (idf_units_in_local_frame(), secção 4b acima), ja que cada
## terrain_mesh tem o seu proprio referencial local (crs_local, AEQD
## centrado nessa turbina).

save_coverage_3d_plots <- function(cov_all, folder_out, radius, cyl_height,
                                   screenshot = FALSE, screenshot_width = 1200,
                                   screenshot_height = 900, screenshot_delay = 2,
                                   idf_sf = NULL, idf_max_dist_m = 1000, idf_id_col = "imaging_he",
                                   file_suffix = "") {

  # file_suffix: acrescentado ao nome de cada ficheiro, antes da extensao
  # (ex: "_ZRF_T94_20261001_20261007", ver R/output_paths.R); "" mantem os
  # nomes antigos para quem nao o usa (IDF_analysis.R).
  dir.create(folder_out, showWarnings = FALSE, recursive = TRUE)

  take_screenshot <- function(html_path, png_path) {
    tryCatch({
      webshot2::webshot(
        html_path, png_path,
        vwidth = screenshot_width, vheight = screenshot_height, delay = screenshot_delay
      )
      png_path
    }, error = function(e) {
      message(sprintf(
        "Aviso: nao foi possivel gerar screenshot de %s (%s) -- precisa do pacote webshot2 e de um Chrome/Edge instalado. So o HTML interativo fica disponivel.",
        html_path, conditionMessage(e)
      ))
      NULL
    })
  }

  png_paths <- list()

  for (wtg_id in names(cov_all)) {

    terrain_mesh_i <- cov_all[[wtg_id]]$terrain_mesh
    coverage_i     <- cov_all[[wtg_id]]$coverage

    idf_units_local_i <- if (!is.null(idf_sf)) {
      idf_units_in_local_frame(idf_sf, terrain_mesh_i$crs_local, max_dist_m = idf_max_dist_m, idf_id_col = idf_id_col)
    } else NULL

    html_cov <- file.path(folder_out, paste0("coverage_3d_", wtg_id, file_suffix, ".html"))
    p_cov <- plot_mesh_coverage_3d(terrain_mesh_i, coverage_i, radius = radius, cyl_height = cyl_height, idf_units_local = idf_units_local_i)
    htmlwidgets::saveWidget(p_cov, html_cov, selfcontained = TRUE)

    html_notcov <- file.path(folder_out, paste0("coverage_3d_not_covered_", wtg_id, file_suffix, ".html"))
    p_notcov <- plot_mesh_coverage_debug(terrain_mesh_i, coverage_i, radius = radius, cyl_height = cyl_height, idf_units_local = idf_units_local_i)
    htmlwidgets::saveWidget(p_notcov, html_notcov, selfcontained = TRUE)

    if (isTRUE(screenshot)) {
      png_paths[[wtg_id]] <- list(
        covered     = take_screenshot(html_cov, file.path(folder_out, paste0("coverage_3d_", wtg_id, file_suffix, ".png"))),
        not_covered = take_screenshot(html_notcov, file.path(folder_out, paste0("coverage_3d_not_covered_", wtg_id, file_suffix, ".png")))
      )
    }
  }

  invisible(png_paths)
}


## 8. Atalho para visualizar o plot de UMA turbina a partir de cov_all ----
##    Util quando a analise correu para varias turbinas (run_coverage_3d_all_turbines())
##    mas so queremos ver/inspecionar interativamente uma de cada vez no Viewer.
##    not_covered = TRUE mostra o inverso (pontos da malha "air" sem deteções).

plot_coverage_3d_for_turbine <- function(cov_all, wtg_id, radius, cyl_height, not_covered = FALSE) {

  if (!wtg_id %in% names(cov_all)) {
    stop(sprintf(
      "Turbina '%s' nao encontrada em cov_all. Disponiveis: %s",
      wtg_id, paste(names(cov_all), collapse = ", ")
    ))
  }

  terrain_mesh_i <- cov_all[[wtg_id]]$terrain_mesh
  coverage_i     <- cov_all[[wtg_id]]$coverage

  if (not_covered) {
    plot_mesh_coverage_debug(terrain_mesh_i, coverage_i, radius = radius, cyl_height = cyl_height)
  } else {
    plot_mesh_coverage_3d(terrain_mesh_i, coverage_i, radius = radius, cyl_height = cyl_height)
  }
}
