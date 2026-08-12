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
## Depende de: data.table, sf, terra, RANN, plotly
##
## Uso:
##   source("R/coverage_3d_topography.R")
##   dem_file <- file.path(databases_dir, dem_filename)
##   cov_all  <- run_coverage_3d_all_turbines(
##     wtg, track_dt, dem_file,
##     radius = coverage_cylinder_wider_radius, cyl_height = coverage_cylinder_height,
##     step_xy = coverage_mesh_step_xy, step_z = coverage_mesh_step_z,
##     prox_thresh_m = coverage_prox_thresh_m,
##     risk_band_breaks = c(200), risk_band_labels = c("at risk", "above")
##   )
##   summary_cov <- summarise_mesh_coverage(lapply(cov_all, `[[`, "coverage"))
##   plot_mesh_coverage_3d(cov_all$BSH54$terrain_mesh, cov_all$BSH54$coverage,
##                        radius = coverage_cylinder_wider_radius, cyl_height = coverage_cylinder_height)
##


## 1. Malha 3D corrigida ao terreno, para UMA turbina ----

build_terrain_mesh <- function(wtg_id, wtg_lat, wtg_lon, dem_file,
                               radius, cyl_height, step_xy, step_z,
                               risk_band_breaks = c(200),
                               risk_band_labels = c("at risk", "above")) {

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

  ## Malha x/y dentro do raio, z de 0 a cyl_height
  xs <- seq(-radius, radius, by = step_xy)
  ys <- seq(-radius, radius, by = step_xy)
  zs <- seq(0, cyl_height, by = step_z)

  grid_xy <- data.table::CJ(x = xs, y = ys)
  grid_xy <- grid_xy[(x^2 + y^2) <= radius^2]

  ## Elevacao do terreno em cada no x/y (para classificar a malha e desenhar a superficie)
  mesh_xy    <- data.table::copy(grid_xy)
  mesh_xy_sf <- sf::st_as_sf(mesh_xy, coords = c("x", "y"), crs = crs_local)

  terrain_vals <- terra::extract(dem_local, terra::vect(mesh_xy_sf))
  mesh_xy[, terrain_elev := as.numeric(terrain_vals[[2]])]

  mesh <- grid_xy[, .(z_rel_turbine = zs), by = .(x, y)]
  mesh[, z_abs := wtg_elev + z_rel_turbine]
  mesh <- merge(mesh, mesh_xy, by = c("x", "y"), all.x = TRUE)

  mesh[, vertical_clearance := z_abs - terrain_elev]
  mesh[, medium := data.table::fifelse(
    is.na(terrain_elev), NA_character_,
    data.table::fifelse(vertical_clearance <= 0, "terrain", "air")
  )]

  mesh[, risk_band := cut(
    z_rel_turbine,
    breaks = c(0, risk_band_breaks, cyl_height),
    labels = risk_band_labels,
    include.lowest = TRUE, right = FALSE
  )]

  list(
    wtg_id       = wtg_id,
    crs_local    = crs_local,
    wtg_elev     = wtg_elev,
    dem_local    = dem_local,
    mesh_air     = mesh[medium == "air" & !is.na(risk_band)],
    mesh_terrain = mesh[medium == "terrain"],
    mesh_xy      = mesh_xy # elevacao do terreno por no x/y -- usado no plot da superficie
  )
}


## 2. Cobertura da malha "air" pelas deteções de aves, para UMA turbina ----
##    track_wtg: subconjunto de track_dt para esta turbina (colunas lat, lon, height)

compute_mesh_coverage <- function(terrain_mesh, track_wtg, radius, cyl_height, prox_thresh_m) {

  wtg_id    <- terrain_mesh$wtg_id
  mesh_air  <- data.table::copy(terrain_mesh$mesh_air)
  crs_local <- terrain_mesh$crs_local
  wtg_elev  <- terrain_mesh$wtg_elev
  dem_local <- terrain_mesh$dem_local

  empty_result <- function(n_records, track_wtg_valid) {
    mesh_air[, `:=`(hits = 0L, covered = FALSE)]
    list(
      wtg_id = wtg_id, mesh_air = mesh_air, track_wtg = track_wtg_valid,
      metrics = data.table::data.table(
        wtg_id = wtg_id, n_records = n_records, n_valid = nrow(track_wtg_valid),
        n_air_mesh = nrow(mesh_air), n_covered = 0L, pct_covered = NA_real_
      ),
      by_risk_band = mesh_air[, .(n_mesh = .N, n_covered = 0L, pct_covered = 0), by = risk_band][, wtg_id := wtg_id][]
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
      z_rel_turbine >= 0 & z_rel_turbine <= cyl_height
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

  by_risk_band <- mesh_air[, .(n_mesh = .N, n_covered = sum(covered)), by = risk_band]
  by_risk_band[, pct_covered := round(100 * n_covered / n_mesh, 1)]
  by_risk_band[, wtg_id := wtg_id]

  metrics <- data.table::data.table(
    wtg_id      = wtg_id,
    n_records   = nrow(track_wtg),
    n_valid     = nrow(track_wtg_valid),
    n_air_mesh  = n_total,
    n_covered   = n_cov,
    pct_covered = round(100 * n_cov / n_total, 1)
  )

  list(wtg_id = wtg_id, mesh_air = mesh_air, track_wtg = track_wtg_valid,
       metrics = metrics, by_risk_band = by_risk_band[])
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
                                         wtg_id_col = "InternalNa",
                                         track_dist_buffer_m = 200) {

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
      radius, cyl_height, step_xy, step_z, risk_band_breaks, risk_band_labels
    )
    coverage <- compute_mesh_coverage(terrain_mesh, track_wtg, radius, cyl_height, prox_thresh_m)

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

  list(by_turbine = by_turbine[], by_turbine_risk_band = by_turbine_risk_band[])
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


## 5. Plot 3D (Plotly) da malha "air" coberta, terreno e cilindro, para UMA turbina ----

plot_mesh_coverage_3d <- function(terrain_mesh, coverage, radius, cyl_height,
                                  step_z = 50, bearing_deg = 135, z_pad_lower = 50) {

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
    "Terrain-corrected WTG mesh coverage<br>WTG: %s<br>Bird records: %d<br>Covered air mesh: %d / %d (%.1f%%)",
    wtg_id, metrics$n_records, metrics$n_covered, metrics$n_air_mesh, metrics$pct_covered
  )

  # usar o range do proprio cilindro (nao so do terreno) -- o cilindro e a
  # bearing_line estendem-se para alem do que so o terreno cobre; com
  # autorange=FALSE, dados fora do range declarado podem fazer a cena 3D
  # nao renderizar nada
  z_min <- surf$z_min_cyl
  z_max <- surf$z_max_cyl

  plotly::plot_ly() %>%
    plotly::add_surface(
      x = surf$xs_surf, y = surf$ys_surf, z = surf$Zterrain_rel, opacity = 0.85, showscale = FALSE,
      colorscale = "Viridis",
      name = "Terrain"
    ) %>%
    plotly::add_markers(
      data = mesh_cov, x = ~x, y = ~y, z = ~z_plot, type = "scatter3d", mode = "markers",
      color = ~risk_band, marker = list(size = 1.2, opacity = 0.75), name = ~risk_band
    ) %>%
    plotly::add_trace(
      x = surf$cyl_mesh$x, y = surf$cyl_mesh$y, z = surf$cyl_mesh$z,
      i = surf$cyl_mesh$i, j = surf$cyl_mesh$j, k = surf$cyl_mesh$k,
      type = "mesh3d", opacity = 0.08, color = I("grey"),
      name = paste0(radius, " m cylinder boundary"), showlegend = FALSE, hoverinfo = "skip"
    ) %>%
    plotly::add_trace(
      data = bearing_line, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "lines",
      line = list(color = "black", width = 4),
      name = paste0("View axis ", bearing_line$label[1], "→", bearing_line$label[2]),
      showlegend = FALSE
    ) %>%
    plotly::add_trace(
      data = bearing_labels, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "text",
      text = ~label, textposition = "middle center", textfont = list(size = 16, color = "black"),
      showlegend = FALSE, hoverinfo = "skip"
    ) %>%
    plotly::layout(
      title = list(text = plot_title, x = 0.05, y = 0.95, font = list(size = 12)),
      legend = list(x = 0.02, y = 0.95, xanchor = "left", yanchor = "top",
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
                                     wtg_tower_height = 90) {

  wtg_id <- terrain_mesh$wtg_id

  mesh_not_cov <- coverage$mesh_air[covered == FALSE]
  mesh_not_cov[, z_plot := z_rel_turbine]

  surf <- .build_plot_surfaces(terrain_mesh, coverage$mesh_air$z_rel_turbine, radius, cyl_height, step_z, z_pad_lower)

  wtg_line    <- data.table::data.table(x = c(0, 0), y = c(0, 0), z = c(0, wtg_tower_height))
  wtg_nacelle <- data.table::data.table(x = 0, y = 0, z = wtg_tower_height)

  plotly::plot_ly() %>%
    plotly::add_surface(
      x = surf$xs_surf, y = surf$ys_surf, z = surf$Zterrain_rel, opacity = 0.7, showscale = FALSE,
      colorscale = "Viridis",
      name = "Terrain"
    ) %>%
    plotly::add_markers(
      data = mesh_not_cov, x = ~x, y = ~y, z = ~z_plot, type = "scatter3d", mode = "markers",
      marker = list(size = 1.5, opacity = 0.51, color = "orange"),
      name = "Air mesh - not covered"
    ) %>%
    plotly::add_trace(
      data = wtg_line, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "lines",
      line = list(color = "red", width = 10), name = "WTG tower"
    ) %>%
    plotly::add_markers(
      data = wtg_nacelle, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "markers",
      marker = list(size = 5, color = "darkred"), name = "WTG nacelle"
    ) %>%
    plotly::add_trace(
      x = surf$cyl_mesh$x, y = surf$cyl_mesh$y, z = surf$cyl_mesh$z,
      i = surf$cyl_mesh$i, j = surf$cyl_mesh$j, k = surf$cyl_mesh$k,
      type = "mesh3d", opacity = 0.06, color = I("grey"), hoverinfo = "skip",
      name = "Cylinder boundary", showlegend = FALSE
    ) %>%
    plotly::layout(
      title = sprintf("Debug view - full air mesh and covered points<br>WTG: %s", wtg_id),
      legend = list(x = 0.02, y = 0.95, xanchor = "left", yanchor = "top",
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
