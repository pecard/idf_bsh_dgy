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
                                         risk_band_labels = c("at risk", "above")) {

  wtg_wgs84 <- sf::st_transform(wtg_sf, 4326)
  coords    <- sf::st_coordinates(wtg_wgs84)

  wtg_list <- data.table::data.table(
    wtg_id  = wtg_wgs84$ID,
    wtg_lon = coords[, "X"],
    wtg_lat = coords[, "Y"]
  )

  results <- lapply(seq_len(nrow(wtg_list)), function(i) {

    wtg_id    <- wtg_list$wtg_id[i]
    track_wtg <- track_dt[turbine == wtg_id]

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

## Contorno do cilindro fronteira, como WIREFRAME (linhas), nao superficie
## preenchida -- uma superficie semitransparente que ENVOLVE outras traces
## (terreno, marcadores) pode continuar a escrever no depth-buffer do WebGL
## e escondê-las por completo, mesmo com opacidade baixa (bug conhecido do
## plotly.js). Linhas nao tem esse problema.
## Devolve n_ribs varais verticais + 2 aneis horizontais (base e topo),
## como um unico trace (segmentos separados por linhas de NA).
.build_cylinder_wireframe <- function(radius, z_min, z_max, n_ribs = 24) {

  theta <- seq(0, 2 * pi, length.out = n_ribs + 1)[-(n_ribs + 1)]

  na_row <- function(id) data.table::data.table(seg_id = id, x = NA_real_, y = NA_real_, z = NA_real_)

  ## varais verticais
  ribs <- data.table::rbindlist(lapply(seq_along(theta), function(i) {
    th <- theta[i]
    rbind(
      data.table::data.table(
        seg_id = i,
        x = radius * cos(th), y = radius * sin(th), z = c(z_min, z_max)
      ),
      na_row(i)
    )
  }))

  ## aneis horizontais (base e topo)
  ring_theta <- seq(0, 2 * pi, length.out = 73) # fecha o circulo (73 = 72 segmentos + repete o 1o ponto)
  rings <- data.table::rbindlist(lapply(c(z_min, z_max), function(zz) {
    rbind(
      data.table::data.table(
        seg_id = paste0("ring_", zz),
        x = radius * cos(ring_theta), y = radius * sin(ring_theta), z = zz
      ),
      na_row(paste0("ring_", zz))
    )
  }))

  rbind(ribs, rings)[, .(x, y, z)]
}


## Superficies/linhas partilhadas pelos 2 plots (terreno + cilindro fronteira) --
## mesh_z_values: valores de z (m, relativos a turbina) a considerar no
## calculo do limite inferior do cilindro (ex: mesh_cov$z_rel_turbine)
.build_plot_surfaces <- function(terrain_mesh, mesh_z_values, radius, cyl_height, step_z, z_pad_lower) {

  mesh_xy  <- terrain_mesh$mesh_xy
  wtg_elev <- terrain_mesh$wtg_elev

  terrain_surface <- data.table::dcast(mesh_xy, y ~ x, value.var = "terrain_elev")
  ys_surf <- terrain_surface$y
  xs_surf <- as.numeric(names(terrain_surface)[-1])

  Zterrain_abs <- as.matrix(terrain_surface[, -1, with = FALSE])
  Zterrain_rel <- Zterrain_abs - wtg_elev

  z_min_cyl <- floor(min(Zterrain_rel, mesh_z_values, na.rm = TRUE) / step_z) * step_z - z_pad_lower
  z_max_cyl <- cyl_height

  list(
    xs_surf = xs_surf, ys_surf = ys_surf, Zterrain_rel = Zterrain_rel,
    cyl_wire = .build_cylinder_wireframe(radius, z_min_cyl, z_max_cyl),
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

  bearing_line <- .make_bearing_line(bearing_from_deg = bearing_deg, radius = radius, z = cyl_height * 0.98)
  bearing_labels <- data.table::copy(bearing_line)
  bearing_labels[, `:=`(x = x * 0.90, y = y * 0.90, z = z - 30)]

  plot_title <- sprintf(
    "Terrain-corrected WTG mesh coverage<br>WTG: %s<br>Bird records: %d<br>Covered air mesh: %d / %d (%.1f%%)",
    wtg_id, metrics$n_records, metrics$n_covered, metrics$n_air_mesh, metrics$pct_covered
  )

  z_min <- min(surf$Zterrain_rel, na.rm = TRUE)
  z_max <- cyl_height

  plotly::plot_ly() %>%
    plotly::add_surface(
      x = surf$xs_surf, y = surf$ys_surf, z = surf$Zterrain_rel, opacity = 0.85, showscale = FALSE,
      surfacecolor = matrix("black", nrow = nrow(surf$Zterrain_rel), ncol = ncol(surf$Zterrain_rel)),
      name = "Terrain"
    ) %>%
    plotly::add_markers(
      data = mesh_cov, x = ~x, y = ~y, z = ~z_plot, type = "scatter3d", mode = "markers",
      color = ~risk_band, marker = list(size = 2, opacity = 0.75), name = ~risk_band
    ) %>%
    plotly::add_trace(
      data = surf$cyl_wire, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "lines",
      line = list(color = "grey60", width = 1),
      name = paste0(radius, " m cylinder boundary"), showlegend = FALSE, hoverinfo = "skip"
    ) %>%
    plotly::add_trace(
      data = bearing_line, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "lines",
      line = list(color = "black", width = 4),
      name = paste0("View axis ", bearing_line$label[1], "→", bearing_line$label[2])
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
      surfacecolor = matrix("black", nrow = nrow(surf$Zterrain_rel), ncol = ncol(surf$Zterrain_rel)),
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
      data = surf$cyl_wire, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "lines",
      line = list(color = "grey60", width = 1), hoverinfo = "skip",
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
