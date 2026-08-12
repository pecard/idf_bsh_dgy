##
## Smoke test com dados REAIS (nao sintetico) para R/coverage_3d_topography.R
##
## Objetivo: validar rapidamente, so para 1 turbina (BSH54), que o DEM/CRS/
## malha 3D estao a funcionar corretamente, antes de ligar ao IDF_analysis.R
## e correr para todas as turbinas do parque.
##
## Pre-requisito: correr o IDF_analysis.R normalmente ate a seccao
## "0. Filter data" (inclusive) primeiro -- este script reaproveita os
## objetos ja criados em memoria: wtg, track_dt, databases_dir,
## dem_filename, coverage_cylinder_wider_radius, coverage_cylinder_height,
## coverage_mesh_step_xy, coverage_mesh_step_z, coverage_prox_thresh_m.
##
## Correr: source("tests/smoke_test_coverage_3d.R")
##

source("R/coverage_3d_topography.R")

wtg_id_test <- "BSH54"

dem_file <- file.path(databases_dir, dem_filename)
cat(sprintf("DEM file: %s\nExiste: %s\n", dem_file, file.exists(dem_file)))
stopifnot("DEM file nao encontrado -- confirma o nome/pasta" = file.exists(dem_file))


## 1. Coordenadas da turbina, extraidas do shapefile (nao a mao) -----------

wtg_wgs84_test <- sf::st_transform(wtg[wtg$InternalNa == wtg_id_test, ], 4326)
stopifnot("Turbina nao encontrada no shapefile wtg" = nrow(wtg_wgs84_test) == 1L)

coords_test <- sf::st_coordinates(wtg_wgs84_test)
wtg_lon_test <- coords_test[1, "X"]
wtg_lat_test <- coords_test[1, "Y"]

cat(sprintf("\n%s -- lat=%.6f, lon=%.6f\n", wtg_id_test, wtg_lat_test, wtg_lon_test))


## 2. Malha 3D corrigida ao terreno ------------------------------------------

terrain_mesh_test <- build_terrain_mesh(
  wtg_id_test, wtg_lat_test, wtg_lon_test, dem_file,
  radius     = coverage_cylinder_wider_radius,
  cyl_height = coverage_cylinder_height,
  step_xy    = coverage_mesh_step_xy,
  step_z     = coverage_mesh_step_z,
  risk_band_breaks = c(200), risk_band_labels = c("at risk", "above")
)

cat("\n===== build_terrain_mesh() =====\n")
cat(sprintf("wtg_elev (m): %.1f\n", terrain_mesh_test$wtg_elev))
cat(sprintf("nrow(mesh_air): %d | nrow(mesh_terrain): %d | nrow(mesh_xy): %d\n",
           nrow(terrain_mesh_test$mesh_air), nrow(terrain_mesh_test$mesh_terrain), nrow(terrain_mesh_test$mesh_xy)))
cat("\nResumo terrain_elev (mesh_xy) -- se vier tudo NA, o DEM nao cobre a area ou o CRS nao bate certo:\n")
print(summary(terrain_mesh_test$mesh_xy$terrain_elev))
cat("\nDistribuicao mesh_air por risk_band:\n")
print(terrain_mesh_test$mesh_air[, .N, by = risk_band])


## 3. Cobertura com os tracks reais desta turbina ----------------------------

## Candidatos por distancia real (UTM) a turbina, nao pela classificacao
## "NearestTurbine3d" do IdentiFlight (coluna `turbine`) -- ver nota em
## run_coverage_3d_all_turbines() no R/coverage_3d_topography.R
wtg_utm_test <- sf::st_coordinates(wtg[wtg$InternalNa == wtg_id_test, ])
dist_to_wtg_test <- sqrt(
  (track_dt$utm_x - wtg_utm_test[1, "X"])^2 + (track_dt$utm_y - wtg_utm_test[1, "Y"])^2
)
track_wtg_test <- track_dt[dist_to_wtg_test <= coverage_cylinder_wider_radius + 200]
cat(sprintf("\nRegistos de track para %s (por distancia): %d\n", wtg_id_test, nrow(track_wtg_test)))

coverage_test <- compute_mesh_coverage(
  terrain_mesh_test, track_wtg_test,
  radius = coverage_cylinder_wider_radius, cyl_height = coverage_cylinder_height,
  prox_thresh_m = coverage_prox_thresh_m
)

cat("\n===== compute_mesh_coverage() =====\n")
print(coverage_test$metrics)
cat("\nPor risk_band:\n")
print(coverage_test$by_risk_band)


## 4. Plot 3D -- abre no Viewer do RStudio -----------------------------------
source("R/coverage_3d_topography.R")
p_test <- plot_mesh_coverage_3d(
  terrain_mesh_test, coverage_test,
  radius = coverage_cylinder_wider_radius, cyl_height = coverage_cylinder_height
)
p_test


# Teste 2 Plot 3D
source("R/coverage_3d_topography.R")

## reconstruir tudo de fresco (assume que ja correste a importacao do IDF_analysis.R
## e o resto do smoke test ate ao coverage_test)

surf <- .build_plot_surfaces(
  terrain_mesh_test, coverage_test$mesh_air[covered == TRUE]$z_rel_turbine,
  radius = coverage_cylinder_wider_radius, cyl_height = coverage_cylinder_height,
  step_z = coverage_mesh_step_z, z_pad_lower = 50, z_pad_upper = 50
)

mesh_cov <- coverage_test$mesh_air[covered == TRUE]
mesh_cov[, z_plot := z_rel_turbine]

radius <- coverage_cylinder_wider_radius
cyl_height <- coverage_cylinder_height
z_min <- surf$z_min_cyl
z_max <- surf$z_max_cyl

bearing_line <- .make_bearing_line(bearing_from_deg = 135, radius = radius, z = cyl_height + 30)
bearing_labels <- data.table::copy(bearing_line)
bearing_labels[, `:=`(x = x * 0.90, y = y * 0.90, z = z - 15)]

cat(sprintf("z_min=%.1f z_max=%.1f | bearing_line z range: %.1f a %.1f\n",
            z_min, z_max, min(bearing_line$z), max(bearing_line$z)))

plot_ly() %>%
  add_surface(x = surf$xs_surf, y = surf$ys_surf, z = surf$Zterrain_rel, opacity = 0.85, showscale = FALSE) %>%
  add_markers(data = mesh_cov, x = ~x, y = ~y, z = ~z_plot, type = "scatter3d", mode = "markers",
              color = ~risk_band, marker = list(size = 1.2, opacity = 0.75)) %>%
  add_trace(x = surf$cyl_mesh$x, y = surf$cyl_mesh$y, z = surf$cyl_mesh$z,
            i = surf$cyl_mesh$i, j = surf$cyl_mesh$j, k = surf$cyl_mesh$k,
            type = "mesh3d", opacity = 0.08, color = I("lightblue"), hoverinfo = "skip") %>%
  add_trace(data = bearing_line, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "lines",
            line = list(color = "black", width = 4)) %>%
  add_trace(data = bearing_labels, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "text",
            text = ~label, textposition = "middle center", textfont = list(size = 16, color = "black"),
            showlegend = FALSE, hoverinfo = "skip") %>%
  layout(
    scene = list(
      xaxis = list(range = c(-radius, radius), autorange = FALSE),
      yaxis = list(range = c(-radius, radius), autorange = FALSE),
      zaxis = list(range = c(z_min, z_max), autorange = FALSE),
      aspectmode = "manual",
      aspectratio = list(x = 1, y = 1, z = (z_max - z_min) / (2 * radius)),
      camera = .camera_from_bearing(bearing_deg = 135, distance = 2.5, z = 0.7)
    )
  )

teste_fn <- function() {
  plot_ly() %>%
    add_surface(x = surf$xs_surf, y = surf$ys_surf, z = surf$Zterrain_rel, opacity = 0.85, showscale = FALSE) %>%
    add_markers(data = mesh_cov, x = ~x, y = ~y, z = ~z_plot, type = "scatter3d", mode = "markers",
                color = ~risk_band, marker = list(size = 1.2, opacity = 0.75)) %>%
    add_trace(x = surf$cyl_mesh$x, y = surf$cyl_mesh$y, z = surf$cyl_mesh$z,
              i = surf$cyl_mesh$i, j = surf$cyl_mesh$j, k = surf$cyl_mesh$k,
              type = "mesh3d", opacity = 0.08, color = I("lightblue"), hoverinfo = "skip") %>%
    add_trace(data = bearing_line, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "lines",
              line = list(color = "black", width = 4)) %>%
    add_trace(data = bearing_labels, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "text",
              text = ~label, textposition = "middle center", textfont = list(size = 16, color = "black"),
              showlegend = FALSE, hoverinfo = "skip") %>%
    layout(
      scene = list(
        xaxis = list(range = c(-radius, radius), autorange = FALSE),
        yaxis = list(range = c(-radius, radius), autorange = FALSE),
        zaxis = list(range = c(z_min, z_max), autorange = FALSE),
        aspectmode = "manual",
        aspectratio = list(x = 1, y = 1, z = (z_max - z_min) / (2 * radius)),
        camera = .camera_from_bearing(bearing_deg = 135, distance = 2.5, z = 0.7)
      )
    )
}

teste_fn()


options(error = NULL)  # temporario, so para diagnostico

p_test <- plot_mesh_coverage_3d(
  terrain_mesh_test, coverage_test,
  radius = coverage_cylinder_wider_radius, cyl_height = coverage_cylinder_height
)
p_test

## 1. Confirma o commit exato
system("git log -1 --oneline")

## 2. Mostra a funcao REAL que tens carregada em memoria, tal como R a interpretou
print(plot_mesh_coverage_3d)

plot_ly() %>%
  add_surface(x = surf$xs_surf, y = surf$ys_surf, z = surf$Zterrain_rel, opacity = 0.85, showscale = FALSE,
              colorscale = "Viridis", name = "Terrain") %>%
  add_markers(data = mesh_cov, x = ~x, y = ~y, z = ~z_plot, type = "scatter3d", mode = "markers",
              color = ~risk_band, marker = list(size = 1.2, opacity = 0.75)) %>%
  add_trace(x = surf$cyl_mesh$x, y = surf$cyl_mesh$y, z = surf$cyl_mesh$z,
            i = surf$cyl_mesh$i, j = surf$cyl_mesh$j, k = surf$cyl_mesh$k,
            type = "mesh3d", opacity = 0.08,
            facecolor = rep("grey70", length(surf$cyl_mesh$i)),
            hoverinfo = "skip") %>%
  layout(
    scene = list(
      xaxis = list(range = c(-radius, radius), autorange = FALSE),
      yaxis = list(range = c(-radius, radius), autorange = FALSE),
      zaxis = list(range = c(z_min, z_max), autorange = FALSE),
      aspectmode = "manual",
      aspectratio = list(x = 1, y = 1, z = (z_max - z_min) / (2 * radius)),
      camera = .camera_from_bearing(bearing_deg = 135, distance = 2.5, z = 0.7)
    )
  )

bearing_line <- .make_bearing_line(bearing_from_deg = 135, radius = radius, z = cyl_height + 30)
bearing_labels <- data.table::copy(bearing_line)
bearing_labels[, `:=`(x = x * 0.90, y = y * 0.90, z = z - 15)]

plot_ly() %>%
  add_surface(x = surf$xs_surf, y = surf$ys_surf, z = surf$Zterrain_rel, opacity = 0.85, showscale = FALSE,
              colorscale = "Viridis", name = "Terrain") %>%
  add_markers(data = mesh_cov, x = ~x, y = ~y, z = ~z_plot, type = "scatter3d", mode = "markers",
              color = ~risk_band, marker = list(size = 1.2, opacity = 0.75)) %>%
  add_trace(x = surf$cyl_mesh$x, y = surf$cyl_mesh$y, z = surf$cyl_mesh$z,
            i = surf$cyl_mesh$i, j = surf$cyl_mesh$j, k = surf$cyl_mesh$k,
            type = "mesh3d", opacity = 0.38, color = I("grey"), hoverinfo = "skip") %>%
  add_trace(data = bearing_line, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "lines",
            line = list(color = "black", width = 4)) %>%
  add_trace(data = bearing_labels, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "text",
            text = ~label, textposition = "middle center", textfont = list(size = 16, color = "black"),
            showlegend = FALSE, hoverinfo = "skip") %>%
  layout(
    scene = list(
      xaxis = list(range = c(-radius, radius), autorange = FALSE),
      yaxis = list(range = c(-radius, radius), autorange = FALSE),
      zaxis = list(range = c(z_min, z_max), autorange = FALSE),
      aspectmode = "manual",
      aspectratio = list(x = 1, y = 1, z = (z_max - z_min) / (2 * radius)),
      camera = .camera_from_bearing(bearing_deg = 135, distance = 2.5, z = 0.7)
    )
  )

source("R/coverage_3d_topography.R")

p_test <- plot_mesh_coverage_3d(
  terrain_mesh_test, coverage_test,
  radius = coverage_cylinder_wider_radius, cyl_height = coverage_cylinder_height
)
p_test
