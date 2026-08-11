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

wtg_wgs84_test <- sf::st_transform(wtg[wtg$ID == wtg_id_test, ], 4326)
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

track_wtg_test <- track_dt[turbine == wtg_id_test]
cat(sprintf("\nRegistos de track para %s: %d\n", wtg_id_test, nrow(track_wtg_test)))

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

p_test <- plot_mesh_coverage_3d(
  terrain_mesh_test, coverage_test,
  radius = coverage_cylinder_wider_radius, cyl_height = coverage_cylinder_height
)
p_test
