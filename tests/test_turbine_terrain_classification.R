##
## Teste com dados simulados para R/turbine_terrain_classification.R
##
## Foca-se em classify_terrain() -- a logica pura (data.table, sem DEM/CRS)
## que decide a classe de terreno a partir das metricas ja calculadas. E' a
## unica funcao testavel sem um DEM real -- compute_turbine_terrain_metrics()
## faz crop/reproject/extract sobre um GeoTIFF real (mesma limitacao de
## build_terrain_mesh(), R/coverage_3d_topography.R -- ver
## tests/test_coverage_3d_topography.R e tests/smoke_test_coverage_3d.R para
## o mesmo padrao) -- correr compute_turbine_terrain_metrics() manualmente,
## com wtg/dem_file reais, para validar essa parte.
##
## Correr: source("tests/test_turbine_terrain_classification.R")
##
## Depende de: data.table
##

source("R/turbine_terrain_classification.R")

## 4 turbinas sinteticas cobrindo as 3 classes + 1 caso-limite:
##   TTC_RIDGE  -- relative_elev_m bem acima do limiar (20 >= 15) -> "ridge",
##                 mesmo com slope baixo (a prioridade de ridge e' testada aqui)
##   TTC_COMPLEX -- relative_elev_m baixo (nao ridge), mas slope acima do
##                  limiar (10 >= 8) -> "complex"
##   TTC_FLAT   -- relative_elev_m e slope baixos -> "flat"
##   TTC_BOUNDARY -- relative_elev_m e slope EXATAMENTE nos limiares (15 e 8)
##                   -- testa o >= (inclusive) nos dois limiares -> "ridge"
##                   (prioridade sobre "complex" mesmo empatando nos 2 limiares)
turrain_test <- data.table::data.table(
  wtg_id          = c("TTC_RIDGE", "TTC_COMPLEX", "TTC_FLAT", "TTC_BOUNDARY"),
  elev_m          = c(120, 105, 100, 115),
  mean_elev_m     = c(100, 100, 100, 100),
  relative_elev_m = c(20, 5, 0, 15),
  mean_slope_deg  = c(3, 10, 2, 8),
  mean_tri_m      = c(5, 12, 1, 8)
)

cat("\n===== classify_terrain(turrain_test) =====\n")
classified_test <- classify_terrain(turrain_test, ridge_relelev_m = 15, complex_slope_deg = 8)
print(classified_test[, .(wtg_id, relative_elev_m, mean_slope_deg, terrain_class)])

expected_class <- c(TTC_RIDGE = "ridge", TTC_COMPLEX = "complex", TTC_FLAT = "flat", TTC_BOUNDARY = "ridge")
classified_test[, expected := expected_class[wtg_id]]
classified_test[, ok := as.character(terrain_class) == expected]
cat(sprintf(
  "Resultado: %d/%d turbinas classificadas corretamente.\n",
  sum(classified_test$ok), nrow(classified_test)
))

cat("\nNiveis do factor terrain_class (esperado: flat, complex, ridge, nesta ordem):\n")
print(levels(classified_test$terrain_class))
