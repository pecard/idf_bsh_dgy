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
classified_test <- classify_terrain(turrain_test, ridge_metric = "relative_elev_m", ridge_relelev_m = 15, complex_slope_deg = 8)
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


## 2. Limiares por omissao (quantil, calibrados ao proprio conjunto de
## turbinas) -- 10 turbinas sinteticas com relative_elev_m e mean_slope_deg
## espacados uniformemente, para poder calcular os quantis a mao (quantile()
## tipo 7, omissao do R) --------------------------------------------------
##
## relative_elev_m = 0,2,4,...,20 (10 valores) -> quantile(0.90): h=9.1 ->
##   x[9]=16 + 0.4*(20-16) = 16.4 -> so' a turbina com 20 fica "ridge" (1/10)
## mean_slope_deg (so' entre as 9 nao-ridge) = 1,2,...,9 -> quantile(0.75):
##   h=7.0 -> x[7]=7 -> turbinas com slope 7,8,9 ficam "complex" (3/9)
turrain_test2 <- data.table::data.table(
  wtg_id          = paste0("TTQ", 1:10),
  elev_m          = 100 + c(0, 2, 4, 6, 8, 10, 12, 14, 16, 20),
  mean_elev_m     = 100,
  relative_elev_m = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 20),
  mean_slope_deg  = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 20),
  mean_tri_m      = 1
)

cat("\n===== classify_terrain(turrain_test2, ridge_metric = \"relative_elev_m\") -- limiares por omissao (quantil) =====\n")
classified_test2 <- classify_terrain(turrain_test2, ridge_metric = "relative_elev_m")
print(classified_test2[, .(wtg_id, relative_elev_m, mean_slope_deg, terrain_class)])

thresholds2 <- attr(classified_test2, "thresholds")
cat(sprintf(
  "\nLimiares calculados: ridge_cutoff=%.2f (esperado 16.40, sobre relative_elev_m), complex_slope_deg=%.2f (esperado 7.00)\n",
  thresholds2$ridge_cutoff, thresholds2$complex_slope_deg
))

expected_class2 <- stats::setNames(
  c(rep("flat", 6), "complex", "complex", "complex", "ridge"),
  paste0("TTQ", 1:10)
)
classified_test2[, expected := expected_class2[wtg_id]]
classified_test2[, ok := as.character(terrain_class) == expected]
cat(sprintf(
  "Resultado: %d/%d turbinas classificadas corretamente (esperado: TTQ1-6 flat, TTQ7-9 complex, TTQ10 ridge).\n",
  sum(classified_test2$ok), nrow(classified_test2)
))


## 3. Caso-limite: todas as turbinas ficam "ridge" (relative_elev_m
## constante e >= o proprio quantil 0.90 de um vetor constante) -- nao ha
## nao-ridge para o quantil de "complex" -- complex_cutoff deve ficar Inf,
## sem erro (em vez de quantile(numeric(0), ...)) -----------------------

cat("\n===== classify_terrain() -- caso-limite: todas as turbinas com a mesma relative_elev_m =====\n")
turrain_test3 <- data.table::data.table(
  wtg_id = c("TTQ_A", "TTQ_B", "TTQ_C"),
  elev_m = 110, mean_elev_m = 100, relative_elev_m = 10,
  mean_slope_deg = c(1, 5, 9), mean_tri_m = 1
)
classified_test3 <- classify_terrain(turrain_test3, ridge_metric = "relative_elev_m")
print(classified_test3[, .(wtg_id, relative_elev_m, mean_slope_deg, terrain_class)])
cat(sprintf(
  "Esperado: todas 'ridge' (quantile(0.90) de um vetor constante == o proprio valor, >= inclusive) -- %s\n",
  if (all(as.character(classified_test3$terrain_class) == "ridge")) "OK" else "FALHOU"
))


## 4. ridge_metric -- confirma que o argumento realmente troca a coluna
## usada para o criterio de "ridge" (por omissao "ridge_proximity_m", o
## combinado de 2 buffers -- ver compute_turbine_terrain_metrics()) --------
##
## 2 turbinas com relative_elev_m e ridge_proximity_m EM DESACORDO
## deliberadamente, para que a escolha do criterio mude o resultado:
##   TTM_A -- relative_elev_m alto (20, seria "ridge" pelo criterio antigo),
##            ridge_proximity_m baixo (2, plateau alto mas sem crista perto)
##   TTM_B -- relative_elev_m baixo (2, seria "flat"/"complex" pelo criterio
##            antigo), ridge_proximity_m alto (20 -- ex: elev_gradient_m
##            alto, terreno a subir para uma crista fora do footing imediato,
##            o caso que motivou esta seccao 2 de buffers)
cat("\n===== classify_terrain() -- ridge_metric muda qual coluna decide \"ridge\" =====\n")
turrain_test4 <- data.table::data.table(
  wtg_id = c("TTM_A", "TTM_B"), elev_m = c(120, 102), mean_elev_m = 100,
  relative_elev_m = c(20, 2), ridge_proximity_m = c(2, 20),
  mean_slope_deg = 1, mean_tri_m = 1
)

## complex_slope_deg fixo e alto de proposito -- isola este teste do
## criterio de "ridge", sem interferencia do quantil de "complex" (que com
## so' 1 turbina nao-ridge de cada vez colapsaria no proprio valor -- mesmo
## caso-limite da seccao 3 acima)
by_proximity <- classify_terrain(turrain_test4, ridge_relelev_m = 15, complex_slope_deg = 999) # omissao: ridge_metric = "ridge_proximity_m"
by_relelev   <- classify_terrain(turrain_test4, ridge_metric = "relative_elev_m", ridge_relelev_m = 15, complex_slope_deg = 999)

cat("Por ridge_proximity_m (omissao):\n")
print(by_proximity[, .(wtg_id, relative_elev_m, ridge_proximity_m, terrain_class)])
cat("Por relative_elev_m (explicito):\n")
print(by_relelev[, .(wtg_id, relative_elev_m, ridge_proximity_m, terrain_class)])

ok_proximity <- identical(as.character(by_proximity$terrain_class), c("flat", "ridge")) # TTM_A flat, TTM_B ridge
ok_relelev   <- identical(as.character(by_relelev$terrain_class), c("ridge", "flat"))   # invertido
cat(sprintf(
  "Esperado: por ridge_proximity_m -> TTM_A='flat'/TTM_B='ridge'; por relative_elev_m -> invertido -- %s\n",
  if (ok_proximity && ok_relelev) "OK" else "FALHOU"
))


## 5. summarise_terrain_class_counts() -- n e % de turbinas por classe -----
## (reutiliza classified_test da seccao 1: 1 flat, 1 complex, 2 ridge)

cat("\n===== summarise_terrain_class_counts(classified_test) =====\n")
counts_test <- summarise_terrain_class_counts(classified_test)
print(counts_test)
cat("Esperado: flat n=1 (25%), complex n=1 (25%), ridge n=2 (50%).\n")

cat("\n----- Caso-limite: nenhuma turbina 'complex' -----\n")
counts_test3 <- summarise_terrain_class_counts(classified_test3) # so' "ridge" (secção 3)
print(counts_test3)
cat("Esperado: flat n=0/0%, complex n=0/0%, ridge n=3/100% (0-fill nas classes ausentes).\n")
