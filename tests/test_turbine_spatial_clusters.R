##
## Teste com dados simulados para R/turbine_spatial_clusters.R
##
## 6 turbinas sobre uma linha reta (y=0), em 3 grupos claramente separados
## por distancia, para poder confirmar a olho o comportamento de
## single-linkage (cadeia transitiva) e a sensibilidade ao limiar:
##   T1=0, T2=300, T3=600           -- espacadas 300m consecutivas
##   T4=5000, T5=5300               -- espacadas 300m, longe do 1º grupo
##   T6=20000                        -- isolada
##
## Distancias-chave (|x_i - x_j|, y=0 para todas):
##   T1-T2=300, T2-T3=300, T1-T3=600
##   T4-T5=300
##   T3-T4=4400 (a "ponte" mais curta entre os 2 primeiros grupos)
##   T5-T6=14700, T4-T6=15000 (as "pontes" mais curtas ate T6)
##
## Nao usa testthat -- dados sinteticos, resultado calculado a mao.
##
## Correr: source("tests/test_turbine_spatial_clusters.R")
##
## Depende de: data.table, sf, cluster
##

source("R/turbine_spatial_clusters.R")

wtg_test <- sf::st_as_sf(
  data.frame(
    InternalNa = c("T1", "T2", "T3", "T4", "T5", "T6"),
    x = c(0, 300, 600, 5000, 5300, 20000),
    y = c(0, 0, 0, 0, 0, 0)
  ),
  coords = c("x", "y"), crs = 32641
)

dist_mat_test <- turbine_distance_matrix(wtg_test)

cat("\n===== turbine_distance_matrix() =====\n")
print(dist_mat_test)
cat(paste(
  "\nEsperado: T1-T2=300, T2-T3=300, T1-T3=600, T4-T5=300, T3-T4=4400,",
  "T5-T6=14700, T4-T6=15000 (conferir a olho contra a matriz acima).\n"
))

## ---- cluster_turbines_by_distance() em 3 limiares ----

cl_400 <- cluster_turbines_by_distance(dist_mat_test, max_dist_m = 400)
cat("\n===== cluster_turbines_by_distance(max_dist_m=400) =====\n")
print(cl_400[order(cluster_id, turbine)])
cat(paste(
  "\nEsperado: 3 clusters -- {T1,T2,T3} (cadeia 300+300, mesmo T1-T3=600>400",
  "ficam juntos por transitividade via T2), {T4,T5} (300<=400), {T6} sozinha",
  "(min distancia a qualquer outra e' 14700).\n"
))
n_clusters_400 <- data.table::uniqueN(cl_400$cluster_id)
cat(sprintf("Nº de clusters obtido: %d (esperado: 3)\n", n_clusters_400))

cl_200 <- cluster_turbines_by_distance(dist_mat_test, max_dist_m = 200)
n_clusters_200 <- data.table::uniqueN(cl_200$cluster_id)
cat(sprintf(
  "\ncluster_turbines_by_distance(max_dist_m=200): nº de clusters = %d (esperado: 6 -- nenhum par a <=200m, todas isoladas)\n",
  n_clusters_200
))

cl_10000 <- cluster_turbines_by_distance(dist_mat_test, max_dist_m = 10000)
n_clusters_10000 <- data.table::uniqueN(cl_10000$cluster_id)
cat(sprintf(
  "cluster_turbines_by_distance(max_dist_m=10000): nº de clusters = %d (esperado: 2 -- {T1..T5} juntam-se via T3-T4=4400<=10000, T6 continua isolada pois min(14700,15000)>10000)\n",
  n_clusters_10000
))

cl_30000 <- cluster_turbines_by_distance(dist_mat_test, max_dist_m = 30000)
n_clusters_30000 <- data.table::uniqueN(cl_30000$cluster_id)
cat(sprintf(
  "cluster_turbines_by_distance(max_dist_m=30000): nº de clusters = %d (esperado: 1 -- T5-T6=14700<=30000, tudo junta)\n",
  n_clusters_30000
))

## ---- turbine_cluster_threshold_sensitivity() ----

sens_dt <- turbine_cluster_threshold_sensitivity(dist_mat_test, thresholds_m = c(200, 400, 10000, 30000))
cat("\n===== turbine_cluster_threshold_sensitivity() =====\n")
print(sens_dt)
cat(paste(
  "\nEsperado: n_clusters = 6, 3, 2, 1 (por esta ordem de limiares) --",
  "monotonamente decrescente, como confirmado acima. n_singleton_clusters:",
  "6 (200, todas sozinhas), 1 (400, so' T6), 1 (10000, so' T6), 0 (30000,",
  "so' ha 1 cluster com todas). mean_silhouette_width fica NA em 200",
  "(n_clusters==n_turbines) e em 30000 (n_clusters==1, silhouette",
  "indefinida) -- so' e' calculada para 400 e 10000. NAO da' para conferir",
  "o valor exato de silhouette a mao com confianca; o esperado e' um valor",
  "positivo e alto (grupos bem separados face ao seu tamanho interno) --",
  "conferir que fica claramente > 0.5 nos 2 casos calculados.\n"
))

## ---- manual_turbine_clusters_dt() ----

manual_test <- list(Grupo_X = c("T1", "T2", "T3"), Grupo_Y = c("T4", "T5"), Grupo_Z = c("T6"))
manual_dt_test <- manual_turbine_clusters_dt(manual_test)
cat("\n===== manual_turbine_clusters_dt() =====\n")
print(manual_dt_test[order(cluster_id, turbine)])
cat(paste(
  "\nEsperado: 6 linhas, turbine/cluster_id tal como definido na lista",
  "(T1,T2,T3 -> Grupo_X; T4,T5 -> Grupo_Y; T6 -> Grupo_Z), mesmo formato de",
  "colunas que cluster_turbines_by_distance() (para serem intermutaveis).\n"
))
