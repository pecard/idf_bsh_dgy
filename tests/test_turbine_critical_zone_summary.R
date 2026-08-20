##
## Teste com dados simulados para R/turbine_critical_zone_summary.R
##
## Nao recalcula nada -- so' confirma que summarise_turbine_critical_zone()
## junta corretamente as 2 tabelas de origem (contributo marginal de
## curtailments + ranking do cluster de atividade da especie) por
## turbine+cluster_id, com o setcolorder esperado. Os valores em si vem
## diretamente das 2 tabelas de entrada, sem calculo novo.
##
## Nao usa testthat -- dados sinteticos, resultado calculado a mao.
##
## Correr: source("tests/test_turbine_critical_zone_summary.R")
##
## Depende de: data.table
##

source("R/turbine_critical_zone_summary.R")

marginal_dt_test <- data.table(
  turbine = c("TA1", "TB3"),
  cluster_id = c("ClusterA", "ClusterB"),
  n_turbines_in_cluster = c(2L, 3L),
  n_total = c(5L, 3L),
  pct_of_cluster = c(62.5, 50.0),
  cluster_rank = c(1L, 2L),
  n_clusters = c(2L, 2L),
  median_weekly_pct_of_cluster = c(62.5, 50.0)
)

species_cluster_rank_dt_test <- data.table(
  turbine = c("TA1", "TB3"),
  cluster_id = c("ClusterA", "ClusterB"),
  track_n_tracks = c(10L, 4L),
  track_pct_of_total = c(70.0, 40.0),
  track_cluster_rank = c(1L, 2L),
  track_n_clusters = c(2L, 2L)
)

critical_zone_dt <- summarise_turbine_critical_zone(marginal_dt_test, species_cluster_rank_dt_test)

cat("\n===== summarise_turbine_critical_zone() =====\n")
print(critical_zone_dt)
cat(paste(
  "\nEsperado: 2 linhas. TA1 -> ClusterA, n_total=5 (62.5% do cluster,",
  "cluster_rank=1 de 2) E track_n_tracks=10 (70.0% do total de tracks,",
  "track_cluster_rank=1 de 2) -- concentra-se nos 2 eixos, e' o caso mais",
  "claro de 'zona critica'. TB3 -> ClusterB, n_total=3 (50.0%, rank=2 de 2)",
  "E track_n_tracks=4 (40.0%, track_rank=2 de 2) -- nem um eixo nem o outro",
  "se destaca. Confirmar tambem a ordem das colunas: turbine, cluster_id,",
  "n_turbines_in_cluster, n_total, pct_of_cluster, cluster_rank, n_clusters,",
  "track_n_tracks, track_pct_of_total, track_cluster_rank, track_n_clusters",
  "(median_weekly_pct_of_cluster fica no fim, fora do setcolorder explicito).\n"
))
