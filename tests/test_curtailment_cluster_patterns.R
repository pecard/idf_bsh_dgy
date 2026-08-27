##
## Teste com dados simulados para R/curtailment_cluster_patterns.R
##
## Usa clusters manuais (2 grupos, ver manual_turbine_clusters_dt() em
## R/turbine_spatial_clusters.R) para nao depender do rotulo interno
## arbitrario que cutree() atribui aos clusters estatisticos -- a logica de
## sumario/permutacao e' identica para os 2 tipos de cluster.
##
## ClusterA (2 turbinas, TA1/TA2): 2 semanas de curtailments --
##   semana 1 (2026-04-07): TA1 x3, TA2 x1 (total semana=4)
##   semana 2 (2026-04-14): TA1 x2, TA2 x2 (total semana=4)
##   -> TA1=5, TA2=3, total cluster=8
##
## ClusterB (3 turbinas, TB1/TB2/TB3):
##   semana 1: TB1 x1, TB2 x1, TB3 x1 (total semana=3)
##   semana 2: TB1 x0, TB2 x1, TB3 x2 (total semana=3)
##   -> TB1=1, TB2=2, TB3=3, total cluster=6
##
## Nao usa testthat -- dados sinteticos, resultado calculado a mao.
##
## Correr: source("tests/test_curtailment_cluster_patterns.R")
##
## Depende de: data.table
##

source("R/turbine_spatial_clusters.R")
source("R/curtailment_cluster_patterns.R")

manual_test <- list(ClusterA = c("TA1", "TA2"), ClusterB = c("TB1", "TB2", "TB3"))
cluster_dt_test <- manual_turbine_clusters_dt(manual_test)

w1 <- as.POSIXct("2026-04-07 10:00:00", tz = "UTC")
w2 <- as.POSIXct("2026-04-14 10:00:00", tz = "UTC")  # exatamente 7 dias depois -> semana seguinte

curtl_dt_test <- data.table(
  turbine = c(
    rep("TA1", 3), rep("TA2", 1),               # ClusterA, semana 1
    rep("TA1", 2), rep("TA2", 2),               # ClusterA, semana 2
    "TB1", "TB2", "TB3",                        # ClusterB, semana 1
    "TB2", rep("TB3", 2)                        # ClusterB, semana 2
  ),
  start = c(
    rep(w1, 4), rep(w2, 4),
    rep(w1, 3), rep(w2, 3)
  ),
  track_id = as.character(1:14),
  species  = "Steppe-Eagle"
)

curtl_cl_dt <- join_curtailments_to_clusters(curtl_dt_test, cluster_dt_test)

cluster_summary <- summarise_cluster_curtailments(curtl_cl_dt)
cat("\n===== summarise_cluster_curtailments()$by_turbine =====\n")
print(cluster_summary$by_turbine)
cat(paste(
  "\nEsperado: TA1 n=5 (62.5% do ClusterA), TA2 n=3 (37.5%); TB1 n=1",
  "(16.7% do ClusterB), TB2 n=2 (33.3%), TB3 n=3 (50.0%).\n"
))

cat("\n===== summarise_cluster_curtailments()$by_cluster =====\n")
print(cluster_summary$by_cluster)
cat(paste(
  "\nEsperado: ClusterA n_total=8 (2 turbinas, mean_per_turbine=4.0,",
  "cluster_rank=1), ClusterB n_total=6 (3 turbinas, mean_per_turbine=2.0,",
  "cluster_rank=2) -- ClusterA e' mais ativo no total.\n"
))

weekly_dt <- summarise_cluster_curtailments_weekly(curtl_cl_dt)
cat("\n===== summarise_cluster_curtailments_weekly() =====\n")
print(weekly_dt)
cat(paste(
  "\nEsperado: 8 linhas (2 clusters x ate 3 turbinas x 2 semanas, exceto",
  "TB1 que so' aparece na semana 1, ja que teve 0 curtailments na semana 2)",
  "-- TA1: semana1 n=3, semana2 n=2. TA2: semana1 n=1, semana2 n=2. TB1:",
  "so semana1 n=1. TB2: semana1 n=1, semana2 n=1. TB3: semana1 n=1,",
  "semana2 n=2.\n"
))

## ---- summarise_turbine_marginal_contribution() ----

marginal_dt <- summarise_turbine_marginal_contribution(curtl_cl_dt, c("TA1", "TB3"), cluster_dt_test)
cat("\n===== summarise_turbine_marginal_contribution() =====\n")
print(marginal_dt)
cat(paste(
  "\nEsperado TA1: cluster_id=ClusterA, n_turbines_in_cluster=2, n_total=5,",
  "pct_of_cluster=62.5, cluster_rank=1, n_clusters=2. Quota semanal: semana1",
  "3/4=75%, semana2 2/4=50% -> median_weekly_pct_of_cluster=62.5 (coincide",
  "com a quota do periodo completo neste caso).\n",
  "Esperado TB3: cluster_id=ClusterB, n_turbines_in_cluster=3, n_total=3,",
  "pct_of_cluster=50.0, cluster_rank=2, n_clusters=2. Quota semanal: semana1",
  "1/3=33.3%, semana2 2/3=66.7% -> median_weekly_pct_of_cluster=50.0.\n"
))

## ---- permutation_test_marginal_contribution() ----
## H0: curtailments uniformemente distribuidos pelas turbinas do cluster.

perm_ta1 <- permutation_test_marginal_contribution(curtl_cl_dt, "TA1", cluster_dt_test, n_perm = 999, seed = 1)
cat("\n===== permutation_test_marginal_contribution('TA1') =====\n")
print(perm_ta1)
cat(paste(
  "\nClusterA tem 2 turbinas -- sob H0 e' um Binomial(n=8, p=0.5).",
  "P(X>=5) exato = (C(8,5)+C(8,6)+C(8,7)+C(8,8))/2^8 = (56+28+8+1)/256 =",
  "93/256 = 0.3633. p_value_gt_uniform (por permutacao, 999 iteracoes) deve",
  "ficar perto disto (nao exatamente igual -- e' Monte Carlo), tipicamente",
  "dentro de +/-0.03. Nao e' um resultado extremo -- TA1 ter 62.5% de um",
  "cluster de so' 2 turbinas nao e' surpreendente ao acaso.\n"
))

perm_tb3 <- permutation_test_marginal_contribution(curtl_cl_dt, "TB3", cluster_dt_test, n_perm = 999, seed = 1)
cat("\n===== permutation_test_marginal_contribution('TB3') =====\n")
print(perm_tb3)
cat(paste(
  "\nClusterB tem 3 turbinas -- sob H0 a proporcao esperada e' 33.3% por",
  "turbina, TB3 observou 50.0% (3/6). Amostra pequena (n=6), por isso nao",
  "se espera um p-value muito baixo -- so' se confirma que fica um valor",
  "plausivel (positivo, nao extremo) e nao um erro/NA.\n"
))

perm_all <- permutation_test_marginal_contribution_all(curtl_cl_dt, c("TA1", "TA2", "TB1", "TB2", "TB3"), cluster_dt_test, n_perm = 999, seed = 1)
cat("\n===== permutation_test_marginal_contribution_all() =====\n")
print(perm_all)
cat("\nEsperado: 5 linhas, uma por turbina, todas com cluster_id/observed_n consistentes com as tabelas acima.\n")


## ---- Turbina pertencente a um cluster mas SEM curtailments proprios ----
## Bug real do DGY (corrigido 2026-08): cluster_id era inferido a partir
## dos PROPRIOS curtailments da turbina (curtl_cl_dt), nao da sua pertenca
## estrutural ao setor/cluster (cluster_dt) -- uma turbina de interesse sem
## nenhum curtailment proprio (ex: sem cobertura IDF/SCADA, caso de todas
## as turbinas de fatality_incidents no DGY) ficava com cluster_id = NA,
## que por sua vez fazia falhar o merge por (turbine, cluster_id) em
## summarise_turbine_critical_zone() -- a secção "zona critica" do
## relatorio ficava vazia. Dataset isolado (nao usa cluster_dt_test/
## curtl_cl_dt acima) para nao alterar as expectativas ja' documentadas.

manual_test_zero <- list(ClusterC = c("TC1", "TC2"))
cluster_dt_test_zero <- manual_turbine_clusters_dt(manual_test_zero)

curtl_dt_test_zero <- data.table(
  turbine  = c("TC1", "TC1"),  # TC2 nunca aparece -- 0 curtailments proprios
  start    = c(w1, w1),
  track_id = c("zero1", "zero2"),
  species  = "Steppe-Eagle"
)
curtl_cl_dt_zero <- join_curtailments_to_clusters(curtl_dt_test_zero, cluster_dt_test_zero)

marginal_tc2 <- summarise_turbine_marginal_contribution(curtl_cl_dt_zero, "TC2", cluster_dt_test_zero)
cat("\n===== summarise_turbine_marginal_contribution('TC2', 0 curtailments proprios) =====\n")
print(marginal_tc2)
cat(paste(
  "\nEsperado: cluster_id=ClusterC (NAO NA), n_turbines_in_cluster=2, n_total=0,",
  "pct_of_cluster=0 (NAO NA), cluster_rank=1 (unico cluster), n_clusters=1,",
  "median_weekly_pct_of_cluster=0. TC2 nunca teve curtailment proprio, mas",
  "continua a entrar com valores reais -- antes desta correcao, ficava tudo NA.\n"
))

perm_tc2 <- permutation_test_marginal_contribution(curtl_cl_dt_zero, "TC2", cluster_dt_test_zero, n_perm = 999, seed = 1)
cat("\n===== permutation_test_marginal_contribution('TC2', 0 curtailments proprios) =====\n")
print(perm_tc2)
cat(paste(
  "\nEsperado: cluster_id=ClusterC (NAO NA), n_cluster_turbines=2 (TC1+TC2,",
  "nao so' TC1 -- denominador tem de incluir toda a pertenca estrutural do",
  "setor), n_cluster_total=2, observed_n=0, observed_pct=0.0,",
  "expected_pct_uniform=50.0, p_value_gt_uniform=1 (0 e' sempre >= a",
  "qualquer contagem simulada). Antes desta correcao, ficava tudo NA.\n"
))
