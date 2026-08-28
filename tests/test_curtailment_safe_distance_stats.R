##
## Teste com dados simulados para a seccao 5 de R/curtailment_safe_distance.R
## (wilson_ci, summarise_safe_distance_ci, summarise_safe_distance_by_month,
## test_safe_distance_trend, test_safe_distance_by_species)
##
## Mesma logica dos outros testes: dados sinteticos com resultado calculado
## a mao, comparados com o que as funcoes reais devolvem.
##
## Correr: source("tests/test_curtailment_safe_distance_stats.R")
##
## Depende de: data.table
##

source("R/curtailment_response.R")
source("R/curtailment_shutdown_time.R")
source("R/curtailment_safe_distance.R")


## 1. wilson_ci() -- verificacao a mao contra um caso simples (n=20, n_ok=15) --

cat("\n===== wilson_ci(n_event = 15, n = 20) =====\n")
ci_hand <- wilson_ci(15, 20)
print(ci_hand)
cat("Esperado (calculo a mao, z=1.959964): ci_low_pct ~= 53.1, ci_high_pct ~= 88.8\n")


## 2. Cenario sintetico para CI/mes/tendencia/teste entre especies -----------
##
## Especie A (Golden-Eagle_test): pct_ok sobe claramente mes a mes --
##   2026-01: 5 OK / 5 Crit (50%), 2026-02: 7 OK / 3 Crit (70%),
##   2026-03: 9 OK / 1 Crit (90%) -- tendencia de melhoria clara, testavel
## Especie B (Kestrel_test): pct_ok baixo e estavel nos 3 meses (20%, 6/30
##   no total) -- para contrastar com a especie A no teste entre especies
##

make_events <- function(species, month_start, n_ok, n_crit) {
  data.table::data.table(
    species = species,
    status  = c(rep("OK", n_ok), rep("Crit", n_crit)),
    start   = month_start + (seq_len(n_ok + n_crit) * 3600) # espacados 1h dentro do mes, irrelevante para o teste
  )
}

safe_dist_dt_test <- data.table::rbindlist(list(
  make_events("Golden-Eagle_test", as.POSIXct("2026-01-05", tz = "UTC"), 5, 5),
  make_events("Golden-Eagle_test", as.POSIXct("2026-02-05", tz = "UTC"), 7, 3),
  make_events("Golden-Eagle_test", as.POSIXct("2026-03-05", tz = "UTC"), 9, 1),
  make_events("Kestrel_test",      as.POSIXct("2026-01-05", tz = "UTC"), 2, 8),
  make_events("Kestrel_test",      as.POSIXct("2026-02-05", tz = "UTC"), 2, 8),
  make_events("Kestrel_test",      as.POSIXct("2026-03-05", tz = "UTC"), 2, 8)
))


## 2a. summarise_safe_distance_ci() -- farm-wide e por especie ---------------

cat("\n===== summarise_safe_distance_ci(safe_dist_dt_test) -- farm-wide =====\n")
ci_overall_test <- summarise_safe_distance_ci(safe_dist_dt_test)
print(ci_overall_test)
cat("Esperado: n=60, n_ok=27 (21+6), pct_ok=45.\n")

cat("\n===== summarise_safe_distance_ci(safe_dist_dt_test, by_species = TRUE) =====\n")
ci_species_test <- summarise_safe_distance_ci(safe_dist_dt_test, by_species = TRUE)
print(ci_species_test)
cat("Esperado: Golden-Eagle_test n=30, n_ok=21, pct_ok=70; Kestrel_test n=30, n_ok=6, pct_ok=20.\n")


## 2b. summarise_safe_distance_by_month() ------------------------------------

cat("\n===== summarise_safe_distance_by_month(safe_dist_dt_test[species == 'Golden-Eagle_test']) =====\n")
by_month_test <- summarise_safe_distance_by_month(safe_dist_dt_test[species == "Golden-Eagle_test"])
print(by_month_test)
cat("Esperado: 3 meses (2026-01-01, 2026-02-01, 2026-03-01), pct_ok = 50, 70, 90 (ordem crescente).\n")


## 2c. test_safe_distance_trend() -- deve detetar melhoria clara ------------

cat("\n===== test_safe_distance_trend(safe_dist_dt_test[species == 'Golden-Eagle_test']) =====\n")
trend_test <- test_safe_distance_trend(safe_dist_dt_test[species == "Golden-Eagle_test"])
print(trend_test)
cat(sprintf(
  "Esperado: direction == 'improving' -- %s\n",
  if (identical(trend_test$direction, "improving")) "OK" else "FALHOU"
))

cat("\n===== test_safe_distance_trend() -- dados insuficientes (so' 1 mes) =====\n")
trend_insuf <- test_safe_distance_trend(safe_dist_dt_test[species == "Golden-Eagle_test" & start < as.POSIXct("2026-02-01", tz = "UTC")])
print(trend_insuf)
cat(sprintf(
  "Esperado: direction == 'insufficient_data' -- %s\n",
  if (identical(trend_insuf$direction, "insufficient_data")) "OK" else "FALHOU"
))


## 2d. test_safe_distance_by_species() -- Golden-Eagle_test (70% OK) vs
## Kestrel_test (20% OK) deve ser detetado como significativamente diferente -

cat("\n===== test_safe_distance_by_species(safe_dist_dt_test) =====\n")
species_test <- test_safe_distance_by_species(safe_dist_dt_test)
print(species_test$table)
cat(sprintf(
  "p_value = %.5f (esperado < 0.05, diferenca clara entre 70%% e 20%% OK) -- %s\n",
  species_test$p_value,
  if (!is.na(species_test$p_value) && species_test$p_value < 0.05) "OK" else "FALHOU"
))

cat("\n===== test_safe_distance_by_species() -- so' 1 especie com >= min_n (dados insuficientes) =====\n")
species_insuf <- test_safe_distance_by_species(safe_dist_dt_test[species == "Golden-Eagle_test"])
print(species_insuf)
cat(sprintf(
  "Esperado: method == 'insufficient_data' -- %s\n",
  if (identical(species_insuf$method, "insufficient_data")) "OK" else "FALHOU"
))
