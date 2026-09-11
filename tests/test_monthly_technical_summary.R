##
## Teste com dados simulados para R/monthly_technical_summary.R -- os 4
## agregados farm-wide usados na secção "Technical Summary" (sem numeração,
## topo do relatorio mensal). Latencia de resposta e no-response nao
## precisam de teste aqui -- usam summarise_latency() diretamente
## (R/curtailment_response_latency.R), ja' testada indiretamente pelo resto
## do pipeline.
##
## Correr: source("tests/test_monthly_technical_summary.R")
##
## Depende de: data.table
##

source("R/monthly_technical_summary.R")

## 1. summarise_availability_overall() -- 3 unidades IDF, mesmo calendario
## (daylight_mins_total = 1000 em todas), offline_mins_total = 100/50/0 ----
## Esperado: daylight_mins_total = 3000, offline_mins_total = 150,
## offline_pct = round(100*150/3000, 1) = 5.0

cat("\n===== summarise_availability_overall() =====\n")
by_idf_test <- data.table::data.table(
  idf = c("A", "B", "C"),
  daylight_mins_total = c(1000, 1000, 1000),
  offline_mins_total  = c(100, 50, 0)
)
avail_overall_test <- summarise_availability_overall(by_idf_test)
print(avail_overall_test)
cat(sprintf(
  "Esperado: daylight_mins_total=3000, offline_mins_total=150, offline_pct=5.0 -- obtido: %d, %d, %.1f\n",
  avail_overall_test$daylight_mins_total, avail_overall_test$offline_mins_total, avail_overall_test$offline_pct
))


## 2. summarise_curtailment_priority_split() -- caso normal (3 grupos
## presentes) e caso-limite (grupo "nonpriority" ausente esse mes) --------

cat("\n===== summarise_curtailment_priority_split() -- caso normal =====\n")
by_group_test <- data.table::data.table(
  species_group = c("priority", "nonpriority", "other"),
  n = c(80, 15, 5),
  pct_of_total = c(80, 15, 5)
)
split_test <- summarise_curtailment_priority_split(by_group_test)
print(split_test)
cat(sprintf(
  "Esperado: priority_n=80/80%%, nonpriority_n=15/15%% -- obtido: %d/%.0f%%, %d/%.0f%%\n",
  split_test$priority_n, split_test$priority_pct, split_test$nonpriority_n, split_test$nonpriority_pct
))

cat("\n----- Caso-limite: 'nonpriority' ausente (0 curtailments nao-prioritarios este mes) -----\n")
by_group_nonp_test <- data.table::data.table(
  species_group = c("priority", "other"),
  n = c(90, 10),
  pct_of_total = c(90, 10)
)
split_nonp_test <- summarise_curtailment_priority_split(by_group_nonp_test)
print(split_nonp_test)
cat(sprintf(
  "Esperado: nonpriority_n=0, nonpriority_pct=0 (0-preenchido, sem erro) -- obtido: %d, %.0f\n",
  split_nonp_test$nonpriority_n, split_nonp_test$nonpriority_pct
))


## 3. summarise_shutdown_overall() -- 5 eventos no limiar "parado" (1 rpm),
## time_to_threshold_sec = 30,40,50,NA,80 (o NA e' um "nunca atingiu",
## corretamente excluido) -- mais 2 eventos noutro limiar (2 rpm), que
## devem ser ignorados por nao serem o stopped_threshold pedido -----------
## Esperado: mean = mean(30,40,50,80) = 50.0, max = 80.0

cat("\n===== summarise_shutdown_overall() =====\n")
tt_test <- data.table::data.table(
  curtailment_id = c(1, 2, 3, 4, 5, 1, 2),
  threshold = c(1, 1, 1, 1, 1, 2, 2),
  time_to_threshold_sec = c(30, 40, 50, NA, 80, 10, 15)
)
shutdown_overall_test <- summarise_shutdown_overall(tt_test, stopped_threshold = 1)
print(shutdown_overall_test)
cat(sprintf(
  "Esperado: mean_time_sec=50.0, max_time_sec=80.0 -- obtido: %.1f, %.1f\n",
  shutdown_overall_test$mean_time_sec, shutdown_overall_test$max_time_sec
))

cat("\n----- Caso-limite: stopped_threshold sem nenhum evento (nunca testado esse limiar) -----\n")
shutdown_empty_test <- summarise_shutdown_overall(tt_test, stopped_threshold = 0)
print(shutdown_empty_test)
cat(sprintf(
  "Esperado: mean_time_sec=NA, max_time_sec=NA (sem erro) -- obtido: %s, %s\n",
  shutdown_empty_test$mean_time_sec, shutdown_empty_test$max_time_sec
))


## 4. summarise_unnecessary_curtailments() -- caso normal e caso-limite
## (pnp_curtailments_dt vazio, mes sem nenhum track multi-ID) -------------

cat("\n===== summarise_unnecessary_curtailments() =====\n")
pnp_test <- data.table::data.table(
  total_curtailments = 200,
  curtailments_from_multi_id_tracks = 30,
  curtailments_due_to_p_to_np = 12,
  pct_of_total = round(100 * 12 / 200, 1)
)
unnecessary_test <- summarise_unnecessary_curtailments(pnp_test)
print(unnecessary_test)
cat(sprintf(
  "Esperado: n=12, pct_of_total=6.0 -- obtido: %d, %.1f\n",
  unnecessary_test$n, unnecessary_test$pct_of_total
))

cat("\n----- Caso-limite: pnp_curtailments_dt vazio (0 linhas, sem tracks multi-ID este mes) -----\n")
pnp_empty_test <- data.table::data.table(
  total_curtailments = integer(), curtailments_from_multi_id_tracks = integer(),
  curtailments_due_to_p_to_np = integer(), pct_of_total = numeric()
)
unnecessary_empty_test <- summarise_unnecessary_curtailments(pnp_empty_test)
print(unnecessary_empty_test)
cat(sprintf(
  "Esperado: n=0, pct_of_total=NA (0-preenchido, sem erro) -- obtido: %d, %s\n",
  unnecessary_empty_test$n, unnecessary_empty_test$pct_of_total
))
