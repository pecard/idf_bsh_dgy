##
## Teste com dados simulados para R/offline_curtailment_check.R e a nova
## funcao top_turbines_by_idf(), R/turbine_idf_coverage.R.
##
## Correr: source("tests/test_offline_curtailment_check.R")
##
## Depende de: data.table
##

source("R/turbine_idf_coverage.R")
source("R/offline_curtailment_check.R")

## Cobertura geometrica sintetica -- IDF-1 cobre 3 turbinas (90%/70%/20%),
## IDF-2 so' 1 (95%) -- top_turbines_by_idf(n=2) deve manter so' as 2
## melhores de IDF-1 (excluir TOC_A3, 20%)
coverage_dt_test <- data.table::data.table(
  turbine = c("TOC_A1", "TOC_A2", "TOC_A3", "TOC_B1"),
  idf     = c("IDF-1", "IDF-1", "IDF-1", "IDF-2"),
  pct_coverage = c(90, 70, 20, 95)
)

cat("\n===== top_turbines_by_idf(coverage_dt_test, n = 2) =====\n")
top2_test <- top_turbines_by_idf(coverage_dt_test, n = 2)
print(top2_test)
cat(sprintf(
  "Esperado: 3 linhas (IDF-1/TOC_A1/rank1, IDF-1/TOC_A2/rank2, IDF-2/TOC_B1/rank1), TOC_A3 (20%%) EXCLUIDO -- obtido: %d linha(s), TOC_A3 presente: %s\n",
  nrow(top2_test), "TOC_A3" %in% top2_test$turbine
))

idf_turbines_geo_test <- idf_turbines_from_coverage(coverage_dt_test, n = 2)
cat(sprintf(
  "idf_turbines_from_coverage(): %d linha(s), so' colunas idf/turbine: %s\n",
  nrow(idf_turbines_geo_test), identical(names(idf_turbines_geo_test), c("idf", "turbine"))
))


## Matriz manual turbina<->IDF sintetica (fonte alternativa) -- mesma
## atribuicao Primary do exemplo geometrico acima, + 1 turbina orfa (sem
## unidade IDF atribuida, caso-limite de "sem dados")
turbine_idf_manual_test <- data.table::data.table(
  `Turbine ID`  = c("TOC_A1", "TOC_A2", "TOC_B1", "TOC_ORPHAN"),
  `Primary IDF` = c("IDF-1", "IDF-1", "IDF-2", NA_character_)
)
idf_turbines_manual_test <- idf_turbines_from_manual_matrix(turbine_idf_manual_test)
cat(sprintf(
  "\nidf_turbines_from_manual_matrix(): %d linha(s) (TOC_ORPHAN excluido, sem Primary IDF) -- obtido: %d\n",
  3, nrow(idf_turbines_manual_test)
))


## 5 intervalos offline sinteticos, cobrindo os casos relevantes:
##   OFF1 (IDF-1, 10:00-11:00) -- curtailment em TOC_A2 as 10:30 (DENTRO), sem SCADA -> "Falha de comunicação"
##   OFF2 (IDF-1, 12:00-13:00) -- sem curtailment, SCADA com RPM em TOC_A1 as 12:30 -> "Turbina operacional sem deteção"
##   OFF3 (IDF-2, 14:00-15:00) -- curtailment na fronteira off_start (14:00) -> "Falha de comunicação"
##   OFF4 (IDF-2, 16:00-17:00) -- curtailment na fronteira off_end (17:00)   -> "Falha de comunicação"
##   OFF5 (IDF-3, 18:00-19:00) -- unidade sem turbina mapeada, sem curtailment nem SCADA -> "Sem evidência"
offline_dt_test <- data.table::data.table(
  idf       = c("IDF-1", "IDF-1", "IDF-2", "IDF-2", "IDF-3"),
  off_start = as.POSIXct(c("2026-06-01 10:00:00", "2026-06-01 12:00:00", "2026-06-01 14:00:00", "2026-06-01 16:00:00", "2026-06-01 18:00:00"), tz = "UTC"),
  off_end   = as.POSIXct(c("2026-06-01 11:00:00", "2026-06-01 13:00:00", "2026-06-01 15:00:00", "2026-06-01 17:00:00", "2026-06-01 19:00:00"), tz = "UTC")
)

curtl_dt_test <- data.table::data.table(
  turbine = c("TOC_A2", "TOC_A1", "TOC_B1", "TOC_B1"),
  start   = as.POSIXct(c("2026-06-01 10:30:00", "2026-06-01 11:30:00", "2026-06-01 14:00:00", "2026-06-01 17:00:00"), tz = "UTC")
)

## RPM so' para TOC_A1, dentro do OFF2 -- deliberadamente SEM nenhuma
## leitura SCADA para OFF1 (mesmo tendo curtailment), para testar que
## classify_offline_evidence() da' prioridade ao curtailment mesmo quando o
## SCADA tambem esta em falta (caso real, DGY 2026-07)
scada_dt_test <- data.table::data.table(
  turbinelabel = c("TOC_A1"),
  datetime     = as.POSIXct(c("2026-06-01 12:30:00"), tz = "UTC"),
  readingname  = c("RPM"),
  value        = c(5)
)

cat("\n===== check_offline_curtailment_overlap() =====\n")
curtl_checked_test <- check_offline_curtailment_overlap(offline_dt_test, curtl_dt_test, idf_turbines_manual_test)
print(curtl_checked_test[, .(idf, off_start, off_end, n_curtailments_during_offline, has_curtailment)])
expected_has_curtailment <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
cat(sprintf(
  "Esperado has_curtailment: %s -- obtido: %s\n",
  paste(expected_has_curtailment, collapse = ", "), paste(curtl_checked_test$has_curtailment, collapse = ", ")
))

cat("\n===== check_offline_scada_presence() =====\n")
scada_checked_test <- check_offline_scada_presence(offline_dt_test, scada_dt_test, idf_turbines_manual_test)
print(scada_checked_test[, .(idf, off_start, off_end, has_scada_rpm)])
expected_has_scada <- c(FALSE, TRUE, FALSE, FALSE, FALSE)
cat(sprintf(
  "Esperado has_scada_rpm: %s -- obtido: %s\n",
  paste(expected_has_scada, collapse = ", "), paste(scada_checked_test$has_scada_rpm, collapse = ", ")
))

cat("\n===== classify_offline_evidence() =====\n")
combined_test <- classify_offline_evidence(curtl_checked_test, scada_checked_test)
print(combined_test[, .(idf, off_start, has_curtailment, has_scada_rpm, classification)])
expected_classification <- c(
  "Falha de comunicação da unidade IDF",   # OFF1 -- curtailment, mesmo sem SCADA
  "Turbina operacional sem deteção",       # OFF2 -- so' SCADA
  "Falha de comunicação da unidade IDF",   # OFF3
  "Falha de comunicação da unidade IDF",   # OFF4
  "Sem evidência (heartbeat e SCADA em falta)" # OFF5 -- nenhum sinal
)
cat(sprintf(
  "Resultado: %d/%d classificacoes corretas.\n",
  sum(combined_test$classification == expected_classification), nrow(combined_test)
))

cat("\n===== summarise_offline_evidence() =====\n")
summary_test <- summarise_offline_evidence(combined_test)
cat("-- by_idf --\n")
print(summary_test$by_idf)
cat("-- overall --\n")
print(summary_test$overall)
cat("Esperado (overall, 60 min por intervalo): 'Falha de comunicação da unidade IDF' n=3/180min, 'Turbina operacional sem deteção' n=1/60min, 'Sem evidência (heartbeat e SCADA em falta)' n=1/60min\n")
