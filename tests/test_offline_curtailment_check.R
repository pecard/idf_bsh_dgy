##
## Teste com dados simulados para R/offline_curtailment_check.R e as
## funcoes de cobertura geometrica em R/turbine_idf_coverage.R
## (top_turbines_by_idf(), turbines_by_idf_threshold()).
##
## Correr: source("tests/test_offline_curtailment_check.R")
##
## Depende de: data.table
##

source("R/turbine_idf_coverage.R")
source("R/offline_curtailment_check.R")

## Cobertura geometrica sintetica -- IDF-1 cobre 3 turbinas (90%/70%/20%),
## IDF-2 so' 1 (95%)
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

## turbines_by_idf_threshold() -- limiar de %, NAO um numero fixo de
## turbinas (decisao do Paulo, 2026-09: uma unidade pode ser primaria para
## um numero VARIAVEL de turbinas -- caso real DZH62-04, primaria de 2)
cat("\n===== turbines_by_idf_threshold(coverage_dt_test, min_pct_coverage = 50) =====\n")
thresh_test <- turbines_by_idf_threshold(coverage_dt_test, min_pct_coverage = 50)
print(thresh_test)
cat(sprintf(
  "Esperado: 3 linhas (IDF-1/TOC_A1, IDF-1/TOC_A2, IDF-2/TOC_B1), TOC_A3 (20%%) EXCLUIDO (< 50%%) -- obtido: %d linha(s), TOC_A3 presente: %s\n",
  nrow(thresh_test), "TOC_A3" %in% thresh_test$turbine
))

cat("\n----- Caso-limite: fronteira exata (pct_coverage == min_pct_coverage) e' incluida -----\n")
boundary_test <- turbines_by_idf_threshold(coverage_dt_test, min_pct_coverage = 20)
cat(sprintf(
  "Esperado: TOC_A3 (exatamente 20%%) incluido com limiar=20 (>=, fronteira inclusive) -- obtido: %s\n",
  "TOC_A3" %in% boundary_test$turbine
))

idf_turbines_geo_test <- idf_turbines_from_coverage(coverage_dt_test, min_pct_coverage = 50)
cat(sprintf(
  "idf_turbines_from_coverage(min_pct_coverage=50): %d linha(s), so' colunas idf/turbine: %s\n",
  nrow(idf_turbines_geo_test), identical(names(idf_turbines_geo_test), c("idf", "turbine"))
))

## omissao (min_pct_coverage=20) -- ver a nota completa em
## idf_turbines_from_coverage(), R/offline_curtailment_check.R
idf_turbines_geo_default_test <- idf_turbines_from_coverage(coverage_dt_test)
cat(sprintf(
  "idf_turbines_from_coverage() omissao (min_pct_coverage=20): %d linha(s) (todas as 4, TOC_A3 na fronteira) -- obtido: %d\n",
  4, nrow(idf_turbines_geo_default_test)
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
##   OFF1 (IDF-1, 10:00-11:00) -- curtailment em TOC_A2 as 10:30 (DENTRO), sem SCADA -> "IDF unit communication failure"
##   OFF2 (IDF-1, 12:00-13:00) -- sem curtailment, SCADA com RPM em TOC_A1 as 12:30 -> "Turbine operational, no detection"
##   OFF3 (IDF-2, 14:00-15:00) -- curtailment na fronteira off_start (14:00) -> "IDF unit communication failure"
##   OFF4 (IDF-2, 16:00-17:00) -- curtailment na fronteira off_end (17:00)   -> "IDF unit communication failure"
##   OFF5 (IDF-3, 18:00-19:00) -- unidade sem turbina mapeada, sem curtailment nem SCADA -> "No evidence"
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
## valores em ingles de proposito -- ver nota em classify_offline_evidence(),
## R/offline_curtailment_check.R (xlsx partilhados com o cliente/equipa IDF)
expected_classification <- c(
  "IDF unit communication failure",                 # OFF1 -- curtailment, mesmo sem SCADA
  "Turbine operational, no detection",               # OFF2 -- so' SCADA
  "IDF unit communication failure",                  # OFF3
  "IDF unit communication failure",                  # OFF4
  "No evidence (heartbeat and SCADA both missing)"   # OFF5 -- nenhum sinal
)
cat(sprintf(
  "Resultado: %d/%d classificacoes corretas.\n",
  sum(combined_test$classification == expected_classification), nrow(combined_test)
))

## resolve_idf_turbines() -- helper que decide entre a matriz manual e o
## fallback geometrico, extraido do if/else que estava duplicado em
## explore_offline_curtailment_check.R/IDF_analysis.R/IDF_monthly_report.R
cat("\n===== resolve_idf_turbines() =====\n")

resolve_manual_test <- resolve_idf_turbines(turbine_idf_manual_dt = turbine_idf_manual_test)
cat(sprintf(
  "Com matriz manual: used_geometric_fallback = %s (esperado FALSE), %d linha(s) (esperado 3, igual a idf_turbines_from_manual_matrix()), turbine_idf_coverage_dt NULL: %s (esperado TRUE)\n",
  resolve_manual_test$used_geometric_fallback, nrow(resolve_manual_test$idf_turbines_dt),
  is.null(resolve_manual_test$turbine_idf_coverage_dt)
))

resolve_coverage_test <- resolve_idf_turbines(turbine_idf_coverage_dt = coverage_dt_test, min_pct_coverage = 50)
cat(sprintf(
  "Sem matriz manual, coverage_dt ja' calculada: used_geometric_fallback = %s (esperado TRUE), %d linha(s) (esperado 3, igual a idf_turbines_from_coverage(min_pct_coverage=50)), reaproveita a mesma coverage_dt (nao recalcula): %s (esperado TRUE)\n",
  resolve_coverage_test$used_geometric_fallback, nrow(resolve_coverage_test$idf_turbines_dt),
  identical(resolve_coverage_test$turbine_idf_coverage_dt, coverage_dt_test)
))

resolve_error_test <- tryCatch({
  resolve_idf_turbines()
  "no_error"
}, error = function(e) "error")
cat(sprintf(
  "Sem matriz manual NEM coverage_dt NEM wtg/idf_sf/buffer_m para a calcular: %s (esperado 'error')\n",
  resolve_error_test
))


cat("\n===== summarise_offline_evidence() =====\n")
summary_test <- summarise_offline_evidence(combined_test)
cat("-- by_idf --\n")
print(summary_test$by_idf)
cat("-- overall --\n")
print(summary_test$overall)
cat("Esperado (overall, 60 min por intervalo): 'IDF unit communication failure' n=3/180min, 'Turbine operational, no detection' n=1/60min, 'No evidence (heartbeat and SCADA both missing)' n=1/60min\n")
