##
## Teste com dados simulados para R/offline_curtailment_check.R
##
## Correr: source("tests/test_offline_curtailment_check.R")
##
## Depende de: data.table
##

source("R/offline_curtailment_check.R")

## Matriz manual turbina<->IDF sintetica -- mesmo formato lido do xlsx
## (colunas "Turbine ID"/"Primary IDF"), 2 turbinas na mesma unidade IDF-1
## (para testar que um curtailment em QUALQUER uma delas conta), 1 turbina
## sem unidade IDF atribuida (NA -- caso-limite de "sem dados")
turbine_idf_manual_test <- data.table::data.table(
  `Turbine ID`  = c("TOC_A1", "TOC_A2", "TOC_B1", "TOC_ORPHAN"),
  `Primary IDF` = c("IDF-1", "IDF-1", "IDF-2", NA_character_)
)

## 5 intervalos offline sinteticos, cobrindo os casos relevantes:
##   OFF1 (IDF-1, 10:00-11:00) -- 1 curtailment em TOC_A2 as 10:30 (DENTRO) -> TRUE
##   OFF2 (IDF-1, 12:00-13:00) -- 1 curtailment em TOC_A1 as 11:30 (ANTES)  -> FALSE
##   OFF3 (IDF-2, 14:00-15:00) -- curtailment exatamente no off_start (14:00, fronteira >=) -> TRUE
##   OFF4 (IDF-2, 16:00-17:00) -- curtailment exatamente no off_end (17:00, fronteira <=)   -> TRUE
##   OFF5 (IDF-3, 18:00-19:00) -- unidade IDF SEM turbina na matriz (nao e' IDF-1/IDF-2)    -> FALSE, n=0
offline_dt_test <- data.table::data.table(
  idf       = c("IDF-1", "IDF-1", "IDF-2", "IDF-2", "IDF-3"),
  off_start = as.POSIXct(c("2026-06-01 10:00:00", "2026-06-01 12:00:00", "2026-06-01 14:00:00", "2026-06-01 16:00:00", "2026-06-01 18:00:00"), tz = "UTC"),
  off_end   = as.POSIXct(c("2026-06-01 11:00:00", "2026-06-01 13:00:00", "2026-06-01 15:00:00", "2026-06-01 17:00:00", "2026-06-01 19:00:00"), tz = "UTC")
)

curtl_dt_test <- data.table::data.table(
  turbine = c("TOC_A2", "TOC_A1", "TOC_B1", "TOC_B1"),
  start   = as.POSIXct(c("2026-06-01 10:30:00", "2026-06-01 11:30:00", "2026-06-01 14:00:00", "2026-06-01 17:00:00"), tz = "UTC")
)

cat("\n===== check_offline_curtailment_overlap() =====\n")
checked_test <- check_offline_curtailment_overlap(offline_dt_test, curtl_dt_test, turbine_idf_manual_test)
print(checked_test)

expected_has_curtailment <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
expected_n <- c(1L, 0L, 1L, 1L, 0L)
cat(sprintf(
  "Esperado has_curtailment: OFF1=T (dentro), OFF2=F (curtailment antes do intervalo), OFF3=T (fronteira off_start), OFF4=T (fronteira off_end), OFF5=F (unidade sem turbina na matriz) -- obtido: %s\n",
  paste(checked_test$has_curtailment, collapse = ", ")
))
cat(sprintf(
  "Resultado: %d/%d intervalos com has_curtailment correto, %d/%d com n_curtailments_during_offline correto.\n",
  sum(checked_test$has_curtailment == expected_has_curtailment), nrow(checked_test),
  sum(checked_test$n_curtailments_during_offline == expected_n), nrow(checked_test)
))


## Caso-limite: nenhum curtailment em curtl_dt (mes sem nenhum evento) --
## todos os intervalos devem ficar has_curtailment = FALSE, sem erro -------

cat("\n----- Caso-limite: curtl_dt vazio -----\n")
curtl_empty_test <- data.table::data.table(turbine = character(), start = as.POSIXct(character()))
checked_empty_test <- check_offline_curtailment_overlap(offline_dt_test, curtl_empty_test, turbine_idf_manual_test)
cat(sprintf(
  "Esperado: 5 intervalos, todos has_curtailment=FALSE -- obtido: %d intervalos, %d com has_curtailment=TRUE\n",
  nrow(checked_empty_test), sum(checked_empty_test$has_curtailment)
))


## summarise_offline_curtailment_overlap() -- usando o resultado principal
## (checked_test): 3 intervalos "Falha de comunicação" (OFF1/OFF3/OFF4, 1h
## cada = 180 min), 2 "Indisponibilidade genuína" (OFF2/OFF5, 1h cada = 120
## min) -- 300 min no total, 60% comunicação / 40% genuina ------------------

cat("\n===== summarise_offline_curtailment_overlap() =====\n")
summary_test <- summarise_offline_curtailment_overlap(checked_test)
cat("-- by_idf --\n")
print(summary_test$by_idf)
cat("-- overall --\n")
print(summary_test$overall)
cat(sprintf(
  "Esperado (overall): 'Falha de comunicação da unidade IDF' n=3/180min/60%%, 'Indisponibilidade genuína' n=2/120min/40%%\n"
))
