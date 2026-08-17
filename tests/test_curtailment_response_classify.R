##
## Teste com dados simulados para R/curtailment_response_classify.R
##
## Objetivo: confirmar que classify_response_flag() distingue corretamente
## "missed" (CONFIRMADO que a turbina nao parou) de "no_data" (NAO SABEMOS,
## por falta de leitura SCADA fiavel perto do sinal) -- ate 2026-08 os dois
## eram somados na mesma categoria "missed", o que inflacionava essa
## contagem so por falta de cobertura SCADA (foi assim que reparamos:
## fatality_global_response_summary_dt mostrava ~75% "missed" na T54, um
## numero demasiado alto para ser credivel dado o que ja sabiamos do projeto
## -- so ~21-29% dos curtailments tem baseline RPM fiavel, ver relatorio).
##
## Testa tambem "delayed" vs "ok", e faz um sweep de start_end_gap_sec e
## drop_pct_threshold para perceber a sensibilidade real da classificacao a
## estes 2 parametros (a pergunta do Paulo: sera que os 10%/2s estao
## demasiado apertados?).
##
## Nao usa testthat -- script normal que constroi dados sinteticos, corre as
## funcoes reais, e imprime "esperado vs obtido" para inspecionares.
##
## Correr: source("tests/test_curtailment_response_classify.R")
##
## Depende de: data.table
##

source("R/curtailment_response.R")
source("R/curtailment_shutdown_time.R")
source("R/curtailment_response_classify.R")


##
## PARTE A -- classificacao com os parametros do projeto (2s, 10%, 50s)
##

## TESTCL1 -- ok: match valido no start e no end, resposta rapida e limpa
base1 <- as.POSIXct("2026-04-01 00:00:00", tz = "UTC")
scada_1 <- data.table(
  turbinelabel = "TESTCL1",
  datetime     = base1 + seq(0, 40, by = 10),
  value        = c(12, 2, 0.8, 0.4, 0.3),
  readingname  = "RPM"
)
curtl_1 <- data.table(turbine = "TESTCL1", track_id = "TCL1", species = "Golden-Eagle",
                      start = base1 + 0.3, end = base1 + 40.2)

## TESTCL2 -- missed CONFIRMADO: match valido no start e no end, RPM nunca desce
base2 <- as.POSIXct("2026-04-01 00:10:00", tz = "UTC")
scada_2 <- data.table(
  turbinelabel = "TESTCL2",
  datetime     = base2 + seq(0, 40, by = 10),
  value        = c(13, 12.8, 13.1, 12.9, 13),
  readingname  = "RPM"
)
curtl_2 <- data.table(turbine = "TESTCL2", track_id = "TCL2", species = "Steppe-Eagle",
                      start = base2 + 0.2, end = base2 + 40.3)

## TESTCL3 -- delayed: para, mas so' bem depois de shutdown_high_cut_sec (50s)
base3 <- as.POSIXct("2026-04-01 00:20:00", tz = "UTC")
scada_3 <- data.table(
  turbinelabel = "TESTCL3",
  datetime     = base3 + seq(0, 90, by = 10),
  value        = c(12, 11.5, 11, 10.5, 10, 9, 6, 3, 1.5, 0.5),
  readingname  = "RPM"
)
curtl_3 <- data.table(turbine = "TESTCL3", track_id = "TCL3", species = "Egyptian-Vulture",
                      start = base3 + 0.2, end = base3 + 90.3)

## TESTCL4 -- no_data: ha SCADA para a turbina, mas a leitura mais proxima
##            fica a 3s do sinal de start E do sinal de end (start_end_gap_sec
##            por omissao e' 2s) -- NAO deve ser "missed", porque nao sabemos
##            o que aconteceu, so' que a amostragem SCADA nao calhou perto
base4 <- as.POSIXct("2026-04-01 00:30:00", tz = "UTC")
scada_4 <- data.table(
  turbinelabel = "TESTCL4",
  datetime     = base4 + c(0, 40),
  value        = c(12, 0.3),
  readingname  = "RPM"
)
curtl_4 <- data.table(turbine = "TESTCL4", track_id = "TCL4", species = "Saker-Falcon",
                      start = base4 + 3, end = base4 + 43)

## TESTCL5 -- already_stopped: ja <1rpm no sinal de start, match apertado valido
base5 <- as.POSIXct("2026-04-01 00:40:00", tz = "UTC")
scada_5 <- data.table(
  turbinelabel = "TESTCL5",
  datetime     = base5 + c(0, 10, 20),
  value        = c(0.4, 0.35, 0.3),
  readingname  = "RPM"
)
curtl_5 <- data.table(turbine = "TESTCL5", track_id = "TCL5", species = "Golden-Eagle",
                      start = base5 + 0.2, end = base5 + 20.3)

scada_all <- rbindlist(list(scada_1, scada_2, scada_3, scada_4, scada_5))
curtl_all <- rbindlist(list(curtl_1, curtl_2, curtl_3, curtl_4, curtl_5))

response_dt <- classify_response_flag(
  curtl_all, scada_all,
  start_end_gap_sec = 2, max_next_gap_sec = 20, drop_pct_threshold = 0.10,
  rpm_threshold = 1, shutdown_thresholds = c(2, 1, 0), shutdown_high_cut_sec = 50
)

expected_a <- data.table(
  turbine       = c("TESTCL1", "TESTCL2", "TESTCL3", "TESTCL4", "TESTCL5"),
  expected_flag = c("ok",      "missed",  "delayed", "no_data", "ok")
)

check_a <- merge(response_dt, expected_a, by = "turbine")
check_a[, flag_ok := response_flag == expected_flag]

cat("\n===== PARTE A: classify_response_flag() -- casos base =====\n")
print(check_a[, .(turbine, final_status, time_to_first_threshold_sec, response_flag, expected_flag, flag_ok)])
cat(sprintf("\nResultado: %d/%d casos corretos.\n", sum(check_a$flag_ok), nrow(check_a)))
cat(paste(
  "Nota sobre TESTCL4: a leitura SCADA mais proxima do sinal de start E do",
  "sinal de end fica a 3s de distancia (> start_end_gap_sec=2s), por isso",
  "final_status='no_data' e response_flag='no_data' -- NAO 'missed'. Esta e'",
  "exatamente a distincao que faltava antes desta correcao: no_data e",
  "missed eram somados na mesma categoria, inflacionando 'missed' com casos",
  "onde so faltava uma leitura SCADA suficientemente perto, nao uma falha",
  "confirmada da turbina.\n"
))


##
## PARTE B -- sensibilidade a start_end_gap_sec (a pergunta do Paulo: 2s e'
## demasiado apertado?)
##
## TESTCL4 tem a leitura mais proxima a 3s de start E de end -- corremos a
## mesma turbina com varias tolerancias para ver a partir de que valor o
## match passa a ser aceite e a classificacao muda.

sweep_gap <- rbindlist(lapply(c(1, 2, 3, 5, 10, 20), function(gap) {
  r <- classify_response_flag(
    curtl_4, scada_4, start_end_gap_sec = gap, max_next_gap_sec = 20,
    drop_pct_threshold = 0.10, rpm_threshold = 1,
    shutdown_thresholds = c(2, 1, 0), shutdown_high_cut_sec = 50
  )
  data.table(start_end_gap_sec = gap, final_status = r$final_status, response_flag = r$response_flag)
}))

cat("\n===== PARTE B: sensibilidade de TESTCL4 a start_end_gap_sec =====\n")
print(sweep_gap)
cat(paste(
  "\nNota: a leitura mais proxima do sinal fica a 3s -- por isso a",
  "classificacao muda exatamente em start_end_gap_sec=3 (no_data -> ok).",
  "O valor atual do projeto (2s) e' mais apertado do que a cadencia tipica",
  "de leitura do SCADA (~10s, o mesmo valor usado em max_next_gap_sec), por",
  "isso e' esperado que uma fracao GRANDE dos curtailments caia em no_data",
  "so por causa disso -- start_end_gap_sec e' o parametro que mais afeta",
  "diretamente esta contagem, mais do que drop_pct_threshold (ver Parte C).",
  "Isto nao invalida a escolha de 2s -- e' deliberadamente conservadora,",
  "para preferir 'nao sabemos' a assumir um RPM que pode ja nao ser",
  "representativo -- mas explica a magnitude do numero que estranhaste, e",
  "e' o parametro a rever primeiro se decidirmos alarga-lo.\n"
))


##
## PARTE C -- sensibilidade a drop_pct_threshold
##
## response_flag() so usa final_status (start/end) e
## time_to_first_threshold_sec -- NUNCA no_immediate_response/immediate_drop_pct
## (o unico sinal que depende de drop_pct_threshold). Confirma-se aqui que
## variar os 10% nao muda a contagem de missed/no_data/delayed/ok neste
## conjunto -- ou seja, o parametro que o Paulo suspeitava (10%) nao e' o
## que esta a gerar os numeros altos.
##

sweep_drop <- rbindlist(lapply(c(0.05, 0.10, 0.15, 0.20), function(pct) {
  r <- classify_response_flag(
    curtl_all, scada_all, start_end_gap_sec = 2, max_next_gap_sec = 20,
    drop_pct_threshold = pct, rpm_threshold = 1,
    shutdown_thresholds = c(2, 1, 0), shutdown_high_cut_sec = 50
  )
  counts <- r[, .(n_flags = .N), by = response_flag]
  counts[, drop_pct_threshold := pct]
  counts[]
}))
sweep_drop_wide <- dcast(sweep_drop, drop_pct_threshold ~ response_flag, value.var = "n_flags", fill = 0)

cat("\n===== PARTE C: sensibilidade a drop_pct_threshold (5 turbinas da Parte A) =====\n")
print(sweep_drop_wide)
cat(paste(
  "\nNota: as colunas nao mudam entre linhas -- confirma que",
  "drop_pct_threshold nao afeta response_flag() neste conjunto, porque essa",
  "funcao nunca olha para no_immediate_response/immediate_drop_pct. Ajustar",
  "os 10% teria efeito no sinal 'resposta imediata' (usado noutras tabelas,",
  "ex: assess_dt$no_immediate_response), mas NAO na contagem de",
  "missed/no_data/delayed que estranhaste -- essa depende de",
  "start_end_gap_sec e shutdown_high_cut_sec (Parte B), nao deste parametro.\n"
))
