##
## Teste com dados simulados para R/curtailment_response.R
##
## Objetivo: perceber exatamente o que o join "nearest" com tolerancia
## (match_nearest_rpm) e a classificacao de resposta/falha
## (classify_curtailment_response) estao a fazer, com dados onde sabemos
## de antemao qual deveria ser o resultado.
##
## Nao usa testthat (nao e uma dependencia do projeto) -- e um script
## normal que constroi dados sinteticos, corre as funcoes reais, e imprime
## uma tabela "esperado vs obtido" para inspecionares.
##
## Correr: source("tests/test_curtailment_response.R")
##
## Depende de: data.table
##

source("R/curtailment_response.R")


##
## PARTE A -- como o join "nearest" com tolerancia esta a funcionar
##

## SCADA com um "buraco" propositado entre t=10s e t=50s (turbina "JOIN-TEST"),
## para testar 3 situacoes: match quase perfeito (sub-segundo), match normal,
## e match dentro do buraco (deve ser rejeitado pela tolerancia).

base_a <- as.POSIXct("2026-01-01 01:00:00", tz = "UTC")

scada_a <- data.table(
  turbinelabel = "JOIN-TEST",
  datetime     = base_a + c(0, 10, 50, 60),
  value        = c(100, 110, 150, 160),
  readingname  = "RPM"
)

events_a <- data.table(
  id         = 1:3,
  turbine    = "JOIN-TEST",
  event_time = c(
    base_a + 0.172,   # E1: 0.172s depois de t=0 -> precisao sub-segundo
    base_a + 25.5,    # E2: dentro do buraco (15.5s do t=10, 24.5s do t=50) -> devia falhar tolerancia (15s)
    base_a + 53        # E3: 3s depois de t=50 -> match normal (evita empate exato com t=60)
  )
)

match_a <- match_nearest_rpm(events_a, scada_a, max_gap_sec = 15)

expected_a <- data.table(
  id = 1:3,
  expected_scada_datetime = c(base_a + 0, NA, base_a + 50),
  expected_rpm            = c(100, NA_real_, 150),
  expected_gap_sec        = c(0.172, 15.5, 3),
  expected_valid_match    = c(TRUE, FALSE, TRUE)
)

check_a <- merge(match_a, expected_a, by = "id")
check_a[, `:=`(
  gap_ok   = abs(gap_sec - expected_gap_sec) < 0.01 | (is.na(gap_sec) & is.na(expected_gap_sec)),
  valid_ok = valid_match == expected_valid_match
)]

cat("\n===== PARTE A: match_nearest_rpm() -- join com tolerancia =====\n")
print(check_a[, .(id, event_time, scada_datetime, rpm, gap_sec, valid_match,
                  expected_gap_sec, expected_valid_match, gap_ok, valid_ok)])
cat(sprintf(
  "\nResultado: %d/%d casos corretos.\n",
  sum(check_a$gap_ok & check_a$valid_ok), nrow(check_a)
))
cat(paste(
  "Nota: E2 cai mesmo no meio do 'buraco' de dados SCADA (entre t=10s e t=50s).",
  "O vizinho mais proximo fica a 15.5s (> tolerancia de 15s), por isso o",
  "match e corretamente marcado como invalido (valid_match=FALSE) em vez de",
  "aceitar silenciosamente um valor de RPM que pode ja nao ser representativo.\n"
))


##
## PARTE B -- deteccao de resposta / falha de resposta (classify_curtailment_response)
##

## 5 turbinas sinteticas, uma por cenario:
##   TEST1 -- responded        (RPM desce abaixo de 1 dentro da janela)
##   TEST2 -- no_response       (RPM nunca desce -- FALHA GRAVE)
##   TEST3 -- already_stopped   (ja estava <1 rpm no sinal de start)
##   TEST4 -- no_data           (sem leituras SCADA nenhumas)
##   TEST5 -- start match invalido, mas a janela ainda tem dados suficientes
##            para classificar (mostra porque a janela e mais robusta que
##            depender so do match "nearest" do sinal de start)

## TEST1: responded, drop aos 40s -----------------------------------------
base1 <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
scada_1 <- data.table(
  turbinelabel = "TEST1",
  datetime     = base1 + seq(0, 80, by = 10),
  value        = c(12, 12, 11, 3, 0.5, 0.3, 0.2, 0.2, 0.2),
  readingname  = "RPM"
)
curtl_1 <- data.table(turbine = "TEST1", track_id = "T1", species = "Golden-Eagle",
                      start = base1 + 0.5, end = base1 + 70)

## TEST2: no_response, RPM nunca desce -------------------------------------
base2 <- as.POSIXct("2026-01-01 00:10:00", tz = "UTC")
scada_2 <- data.table(
  turbinelabel = "TEST2",
  datetime     = base2 + seq(0, 90, by = 10),
  value        = c(13, 12.5, 12.8, 13.2, 12.1, 12.9, 13.5, 12.2, 12.7, 13.1),
  readingname  = "RPM"
)
curtl_2 <- data.table(turbine = "TEST2", track_id = "T2", species = "Steppe-Eagle",
                      start = base2, end = base2 + 90)

## TEST3: already_stopped ---------------------------------------------------
base3 <- as.POSIXct("2026-01-01 00:20:00", tz = "UTC")
scada_3 <- data.table(
  turbinelabel = "TEST3",
  datetime     = base3 + seq(0, 50, by = 10),
  value        = c(0.4, 0.3, 0.35, 0.3, 0.28, 0.25),
  readingname  = "RPM"
)
curtl_3 <- data.table(turbine = "TEST3", track_id = "T3", species = "Egyptian-Vulture",
                      start = base3, end = base3 + 50)

## TEST4: no_data (sem SCADA nenhum para esta turbina) ----------------------
base4 <- as.POSIXct("2026-01-01 00:30:00", tz = "UTC")
scada_4 <- data.table(
  turbinelabel = character(), datetime = as.POSIXct(character(), tz = "UTC"),
  value = numeric(), readingname = character()
)
curtl_4 <- data.table(turbine = "TEST4", track_id = "T4", species = "Saker-Falcon",
                      start = base4, end = base4 + 60)

## TEST5: start match invalido, mas janela recupera -------------------------
base5 <- as.POSIXct("2026-01-01 00:40:00", tz = "UTC")
scada_5 <- data.table(
  turbinelabel = "TEST5",
  datetime     = base5 + c(0, 40, 50),
  value        = c(10, 0.5, 0.3),
  readingname  = "RPM"
)
curtl_5 <- data.table(turbine = "TEST5", track_id = "T5", species = "Golden-Eagle",
                      start = base5 + 18, end = base5 + 60)

scada_all <- rbindlist(list(scada_1, scada_2, scada_3, scada_4, scada_5))
curtl_all <- rbindlist(list(curtl_1, curtl_2, curtl_3, curtl_4, curtl_5))

response_dt <- classify_curtailment_response(
  curtl_all, scada_all,
  monitor_window_sec = 90, rpm_threshold = 1, max_gap_sec = 15
)

expected_b <- data.table(
  turbine = c("TEST1", "TEST2", "TEST3", "TEST4", "TEST5"),
  expected_status         = c("responded", "no_response", "already_stopped", "no_data", "responded"),
  # TEST3: todas as leituras da janela ja estao <1rpm desde o t=0, que
  # coincide com window_start -- por isso o "tempo ate cair" da 0, nao NA
  # (a classificacao fica "already_stopped" na mesma, por prioridade)
  expected_time_to_drop   = c(39.5, NA, 0, NA, 22)
)

check_b <- merge(response_dt, expected_b, by = "turbine")
check_b[, `:=`(
  status_ok = response_status == expected_status,
  time_ok   = (is.na(time_to_drop_sec) & is.na(expected_time_to_drop)) |
              (abs(time_to_drop_sec - expected_time_to_drop) < 0.01)
)]

cat("\n===== PARTE B: classify_curtailment_response() =====\n")
print(check_b[, .(turbine, response_status, expected_status, status_ok,
                  time_to_drop_sec, expected_time_to_drop, time_ok,
                  start_rpm, start_match_valid, n_readings_in_window)])
cat(sprintf(
  "\nResultado: %d/%d cenarios corretos.\n",
  sum(check_b$status_ok & check_b$time_ok), nrow(check_b)
))
cat(paste(
  "Nota sobre TEST5: o sinal de start (00:40:18) fica a 18s da leitura mais",
  "proxima (>tolerancia de 15s), por isso start_match_valid=FALSE e",
  "start_rpm=NA -- nao conseguimos confirmar se ja estava parada. Mas a",
  "janela de monitorizacao [start, start+90s] ainda tem leituras (t=40 e",
  "t=50) que mostram a descida, por isso classify() ainda consegue concluir",
  "'responded'. Isto mostra porque a deteccao usa uma janela de leituras",
  "e nao depende so do match nearest do sinal de start.\n"
))
