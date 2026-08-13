##
## Teste com dados simulados para R/curtailment_safe_distance.R
##
## Mesma logica dos outros testes: dados sinteticos com resultado calculado
## a mao, comparados com o que as funcoes reais devolvem.
##
## Correr: source("tests/test_curtailment_safe_distance.R")
##
## Depende de: data.table
##

source("R/curtailment_response.R")
source("R/curtailment_shutdown_time.R")
source("R/curtailment_safe_distance.R")


##
## Cenarios sinteticos:
##   TDA1 -- "OK": turbina para depressa (29.8s ate 2rpm), ave lenta e longe
##                 -- margem de seguranca positiva
##   TDB1 -- "Crit": turbina demora a parar (49.9s ate 2rpm), ave rapida e perto
##                   -- margem de seguranca negativa (curtailment atrasado)
##   TDC1 -- match do start invalido (4s de distancia, > tolerancia de 2s)
##           -- deve ficar de fora do resultado (excluido em time_to_rpm_thresholds)
##   TDD1 -- turbina nunca atinge 2rpm dentro da janela -- tempo/distancia/status = NA
##

## TDA1 -- "OK" -------------------------------------------------------------
base_a <- as.POSIXct("2026-04-01 00:00:00", tz = "UTC")
scada_a <- data.table(
  turbinelabel = "TESTS1",
  datetime     = base_a + c(0, 10, 20, 30),
  value        = c(12, 8, 3, 1.5),
  readingname  = "RPM"
)
curtl_a <- data.table(turbine = "TESTS1", track_id = "TSA1", species = "Golden-Eagle",
                      start = base_a + 0.2, end = base_a + 60)
track_a <- data.table(
  track_id = "TSA1",
  dist     = 1000,                    # x2d (so o 1o valor e usado)
  speed_ms = c(8, 9, 10, 11, 50)       # 50 e outlier (excluido pelo percentil 95)
)
## avg_speed esperado: quantile(c(8,9,10,11,50), .95) = 42.2 -> filtra <42.2 -> mean(8,9,10,11) = 9.5
## time_to_2rpm esperado: 1a leitura <=2rpm dentro de [start=+0.2, end] e +30 (1.5rpm) -> 30 - 0.2 = 29.8s
## min_safe_dist = 29.8 * 9.5 = 283.1 | margin = 1000 - 283.1 = 716.9 -> "OK"

## TDB1 -- "Crit" -------------------------------------------------------------
base_b <- as.POSIXct("2026-04-01 00:10:00", tz = "UTC")
scada_b <- data.table(
  turbinelabel = "TESTS2",
  datetime     = base_b + c(0, 10, 20, 30, 40, 50),
  value        = c(12, 10, 8, 6, 4, 1.8),
  readingname  = "RPM"
)
curtl_b <- data.table(turbine = "TESTS2", track_id = "TSB1", species = "Steppe-Eagle",
                      start = base_b + 0.1, end = base_b + 80)
track_b <- data.table(
  track_id = "TSB1",
  dist     = 300,                      # x2d
  speed_ms = c(20, 21, 22, 23, 90)      # 90 e outlier
)
## avg_speed esperado: quantile(c(20,21,22,23,90), .95) = 76.6 -> filtra <76.6 -> mean(20,21,22,23) = 21.5
## time_to_2rpm esperado: 1a leitura <=2rpm e +50 (1.8rpm) -> 50 - 0.1 = 49.9s
## min_safe_dist = 49.9 * 21.5 = 1072.85 | margin = 300 - 1072.85 = -772.85 -> "Crit"

## TDC1 -- start match invalido (excluido) ------------------------------------
base_c <- as.POSIXct("2026-04-01 00:20:00", tz = "UTC")
scada_c <- data.table(
  turbinelabel = "TESTS3",
  datetime     = base_c + c(0, 10),
  value        = c(10, 1),
  readingname  = "RPM"
)
curtl_c <- data.table(turbine = "TESTS3", track_id = "TSC1", species = "Egyptian-Vulture",
                      start = base_c + 4, end = base_c + 20)
track_c <- data.table(track_id = "TSC1", dist = 500, speed_ms = c(10, 11, 12))

## TDD1 -- nunca atinge 2rpm dentro da janela ---------------------------------
base_d <- as.POSIXct("2026-04-01 00:30:00", tz = "UTC")
scada_d <- data.table(
  turbinelabel = "TESTS4",
  datetime     = base_d + c(0, 10, 20),
  value        = c(12, 11, 10.5),
  readingname  = "RPM"
)
curtl_d <- data.table(turbine = "TESTS4", track_id = "TSD1", species = "Saker-Falcon",
                      start = base_d + 0.15, end = base_d + 20.2)
track_d <- data.table(track_id = "TSD1", dist = 500, speed_ms = c(15, 16, 17))
## avg_speed esperado (calculado independentemente do tempo/status, que ficam NA):
## quantile(c(15,16,17), .95) = 16.9 -> filtra <16.9 -> mean(15,16) = 15.5

## TDE1 -- "OK", MESMA especie do TDA1 (Golden-Eagle) -- testa agregacao de
## 2 curtailments para a mesma especie em summarise_safe_distance() --------
base_e <- as.POSIXct("2026-04-01 00:40:00", tz = "UTC")
scada_e <- data.table(
  turbinelabel = "TESTS5",
  datetime     = base_e + c(0, 10),
  value        = c(10, 1.8),
  readingname  = "RPM"
)
curtl_e <- data.table(turbine = "TESTS5", track_id = "TSE1", species = "Golden-Eagle",
                      start = base_e + 0.1, end = base_e + 20)
track_e <- data.table(track_id = "TSE1", dist = 200, speed_ms = c(5, 6, 7))
## avg_speed esperado: quantile(c(5,6,7), .95) = 6.9 -> filtra <6.9 -> mean(5,6) = 5.5
## time_to_2rpm esperado: 1a leitura <=2rpm e +10 (1.8rpm) -> 10 - 0.1 = 9.9s
## min_safe_dist = 9.9 * 5.5 = 54.45 | margin = 200 - 54.45 = 145.55 -> "OK"

## TDF1 -- "Crit", especie NAO prioritaria (Kestrel) -- testa que o filtro
## species_sel = prioritysp em summarise_safe_distance() a exclui do resumo,
## mas continua presente na tabela detalhada (compute_safe_distance) --------
base_f <- as.POSIXct("2026-04-01 00:50:00", tz = "UTC")
scada_f <- data.table(
  turbinelabel = "TESTS6",
  datetime     = base_f + c(0, 15),
  value        = c(9, 1),
  readingname  = "RPM"
)
curtl_f <- data.table(turbine = "TESTS6", track_id = "TSF1", species = "Kestrel",
                      start = base_f + 0.1, end = base_f + 25)
track_f <- data.table(track_id = "TSF1", dist = 50, speed_ms = c(30, 32, 34))
## avg_speed esperado: quantile(c(30,32,34), .95) = 33.8 -> filtra <33.8 -> mean(30,32) = 31
## time_to_2rpm esperado: 1a leitura <=2rpm e +15 (1rpm) -> 15 - 0.1 = 14.9s
## min_safe_dist = 14.9 * 31 = 461.9 | margin = 50 - 461.9 = -411.9 -> "Crit"

test_prioritysp <- c("Golden-Eagle", "Steppe-Eagle") # subconjunto local, independente do userSettings_BSH.R

scada_all <- rbindlist(list(scada_a, scada_b, scada_c, scada_d, scada_e, scada_f))
curtl_all <- rbindlist(list(curtl_a, curtl_b, curtl_c, curtl_d, curtl_e, curtl_f))
track_all <- rbindlist(list(track_a, track_b, track_c, track_d, track_e, track_f))

safe_dist_dt <- compute_safe_distance(curtl_all, scada_all, track_all,
                                      start_end_gap_sec = 2, rpm_threshold = 2, speed_trim_q = 0.95)


## TDC1 nao deve aparecer em lado nenhum (start match invalido) -------------
cat("\n===== Verificacao: TDC1/TSC1 excluido (start match invalido) =====\n")
n_tsc1 <- nrow(safe_dist_dt[track_id == "TSC1"])
cat(sprintf("Linhas com track_id=='TSC1': %d (esperado: 0) -- %s\n",
           n_tsc1, if (n_tsc1 == 0) "OK" else "FALHOU"))


## Comparar valores esperados vs obtidos, por track_id ----------------------
expected_d <- data.table(
  track_id             = c("TSA1", "TSB1", "TSD1", "TSE1", "TSF1"),
  expected_time_sec    = c(29.8, 49.9, NA, 9.9, 14.9),
  expected_avg_speed   = c(9.5, 21.5, 15.5, 5.5, 31),
  expected_min_safe_m  = c(283.1, 1072.85, NA, 54.45, 461.9),
  expected_margin_m    = c(716.9, -772.85, NA, 145.55, -411.9),
  expected_status      = c("OK", "Crit", NA, "OK", "Crit")
)

check_d <- merge(safe_dist_dt, expected_d, by = "track_id")

close_or_na <- function(a, b, tol = 0.05) {
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) < tol)
}
same_or_na_chr <- function(a, b) {
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}

check_d[, `:=`(
  time_ok   = close_or_na(time_to_threshold_sec, expected_time_sec),
  speed_ok  = close_or_na(avg_speed_ms, expected_avg_speed),
  dist_ok   = close_or_na(min_safe_dist_m, expected_min_safe_m),
  margin_ok = close_or_na(dist_margin_m, expected_margin_m),
  status_ok = same_or_na_chr(status, expected_status)
)]

cat("\n===== compute_safe_distance() =====\n")
print(check_d[, .(track_id, species, time_to_threshold_sec, expected_time_sec, time_ok,
                  avg_speed_ms, expected_avg_speed, speed_ok,
                  min_safe_dist_m, expected_min_safe_m, dist_ok,
                  dist_margin_m, expected_margin_m, margin_ok,
                  status, expected_status, status_ok)])

n_ok <- sum(check_d$time_ok & check_d$speed_ok & check_d$dist_ok & check_d$margin_ok & check_d$status_ok)
cat(sprintf("\nResultado: %d/%d casos corretos (mais a verificacao do TDC1 acima).\n", n_ok, nrow(check_d)))


## summarise_safe_distance() SEM filtro de especie -- TDD1 (status NA) fica
## de fora; TDA1/TDB1/TDE1/TDF1 entram todos (incluindo o Kestrel) ----------
summary_all <- summarise_safe_distance(safe_dist_dt)

cat("\n===== summarise_safe_distance(safe_dist_dt) -- sem filtro =====\n")
print(summary_all$overall)
cat(paste(
  "\nEsperado: n=4 (TDA1, TDB1, TDE1, TDF1 -- TDD1 tem status NA e fica de fora),",
  "n_ok=2, n_crit=2, pct_ok=50, mean_min_safe_dist_m=468.1, median=372.5.\n"
))
print(summary_all$by_species)
cat(paste(
  "\nEsperado: Golden-Eagle n=2 (100% ok), Steppe-Eagle n=1 (0% ok),",
  "Kestrel n=1 (0% ok) -- Saker-Falcon (TSD1) fica de fora (status NA).\n"
))


## summarise_safe_distance() COM filtro de especies prioritarias -- deve
## excluir o Kestrel (nao prioritario) do resumo, mesmo continuando presente
## na tabela detalhada (safe_dist_dt) acima ---------------------------------
summary_priority <- summarise_safe_distance(safe_dist_dt, test_prioritysp)

cat("\n===== summarise_safe_distance(safe_dist_dt, test_prioritysp) -- so prioritarias =====\n")
print(summary_priority$overall)
cat(paste(
  "\nEsperado: n=3 (TDA1, TDB1, TDE1 -- Kestrel/TDF1 excluido pelo filtro),",
  "n_ok=2, n_crit=1, pct_ok=66.7, mean_min_safe_dist_m=470.1, median=283.1.\n"
))
print(summary_priority$by_species)
cat(sprintf(
  "\nLinhas com species=='Kestrel' no resumo filtrado: %d (esperado: 0) -- %s\n",
  nrow(summary_priority$by_species[species == "Kestrel"]),
  if (nrow(summary_priority$by_species[species == "Kestrel"]) == 0) "OK" else "FALHOU"
))
