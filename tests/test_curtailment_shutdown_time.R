##
## Teste com dados simulados para R/curtailment_shutdown_time.R
##
## Mesma logica dos testes de curtailment_response.R: dados sinteticos com
## resultado calculado a mao, comparados com o que as funcoes reais devolvem.
##
## Usa track_id como chave de comparacao (o curtailment_id e gerado
## internamente pela funcao, nao e previsivel de fora).
##
## Correr: source("tests/test_curtailment_shutdown_time.R")
##
## Depende de: data.table
##

source("R/curtailment_response.R")
source("R/curtailment_shutdown_time.R")


##
## Cenarios sinteticos:
##   TD1  -- desce a 2rpm e 1rpm dentro da janela, nunca chega a 0rpm
##   TD2  -- nunca desce abaixo de 2rpm (nenhum limiar atingido)
##   TD3  -- match do start invalido (>2s) -- deve ficar de fora dos resultados
##   TD4A, TD4B -- 2 curtailments da MESMA turbina (TESTD4), para testar a
##                 agregacao por turbina em summarise_time_to_threshold()
##

## TD1 -------------------------------------------------------------------
base_d1 <- as.POSIXct("2026-03-01 00:00:00", tz = "UTC")
scada_d1 <- data.table(
  turbinelabel = "TESTD1",
  datetime     = base_d1 + seq(0, 60, by = 10),
  value        = c(12, 8, 3, 1.8, 0.9, 0.5, 0.3),
  readingname  = "RPM"
)
curtl_d1 <- data.table(turbine = "TESTD1", track_id = "TD1", species = "Golden-Eagle",
                       start = base_d1 + 0.3, end = base_d1 + 60.2)

## TD2 -- nunca desce abaixo de 2rpm ---------------------------------------
base_d2 <- as.POSIXct("2026-03-01 00:10:00", tz = "UTC")
scada_d2 <- data.table(
  turbinelabel = "TESTD2",
  datetime     = base_d2 + seq(0, 30, by = 10),
  value        = c(13, 12.5, 12.8, 13),
  readingname  = "RPM"
)
curtl_d2 <- data.table(turbine = "TESTD2", track_id = "TD2", species = "Steppe-Eagle",
                       start = base_d2 + 0.2, end = base_d2 + 30.3)

## TD3 -- start match invalido (4s de distancia, > tolerancia de 2s) -------
base_d3 <- as.POSIXct("2026-03-01 00:20:00", tz = "UTC")
scada_d3 <- data.table(
  turbinelabel = "TESTD3",
  datetime     = base_d3 + c(0, 10, 20),
  value        = c(10, 1, 0.5),
  readingname  = "RPM"
)
curtl_d3 <- data.table(turbine = "TESTD3", track_id = "TD3", species = "Egyptian-Vulture",
                       start = base_d3 + 4, end = base_d3 + 20)

## TD4A e TD4B -- 2 curtailments da mesma turbina (TESTD4) -----------------
base_d4a <- as.POSIXct("2026-03-01 00:30:00", tz = "UTC")
scada_d4a <- data.table(
  turbinelabel = "TESTD4",
  datetime     = base_d4a + seq(0, 30, by = 10),
  value        = c(12, 5, 1.5, 0.8),
  readingname  = "RPM"
)
curtl_d4a <- data.table(turbine = "TESTD4", track_id = "TD4A", species = "Saker-Falcon",
                        start = base_d4a + 0.1, end = base_d4a + 30.2)

base_d4b <- as.POSIXct("2026-03-01 00:40:00", tz = "UTC")
scada_d4b <- data.table(
  turbinelabel = "TESTD4",
  datetime     = base_d4b + seq(0, 40, by = 10),
  value        = c(11, 4, 2.5, 1.9, 0.95),
  readingname  = "RPM"
)
curtl_d4b <- data.table(turbine = "TESTD4", track_id = "TD4B", species = "Saker-Falcon",
                        start = base_d4b + 0.15, end = base_d4b + 40.3)

scada_d_all <- rbindlist(list(scada_d1, scada_d2, scada_d3, scada_d4a, scada_d4b))
curtl_d_all <- rbindlist(list(curtl_d1, curtl_d2, curtl_d3, curtl_d4a, curtl_d4b))

tt_dt <- time_to_rpm_thresholds(curtl_d_all, scada_d_all, thresholds = c(2, 1, 0), start_end_gap_sec = 2)

## TD3 nao deve aparecer em lado nenhum (start match invalido) -------------
cat("\n===== Verificacao: TD3 excluido (start match invalido) =====\n")
n_td3 <- nrow(tt_dt[track_id == "TD3"])
cat(sprintf("Linhas com track_id=='TD3': %d (esperado: 0) -- %s\n",
           n_td3, if (n_td3 == 0) "OK" else "FALHOU"))

## Comparar tempos esperados vs obtidos, por track_id + limiar -------------
expected_d <- data.table(
  track_id  = rep(c("TD1", "TD2", "TD4A", "TD4B"), each = 3),
  threshold = rep(c(2, 1, 0), times = 4),
  expected_time_to_threshold_sec = c(
    29.7, 39.7, NA,   # TD1
    NA,   NA,   NA,   # TD2 -- nunca desce abaixo de 2rpm
    19.9, 29.9, NA,   # TD4A
    29.85, 39.85, NA  # TD4B
  )
)

check_d <- merge(tt_dt, expected_d, by = c("track_id", "threshold"))
check_d[, time_ok := (is.na(time_to_threshold_sec) & is.na(expected_time_to_threshold_sec)) |
                     (!is.na(time_to_threshold_sec) & !is.na(expected_time_to_threshold_sec) &
                        abs(time_to_threshold_sec - expected_time_to_threshold_sec) < 0.01)]

cat("\n===== time_to_rpm_thresholds() =====\n")
print(check_d[order(track_id, -threshold), .(track_id, threshold, turbine, start_rpm,
                                              time_to_threshold_sec, expected_time_to_threshold_sec, time_ok)])
cat(sprintf(
  "\nResultado: %d/%d casos corretos (mais a verificacao do TD3 acima).\n",
  sum(check_d$time_ok), nrow(check_d)
))


## Resumo por turbina e limiar ----------------------------------------------
by_turbine <- summarise_time_to_threshold(tt_dt)

cat("\n===== summarise_time_to_threshold() =====\n")
print(by_turbine)
cat(paste(
  "\nNota: TESTD4 tem 2 curtailments (TD4A: 19.9s, TD4B: 29.85s para <=2rpm).",
  "mean_time_sec esperado para TESTD4/threshold=2 e (19.9+29.85)/2 = 24.875",
  "-- confirma visualmente na tabela acima.\n"
))


## Tabela de bandas (% abaixo/acima dos cortes) ------------------------------
bands <- summarise_time_to_threshold_bands(tt_dt, low_cut = 40, high_cut = 50)

cat("\n===== summarise_time_to_threshold_bands() =====\n")
print(bands)
cat(paste(
  "\nNota: para threshold=2, os 3 tempos validos sao 29.7 (TD1), 19.9 (TD4A),",
  "29.85 (TD4B) -- todos <40s, nenhum >50s. Para threshold=1: 39.7, 29.9,",
  "39.85 -- tambem todos <40s. pct_below_cut deveria sair 100 em ambos,",
  "pct_above_cut 0.\n"
))
