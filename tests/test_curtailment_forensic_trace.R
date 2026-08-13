##
## Teste com dados simulados para R/curtailment_forensic_trace.R
##
## Cobre find_high_curtailment_days() e build_forensic_trace() com valores
## calculados a mao (mesma logica dos outros testes desta sessao); reaproveita
## time_to_rpm_thresholds() e compute_safe_distance(), ja validados nos seus
## proprios testes -- aqui o foco e a logica NOVA (dcast/merge/filtro por
## turbina+dia). plot_forensic_rpm() so tem um smoke test (confirma que corre
## sem erro e devolve um ggplot) -- o resultado visual tem de ser inspecionado
## no RStudio.
##
## Correr: source("tests/test_curtailment_forensic_trace.R")
##
## Depende de: data.table, ggplot2
##

source("R/curtailment_response.R")
source("R/curtailment_shutdown_time.R")
source("R/curtailment_safe_distance.R")
source("R/curtailment_forensic_trace.R")


##
## Cenarios sinteticos -- turbina TFOR1, dia 2026-05-01, com 3 curtailments:
##   TFA1 (08:00) -- "OK", full_speed, nunca atinge 1rpm (testa NA no dcast)
##   TFB1 (14:00) -- "Crit", full_speed, atinge 2rpm E 1rpm
##   TFC1 (18:00) -- "OK", already_slowing (start_rpm=4 < 6)
## + TFD1 (turbina TFOR1, dia 2026-05-02) -- testa filtro de DIA
## + TFE1 (turbina TFOR2, dia 2026-05-01) -- testa filtro de TURBINA
##

## TFA1 -- reaproveita os valores do TDA1 (ver test_curtailment_safe_distance.R) --
base_a <- as.POSIXct("2026-05-01 08:00:00", tz = "UTC")
scada_a <- data.table(
  turbinelabel = "TFOR1", datetime = base_a + c(0, 10, 20, 30),
  value = c(12, 8, 3, 1.5), readingname = "RPM"
)
curtl_a <- data.table(turbine = "TFOR1", track_id = "TFA1", species = "Golden-Eagle",
                      start = base_a + 0.2, end = base_a + 60)
track_a <- data.table(
  track_id = "TFA1", timestamp = base_a + c(-10, -8, -6, -4, -2),
  dist = rep(1000, 5), speed_ms = c(8, 9, 10, 11, 50)
)
## esperado: time_to_2rpm_sec=29.8, time_to_1rpm_sec=NA (nunca atinge 1rpm na janela),
## track_started = base_a - 10, x2d=1000, avg_speed_ms=9.5, min_safe_dist_m=283.1,
## dist_margin_m=716.9, status="OK", start_rpm=12 -> turbine_state="full_speed"

## TFB1 -- reaproveita os valores do TDB1, estendido para tambem atingir 1rpm --
base_b <- as.POSIXct("2026-05-01 14:00:00", tz = "UTC")
scada_b <- data.table(
  turbinelabel = "TFOR1", datetime = base_b + c(0, 10, 20, 30, 40, 50, 60),
  value = c(12, 10, 8, 6, 4, 1.8, 0.8), readingname = "RPM"
)
curtl_b <- data.table(turbine = "TFOR1", track_id = "TFB1", species = "Steppe-Eagle",
                      start = base_b + 0.1, end = base_b + 90)
track_b <- data.table(
  track_id = "TFB1", timestamp = base_b + c(-20, -18, -16, -14, -12),
  dist = rep(300, 5), speed_ms = c(20, 21, 22, 23, 90)
)
## esperado: time_to_2rpm_sec = 50-0.1 = 49.9, time_to_1rpm_sec = 60-0.1 = 59.9,
## track_started = base_b - 20, x2d=300, avg_speed_ms=21.5, min_safe_dist_m=1072.85,
## dist_margin_m=-772.85, status="Crit", start_rpm=12 -> turbine_state="full_speed"

## TFC1 -- reaproveita os valores do TDG1 (already_slowing), estendido a 1rpm --
base_c <- as.POSIXct("2026-05-01 18:00:00", tz = "UTC")
scada_c <- data.table(
  turbinelabel = "TFOR1", datetime = base_c + c(0, 5, 8),
  value = c(4, 1.5, 0.9), readingname = "RPM"
)
curtl_c <- data.table(turbine = "TFOR1", track_id = "TFC1", species = "Golden-Eagle",
                      start = base_c + 0.05, end = base_c + 15)
track_c <- data.table(
  track_id = "TFC1", timestamp = base_c + c(-3, -2, -1),
  dist = rep(150, 3), speed_ms = c(6, 7, 8)
)
## esperado: time_to_2rpm_sec = 5-0.05 = 4.95, time_to_1rpm_sec = 8-0.05 = 7.95,
## track_started = base_c - 3, x2d=150, avg_speed_ms=6.5, min_safe_dist_m=32.175,
## dist_margin_m=117.825, status="OK", start_rpm=4 -> turbine_state="already_slowing"

## TFD1 -- mesma turbina TFOR1, mas OUTRO DIA (2026-05-02) -- testa filtro de dia --
base_d <- as.POSIXct("2026-05-02 08:00:00", tz = "UTC")
scada_d <- data.table(
  turbinelabel = "TFOR1", datetime = base_d + c(0, 10),
  value = c(10, 1.5), readingname = "RPM"
)
curtl_d <- data.table(turbine = "TFOR1", track_id = "TFD1", species = "Golden-Eagle",
                      start = base_d + 0.1, end = base_d + 20)
track_d <- data.table(
  track_id = "TFD1", timestamp = base_d + c(-5, -4, -3),
  dist = rep(500, 3), speed_ms = c(5, 6, 7)
)

## TFE1 -- OUTRA TURBINA (TFOR2), mesmo dia 2026-05-01 -- testa filtro de turbina --
base_e <- as.POSIXct("2026-05-01 09:00:00", tz = "UTC")
scada_e <- data.table(
  turbinelabel = "TFOR2", datetime = base_e + c(0, 10),
  value = c(10, 1.5), readingname = "RPM"
)
curtl_e <- data.table(turbine = "TFOR2", track_id = "TFE1", species = "Golden-Eagle",
                      start = base_e + 0.1, end = base_e + 20)
track_e <- data.table(
  track_id = "TFE1", timestamp = base_e + c(-5, -4, -3),
  dist = rep(400, 3), speed_ms = c(5, 6, 7)
)

scada_all <- rbindlist(list(scada_a, scada_b, scada_c, scada_d, scada_e))
curtl_all <- rbindlist(list(curtl_a, curtl_b, curtl_c, curtl_d, curtl_e))
track_all <- rbindlist(list(track_a, track_b, track_c, track_d, track_e))

tt_dt <- time_to_rpm_thresholds(curtl_all, scada_all, thresholds = c(2, 1), start_end_gap_sec = 2)
safe_dist_dt <- compute_safe_distance(curtl_all, scada_all, track_all,
                                      start_end_gap_sec = 2, rpm_threshold = 2, speed_trim_q = 0.95)


## 1. find_high_curtailment_days() ------------------------------------------

days_tfor1 <- find_high_curtailment_days(curtl_all, "TFOR1", min_curtailments = 3)

cat("\n===== find_high_curtailment_days(curtl_all, 'TFOR1', min_curtailments = 3) =====\n")
print(days_tfor1)
cat(sprintf(
  "\nEsperado: 1 linha, day=2026-05-01, n_curtailments=3 (2026-05-02 tem so 1, fica de fora) -- %s\n",
  if (nrow(days_tfor1) == 1 && days_tfor1$day[1] == as.Date("2026-05-01") && days_tfor1$n_curtailments[1] == 3) "OK" else "FALHOU"
))


## 2. build_forensic_trace() -------------------------------------------------

forensic_dt <- build_forensic_trace("TFOR1", "2026-05-01", tt_dt, safe_dist_dt, track_all)

cat("\n===== build_forensic_trace('TFOR1', '2026-05-01', ...) =====\n")
print(forensic_dt[, .(track_id, species, start, track_started, time_to_2rpm_sec, time_to_1rpm_sec,
                      x2d, avg_speed_ms, min_safe_dist_m, dist_margin_m, status, turbine_state)])

expected_f <- data.table(
  track_id            = c("TFA1", "TFB1", "TFC1"),
  expected_t2         = c(29.8, 49.9, 4.95),
  expected_t1         = c(NA, 59.9, 7.95),
  expected_started    = c(base_a - 10, base_b - 20, base_c - 3),
  expected_x2d        = c(1000, 300, 150),
  expected_speed      = c(9.5, 21.5, 6.5),
  expected_min_safe   = c(283.1, 1072.85, 32.175),
  expected_margin     = c(716.9, -772.85, 117.825),
  expected_status     = c("OK", "Crit", "OK"),
  expected_state      = c("full_speed", "full_speed", "already_slowing")
)

check_f <- merge(forensic_dt, expected_f, by = "track_id")
close_or_na <- function(a, b, tol = 0.05) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) < tol)

check_f[, `:=`(
  t2_ok      = close_or_na(time_to_2rpm_sec, expected_t2),
  t1_ok      = close_or_na(time_to_1rpm_sec, expected_t1),
  started_ok = track_started == expected_started,
  x2d_ok     = close_or_na(x2d, expected_x2d),
  speed_ok   = close_or_na(avg_speed_ms, expected_speed),
  minsafe_ok = close_or_na(min_safe_dist_m, expected_min_safe),
  margin_ok  = close_or_na(dist_margin_m, expected_margin),
  status_ok  = status == expected_status,
  state_ok   = turbine_state == expected_state
)]

cat("\n===== Verificacao build_forensic_trace() =====\n")
print(check_f[, .(track_id, t2_ok, t1_ok, started_ok, x2d_ok, speed_ok, minsafe_ok, margin_ok, status_ok, state_ok)])

n_ok <- sum(check_f$t2_ok & check_f$t1_ok & check_f$started_ok & check_f$x2d_ok &
           check_f$speed_ok & check_f$minsafe_ok & check_f$margin_ok & check_f$status_ok & check_f$state_ok)
cat(sprintf("\nResultado: %d/%d casos corretos.\n", n_ok, nrow(check_f)))

cat(sprintf(
  "\nOrdem por 'start' (TFA1 08:00 < TFB1 14:00 < TFC1 18:00): %s -- %s\n",
  paste(forensic_dt$track_id, collapse = " < "),
  if (identical(forensic_dt$track_id, c("TFA1", "TFB1", "TFC1"))) "OK" else "FALHOU"
))

cat(sprintf(
  "\nTFD1 (2026-05-02) e TFE1 (TFOR2) ficam de fora: %d linhas com esses track_id (esperado: 0) -- %s\n",
  nrow(forensic_dt[track_id %in% c("TFD1", "TFE1")]),
  if (nrow(forensic_dt[track_id %in% c("TFD1", "TFE1")]) == 0) "OK" else "FALHOU"
))


## Dia sem nenhum curtailment -- tem de devolver 0 linhas sem erro -----------
forensic_empty <- build_forensic_trace("TFOR1", "2026-05-03", tt_dt, safe_dist_dt, track_all)
cat(sprintf(
  "\nbuild_forensic_trace() num dia sem curtailments: %d linhas (esperado: 0) -- %s\n",
  nrow(forensic_empty), if (nrow(forensic_empty) == 0) "OK" else "FALHOU"
))


## 3. plot_forensic_rpm() -- smoke test (so confirma que corre e devolve ggplot) ----

p <- plot_forensic_rpm(forensic_dt, scada_all, "TFOR1", "2026-05-01")

cat("\n===== plot_forensic_rpm() smoke test =====\n")
cat(sprintf("Devolve objeto ggplot: %s (esperado: TRUE)\n", inherits(p, "ggplot")))
cat("Inspecionar visualmente no RStudio: 3 barras/linhas cinzentas (TFA1 08:00, TFB1 14:00, TFC1 18:00),\n")
cat("cruzes azuis (2rpm) em TFA1/TFB1/TFC1, cruzes vermelhas (1rpm) so em TFB1/TFC1 (TFA1 nunca atinge 1rpm).\n")
cat("Nota: os dados de RPM sinteticos sao 3 clusters muito esparsos (so 3-7 pontos cada) --\n")
cat("a linha vai parecer artificial entre clusters (liga em linha reta). Com scada_dt real (~10s\n")
cat("de resolucao) a curva fica continua como no portal -- isto e so para confirmar que a funcao corre.\n")
