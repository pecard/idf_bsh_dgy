##
## Teste com dados simulados para R/curtailment_bearing_sectors.R
##
## Turbina sintetica "TBS1" fixa em UTM (1000, 1000) (CRS arbitrario, so'
## interessa a geometria plana). 4 tracks sinteticos aproximando-se exatamente
## de N/E/S/W (100m de distancia cada, bearing exato), mais casos de exclusao:
## track curto (< min_points), track ausente de track_dt, e turbina ausente
## do shapefile wtg.
##
## Correr: source("tests/test_curtailment_bearing_sectors.R")
##
## Depende de: data.table, sf
##

source("R/curtailment_bearing_sectors.R")

wtg_test <- sf::st_as_sf(
  data.frame(InternalNa = "TBS1", x = 1000, y = 1000),
  coords = c("x", "y"), crs = 32641
)

t0 <- as.POSIXct("2026-05-01 08:00:00", tz = "UTC")

## 5 pontos por track (>= min_points = 5), speed_ms identico em todos
## (8,9,10,11,50 -- 50 e outlier, excluido pelo percentil 95) para isolar a
## verificacao do bearing/setor/distancia -- avg_speed esperado = 9.5 em
## todos os 4 tracks direcionais (mesma logica de test_curtailment_safe_distance.R)

## TB_N -- exatamente a Norte da turbina (0, +100) -> bearing 0, setor "N" --
track_n <- data.table::data.table(
  track_id = "TB_N", timestamp = t0 + 0:4,
  utm_x = c(1000, 1000, 1001, 1000, 999), utm_y = c(1100, 1101, 1102, 1103, 1104),
  speed_ms = c(8, 9, 10, 11, 50)
)

## TB_E -- exatamente a Este (+100, 0) -> bearing 90, setor "E" -----------
track_e <- data.table::data.table(
  track_id = "TB_E", timestamp = t0 + 0:4,
  utm_x = c(1100, 1101, 1102, 1103, 1104), utm_y = c(1000, 1000, 999, 1001, 1000),
  speed_ms = c(8, 9, 10, 11, 50)
)

## TB_S -- exatamente a Sul (0, -100) -> bearing 180, setor "S" -----------
track_s <- data.table::data.table(
  track_id = "TB_S", timestamp = t0 + 0:4,
  utm_x = c(1000, 999, 1000, 1001, 1000), utm_y = c(900, 899, 898, 897, 896),
  speed_ms = c(8, 9, 10, 11, 50)
)

## TB_W -- exatamente a Oeste (-100, 0) -> bearing 270, setor "W" ---------
track_w <- data.table::data.table(
  track_id = "TB_W", timestamp = t0 + 0:4,
  utm_x = c(900, 899, 898, 897, 896), utm_y = c(1000, 1001, 1000, 999, 1000),
  speed_ms = c(8, 9, 10, 11, 50)
)

## TB_SHORT -- so' 3 pontos (< min_points = 5) -- deve ficar de fora --------
track_short <- data.table::data.table(
  track_id = "TB_SHORT", timestamp = t0 + 0:2,
  utm_x = c(1050, 1050, 1050), utm_y = c(1050, 1050, 1050),
  speed_ms = c(5, 6, 7)
)

track_dt_test <- data.table::rbindlist(list(track_n, track_e, track_s, track_w, track_short))

curtl_dt_test <- data.table::data.table(
  turbine  = c("TBS1", "TBS1", "TBS1", "TBS1", "TBS1", "TBS1", "T_MISSING"),
  track_id = c("TB_N", "TB_E", "TB_S", "TB_W", "TB_SHORT", "TB_ABSENT", "TB_N"),
  species  = c("Golden-Eagle_test", "Golden-Eagle_test", "Golden-Eagle_test",
              "Golden-Eagle_test", "Golden-Eagle_test", "Golden-Eagle_test", "Golden-Eagle_test"),
  start    = t0 + c(10, 10, 10, 10, 10, 10, 10)
)
## TB_SHORT -- excluido por ter < min_points registos em track_dt_test
## TB_ABSENT -- excluido por nao existir de todo em track_dt_test
## T_MISSING -- excluido por nao existir no shapefile wtg_test


## 1. bearing_to_sector() -- casos de fronteira (calculados a mao) ----------

cat("\n===== bearing_to_sector() -- casos de fronteira =====\n")
boundary_cases <- data.table::data.table(
  bearing_deg = c(0, 44, 22.5, 350, 337.5, 90, 180, 270),
  expected    = c("N", "NE", "NE", "N", "N", "E", "S", "W")
)
boundary_cases[, got := bearing_to_sector(bearing_deg)]
boundary_cases[, ok := got == expected]
print(boundary_cases)
cat(sprintf("Resultado: %d/%d casos corretos.\n", sum(boundary_cases$ok), nrow(boundary_cases)))


## 2. compute_curtailment_bearing() -- setor/distancia/velocidade por evento -

cat("\n===== compute_curtailment_bearing(curtl_dt_test, track_dt_test, wtg_test) =====\n")
bearing_dt_test <- compute_curtailment_bearing(curtl_dt_test, track_dt_test, wtg_test, min_points = 5)
print(bearing_dt_test)

expected_events <- data.table::data.table(
  track_id           = c("TB_N", "TB_E", "TB_S", "TB_W"),
  expected_sector    = c("N", "E", "S", "W"),
  expected_dist_m    = c(100, 100, 100, 100),
  expected_avg_speed = c(9.5, 9.5, 9.5, 9.5)
)
check2 <- merge(bearing_dt_test, expected_events, by = "track_id")
check2[, `:=`(
  sector_ok = as.character(sector) == expected_sector,
  dist_ok   = abs(trigger_dist_m - expected_dist_m) < 0.5,
  speed_ok  = abs(avg_speed_ms - expected_avg_speed) < 0.05
)]
print(check2[, .(track_id, sector, expected_sector, sector_ok,
                 trigger_dist_m, expected_dist_m, dist_ok,
                 avg_speed_ms, expected_avg_speed, speed_ok)])

cat(sprintf(
  "Resultado: %d/%d eventos corretos (sector+distancia+velocidade).\n",
  sum(check2$sector_ok & check2$dist_ok & check2$speed_ok), nrow(check2)
))

cat(sprintf(
  "\nLinhas no resultado: %d (esperado: 4 -- TB_SHORT/TB_ABSENT/T_MISSING excluidos) -- %s\n",
  nrow(bearing_dt_test), if (nrow(bearing_dt_test) == 4L) "OK" else "FALHOU"
))


## 3. summarise_bearing_sectors() -- 1 evento por setor N/E/S/W, 0 nos outros -

cat("\n===== summarise_bearing_sectors(bearing_dt_test) =====\n")
summary_bearing_test <- summarise_bearing_sectors(bearing_dt_test)
print(summary_bearing_test)
cat(paste(
  "Esperado: 8 setores, N/E/S/W com n=1 (mean_trigger_dist_m=100, mean_speed_ms=9.5)",
  "cada, NE/SE/SW/NW com n=0 (medias NA).\n"
))
