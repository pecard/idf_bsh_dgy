##
## Teste com dados simulados para R/fatality_track_investigation.R
##
## Turbina sintetica "TFOR1" fixa em UTM (1000, 2000) (CRS arbitrario, so
## interessa a geometria plana). 5 tracks sinteticos cobrindo os 4 "sinais"
## possiveis, mais 2 casos de exclusao (especie errada, fora da janela).
##
## Correr: source("tests/test_fatality_track_investigation.R")
##
## Depende de: data.table, sf
##

source("R/fatality_track_investigation.R")

wtg_test <- sf::st_as_sf(
  data.frame(InternalNa = "TFOR1", x = 1000, y = 2000),
  coords = c("x", "y"), crs = 32641
)

incident_date <- as.Date("2026-04-10")
days_before <- 8
## janela esperada: 2026-04-02 00:00:00 a 2026-04-10 23:59:59 (inclui o dia do registo)

t0 <- as.POSIXct("2026-04-05 08:00:00", tz = "UTC") # dentro da janela

##
## Cenarios:
##   TRK_COLLISION       -- aproxima-se, ultimo ponto a 30m, SEM curtailment
##                           -> "no_curtailment_lost_near_turbine"
##   TRK_CURTAILED_SAFE  -- aproxima-se, ultimo ponto a 40m, COM curtailment
##                           -> "curtailment_lost_near_turbine"
##   TRK_PASSED_BY        -- chega a 50m a meio, mas afasta-se (ultimo a 300m)
##                           -> "near_turbine_not_last"
##   TRK_FAR              -- nunca fica a menos de 100m -> "far_from_turbine"
##   TRK_WRONG_SPECIES    -- muito perto (10m), mas especie errada -- EXCLUIDO
##   TRK_OUTSIDE_WINDOW   -- muito perto (10m), mas fora da janela -- EXCLUIDO
##

track_dt_test <- data.table::rbindlist(list(
  data.table::data.table(track_id = "TRK_COLLISION", spec = "TestSpecies",
                         timestamp = t0 + c(0, 60, 120),
                         utm_x = c(1500, 1200, 1030), utm_y = c(2000, 2000, 2000)),
  data.table::data.table(track_id = "TRK_CURTAILED_SAFE", spec = "TestSpecies",
                         timestamp = t0 + c(0, 60, 120) + 300,
                         utm_x = c(1600, 1250, 1040), utm_y = c(2000, 2000, 2000)),
  data.table::data.table(track_id = "TRK_PASSED_BY", spec = "TestSpecies",
                         timestamp = t0 + c(0, 60, 120) + 600,
                         utm_x = c(1300, 1050, 1000), utm_y = c(2000, 2000, 2300)),
  data.table::data.table(track_id = "TRK_FAR", spec = "TestSpecies",
                         timestamp = t0 + c(0, 60) + 900,
                         utm_x = c(1500, 1000), utm_y = c(2000, 2400)),
  data.table::data.table(track_id = "TRK_WRONG_SPECIES", spec = "OtherSpecies",
                         timestamp = t0 + c(0, 60) + 1200,
                         utm_x = c(1010, 1005), utm_y = c(2000, 2000)),
  data.table::data.table(track_id = "TRK_OUTSIDE_WINDOW", spec = "TestSpecies",
                         timestamp = as.POSIXct("2026-03-21 08:00:00", tz = "UTC") + c(0, 60),
                         utm_x = c(1010, 1005), utm_y = c(2000, 2000))
))
## esperado:
##   TRK_COLLISION:      min_dist=30 (last), last_dist=30  -> within=T, last_within=T, curtailed=F -> "no_curtailment_lost_near_turbine"
##   TRK_CURTAILED_SAFE: min_dist=40 (last), last_dist=40  -> within=T, last_within=T, curtailed=T -> "curtailment_lost_near_turbine"
##   TRK_PASSED_BY:      min_dist=50 (meio), last_dist=300 -> within=T, last_within=F              -> "near_turbine_not_last"
##   TRK_FAR:            min_dist=400 (t=0: dist=500; t=60: dist=400) -> within=F                  -> "far_from_turbine"
##   TRK_WRONG_SPECIES / TRK_OUTSIDE_WINDOW: NAO devem aparecer no resultado

curtl_dt_test <- data.table::data.table(
  turbine = "TFOR1", track_id = "TRK_CURTAILED_SAFE"
)

result <- investigate_fatality_tracks(
  turbine_id = "TFOR1", species = "TestSpecies",
  incident_date = incident_date, days_before = days_before,
  track_dt = track_dt_test, curtl_dt = curtl_dt_test, wtg_sf = wtg_test,
  proximity_threshold_m = 100
)

cat("\n===== investigate_fatality_tracks() =====\n")
print(result[, .(track_id, n_points, first_dist_m, last_dist_m, min_dist_m,
                 within_threshold, last_within_threshold, triggered_curtailment, signal)])

expected <- data.table::data.table(
  track_id = c("TRK_COLLISION", "TRK_CURTAILED_SAFE", "TRK_PASSED_BY", "TRK_FAR"),
  expected_min_dist = c(30, 40, 50, 400),
  expected_last_dist = c(30, 40, 300, 400),
  expected_within = c(TRUE, TRUE, TRUE, FALSE),
  expected_last_within = c(TRUE, TRUE, FALSE, FALSE),
  expected_curtailed = c(FALSE, TRUE, FALSE, FALSE),
  expected_signal = c("no_curtailment_lost_near_turbine", "curtailment_lost_near_turbine",
                      "near_turbine_not_last", "far_from_turbine")
)

check <- merge(result, expected, by = "track_id")
close_ <- function(a, b, tol = 0.01) abs(a - b) < tol
check[, `:=`(
  min_ok    = close_(min_dist_m, expected_min_dist),
  last_ok   = close_(last_dist_m, expected_last_dist),
  within_ok = within_threshold == expected_within,
  lwithin_ok = last_within_threshold == expected_last_within,
  curt_ok   = triggered_curtailment == expected_curtailed,
  signal_ok = signal == expected_signal
)]

cat("\n===== Verificacao =====\n")
print(check[, .(track_id, min_ok, last_ok, within_ok, lwithin_ok, curt_ok, signal_ok)])

n_ok <- sum(check$min_ok & check$last_ok & check$within_ok & check$lwithin_ok & check$curt_ok & check$signal_ok)
cat(sprintf("\nResultado: %d/%d casos corretos (dos 4 esperados).\n", n_ok, nrow(check)))

cat(sprintf(
  "\nTRK_WRONG_SPECIES e TRK_OUTSIDE_WINDOW ficam de fora: %d linhas (esperado: 0) -- %s\n",
  nrow(result[track_id %in% c("TRK_WRONG_SPECIES", "TRK_OUTSIDE_WINDOW")]),
  if (nrow(result[track_id %in% c("TRK_WRONG_SPECIES", "TRK_OUTSIDE_WINDOW")]) == 0) "OK" else "FALHOU"
))

cat(sprintf(
  "\nTotal de tracks no resultado: %d (esperado: 4) -- %s\n",
  nrow(result), if (nrow(result) == 4) "OK" else "FALHOU"
))


## investigate_fatality_incidents() -- 2a turbina/incidente, para confirmar o wrapper ----

wtg_test2 <- sf::st_as_sf(
  data.frame(InternalNa = c("TFOR1", "TFOR2"), x = c(1000, 5000), y = c(2000, 8000)),
  coords = c("x", "y"), crs = 32641
)

track_dt_test2 <- data.table::rbindlist(list(
  track_dt_test,
  data.table::data.table(track_id = "TRK_T2_COLLISION", spec = "OtherFatalitySpecies",
                         timestamp = t0 + c(0, 60), utm_x = c(5300, 5020), utm_y = c(8000, 8000))
))
curtl_dt_test2 <- curtl_dt_test

fatality_incidents_test <- data.table::data.table(
  incident_id   = c("INC_A", "INC_B"),
  turbine       = c("TFOR1", "TFOR2"),
  species       = c("TestSpecies", "OtherFatalitySpecies"),
  incident_date = as.Date(c("2026-04-10", "2026-04-10")),
  days_before   = c(8, 8)
)

all_result <- investigate_fatality_incidents(
  fatality_incidents_test, track_dt_test2, curtl_dt_test2, wtg_test2,
  proximity_threshold_m = 100
)

cat("\n===== investigate_fatality_incidents() -- 2 incidentes =====\n")
print(all_result[, .(incident_id, turbine, species, track_id, min_dist_m, last_dist_m, signal)])

wrapper_ok <- nrow(all_result[incident_id == "INC_A"]) == 4 &&
  nrow(all_result[incident_id == "INC_B"]) == 1 &&
  all_result[incident_id == "INC_B"]$signal == "no_curtailment_lost_near_turbine"
cat(sprintf(
  "\nEsperado: INC_A com 4 tracks (igual ao teste anterior), INC_B com 1 track (TRK_T2_COLLISION,\nmin_dist=20, signal='no_curtailment_lost_near_turbine') -- %s\n",
  if (wrapper_ok) "OK" else "FALHOU"
))
