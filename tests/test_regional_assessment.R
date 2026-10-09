##
## Teste com dados simulados para R/regional_assessment.R
##
## Cobre tudo o que nao precisa de ficheiros brutos: dias monitorizados,
## bins de 2 min -> maximo diario/semanal (NA onde nao ha' monitorizacao),
## timing da passagem, correlacao cruzada, pulsos e plots. Nao cobre
## regional_load_farm() (precisa dos ficheiros brutos + fst/janitor).
##
## Correr: source("tests/test_regional_assessment.R")
##
## Depende de: data.table, lubridate, ggplot2, scales
##

library(data.table)
library(ggplot2)
source("R/track_min_individuals.R")
source("R/regional_assessment.R")

check <- function(label, ok) cat(sprintf("%s -- %s\n", label, if (isTRUE(ok)) "OK" else "FALHOU"))
tz_test <- "Asia/Samarkand"


##
## 1. Bins -> maximo diario/semanal, num parque simulado de 10 dias
##
## dia 1-2 (seg-ter): 2 tracks de EV a 100 m no MESMO bin -> 1 individuo
## dia 2: 2 tracks de EV a 1000 m -> 2 individuos (maximo do dia 2)
## dia 3: so' tracks de outra especie (dia monitorizado, EV ausente -> 0)
## dia 4: sem nenhum track -> NAO monitorizado (NA)
## dias 5-7 (qui-sab): 1 track por dia de outra especie (monitorizados, EV 0)
## dia 10 (ter): 1 track EV
##
base_day <- as.POSIXct("2026-03-02 10:00:00", tz = tz_test) # segunda-feira
mk <- function(day_offset, minute, id, x, spec) {
  data.table(track_id = id, timestamp = base_day + day_offset * 86400 + minute * 60,
             utm_x = x, utm_y = 0, spec = spec)
}
track_dt_test <- rbindlist(list(
  mk(0, 0, "a1", 0, "Egyptian-Vulture"), mk(0, 0, "a2", 100, "Egyptian-Vulture"),
  mk(1, 0, "b1", 0, "Egyptian-Vulture"), mk(1, 0, "b2", 1000, "Egyptian-Vulture"),
  mk(2, 0, "c1", 0, "Steppe-Eagle"), mk(2, 1, "c2", 0, "Steppe-Eagle"),
  mk(4, 0, "d1", 0, "Steppe-Eagle"), mk(4, 1, "d2", 0, "Steppe-Eagle"),
  mk(5, 0, "e1", 0, "Steppe-Eagle"), mk(5, 1, "e2", 0, "Steppe-Eagle"),
  mk(6, 0, "f1", 0, "Steppe-Eagle"), mk(6, 1, "f2", 0, "Steppe-Eagle"),
  mk(9, 0, "g1", 0, "Egyptian-Vulture"), mk(9, 1, "g2", 0, "Steppe-Eagle")
))

mon_test <- regional_monitored_days(track_dt_test, tz_test, min_tracks_per_day = 2L)
check("dias monitorizados: 7 (sem tracks nos dias 3, 7 e 8)", length(mon_test$monitored) == 7L)
check("intervalo de dados: 1o = 2026-03-02", mon_test$first_day == as.Date("2026-03-02"))
check("intervalo de dados: ultimo = 2026-03-11", mon_test$last_day == as.Date("2026-03-11"))

bins_test <- regional_min_individuals_bins(track_dt_test, "Egyptian-Vulture", bin_min = 2, merge_dist_m = 200)
daily_test <- regional_daily_max(bins_test, "Egyptian-Vulture", mon_test, tz_test)
dv <- function(d) daily_test[day == as.Date(d), max_individuals]
check("dia 1 (2 tracks a 100 m -> 1 individuo)", dv("2026-03-02") == 1L)
check("dia 2 (2 tracks a 1000 m -> 2 individuos)", dv("2026-03-03") == 2L)
check("dia 3 (monitorizado, EV ausente -> 0)", dv("2026-03-04") == 0L)
check("dia 4 (sem tracks -> NA, nao 0)", is.na(dv("2026-03-05")))
check("dia 10 (1 track EV -> 1)", dv("2026-03-11") == 1L)

weekly_test <- regional_weekly_max(daily_test, min_monitored_days = 4L)
check("semana 1 (seg 2026-03-02): maximo 2 com >=4 dias monitorizados",
      weekly_test[week_start == as.Date("2026-03-02"), max_individuals] == 2L)
check("semana 2 (seg 2026-03-09): poucos dias monitorizados -> NA",
      is.na(weekly_test[week_start == as.Date("2026-03-09"), max_individuals]))


##
## 2. Antecipacao: outono ZRF -> BSH (+5 d) e DGY (+7 d); primavera BSH (+0) -> ZRF (+6 d)
##
make_series <- function(farm, starts, from, to, width = 3L, height = 3L, spec = "Egyptian-Vulture") {
  d <- data.table(farm = farm, spec = spec, day = seq(as.Date(from), as.Date(to), by = "day"))
  d[, `:=`(monitored = TRUE, max_individuals = 0L)]
  for (s in as.Date(starts)) d[day >= s & day < s + width, max_individuals := as.integer(height)]
  d[]
}
autumn_zrf <- as.Date("2025-09-01") + c(0, 20, 45, 70)
spring_bsh <- as.Date("2026-03-01") + c(0, 15, 35, 55)
rng <- c("2025-08-01", "2026-06-15")
daily_pair_test <- rbindlist(list(
  make_series("ZRF", c(autumn_zrf, spring_bsh + 6), rng[1], rng[2]),
  make_series("BSH", c(autumn_zrf + 5, spring_bsh), rng[1], rng[2]),
  make_series("DGY", c(autumn_zrf + 7, spring_bsh), rng[1], rng[2])
))

seasons_test <- list(autumn = c("08-15", "12-15"), spring = c("02-15", "05-31"))
season_dates_test <- rbind(
  regional_season_dates(seasons_test["autumn"], 2025L),
  regional_season_dates(seasons_test["spring"], 2026L)
)
pairs_test <- data.table(
  season   = c("autumn", "autumn", "spring", "spring"),
  leader   = c("ZRF", "ZRF", "BSH", "DGY"),
  follower = c("BSH", "DGY", "ZRF", "ZRF")
)

ccf_test <- regional_ccf(daily_pair_test, pairs_test, season_dates_test,
                         max_lag_days = 15L, smooth_days = 1L, min_overlap_days = 20L)
ccf_sum_test <- regional_ccf_summary(ccf_test)
best <- function(s, l, f) ccf_sum_test[season == s & leader == l & follower == f, best_lag_days]
check("ccf outono ZRF -> BSH: lag = 5", best("autumn", "ZRF", "BSH") == 5L)
check("ccf outono ZRF -> DGY: lag = 7", best("autumn", "ZRF", "DGY") == 7L)
check("ccf primavera BSH -> ZRF: lag = 6", best("spring", "BSH", "ZRF") == 6L)
check("ccf primavera DGY -> ZRF: lag = 6", best("spring", "DGY", "ZRF") == 6L)

timing_test <- regional_passage_timing(daily_pair_test, season_dates_test, min_total = 3)
check("timing: 3 parques x 2 epocas, todos validos", nrow(timing_test) == 6L && all(timing_test$valid))
lead_test <- regional_lead_table(timing_test, pairs_test)
lead <- function(s, l, f) lead_test[season == s & leader == l & follower == f, lead_days_median]
check("timing mediana outono ZRF -> BSH: positivo (ZRF antecipa)", lead("autumn", "ZRF", "BSH") > 0L)
check("timing mediana primavera BSH -> ZRF: positivo (BSH antecipa)", lead("spring", "BSH", "ZRF") > 0L)

pulses_test <- regional_pulses(daily_pair_test, min_individuals = 1L, merge_gap_days = 3L)
check("pulsos: 8 por parque ZRF (4 outono + 4 primavera)", pulses_test[farm == "ZRF", .N] == 8L)
match_test <- regional_match_pulses(pulses_test, pairs_test, season_dates_test, window_days = 10L)
pulse_sum_test <- regional_pulse_summary(match_test)
ps <- function(s, l, f) pulse_sum_test[season == s & leader == l & follower == f]
check("pulsos outono ZRF -> BSH: 4/4 seguidos, lag mediano 5",
      ps("autumn", "ZRF", "BSH")$pct_followed == 100 && ps("autumn", "ZRF", "BSH")$median_lag_days == 5)
check("pulsos outono ZRF -> BSH: nenhum pulso do seguidor ANTES",
      ps("autumn", "ZRF", "BSH")$n_follower_pulse_before == 0L)
check("pulsos primavera BSH -> ZRF: lag mediano 6", ps("spring", "BSH", "ZRF")$median_lag_days == 6)


##
## 3. Epoca sem cobertura -> NA (nao inventa timing)
##
daily_gap_test <- copy(daily_pair_test)
daily_gap_test[farm == "DGY" & day >= as.Date("2025-09-01") & day <= as.Date("2025-12-01"),
               `:=`(monitored = FALSE, max_individuals = NA_integer_)]
timing_gap_test <- regional_passage_timing(daily_gap_test, season_dates_test, min_season_coverage = 0.7)
check("DGY outono com cobertura < 70% -> valid = FALSE e onset NA",
      !timing_gap_test[farm == "DGY" & season == "autumn", valid] &&
        is.na(timing_gap_test[farm == "DGY" & season == "autumn", onset]))


##
## 4. Plots devolvem objetos ggplot (e NULL quando nao ha' dados)
##
weekly_all_test <- rbindlist(lapply(c("ZRF", "BSH", "DGY"), function(f) {
  w <- regional_weekly_max(daily_pair_test[farm == f], min_monitored_days = 4L); w[, farm := f]; w
}))
sp <- "Egyptian-Vulture"
is_gg <- function(p) inherits(p, "ggplot")
check("plot semanal cronologico", is_gg(plot_regional_weekly_timeline(weekly_all_test, sp)))
check("plot sobreposicao anual", is_gg(plot_regional_annual_overlay(weekly_all_test, sp)))
check("plot sobreposicao anual (relativo)", is_gg(plot_regional_annual_overlay(weekly_all_test, sp, normalise = TRUE)))
check("plot series sazonais", is_gg(plot_regional_season_series(daily_pair_test, season_dates_test, sp)))
check("plot acumulado", is_gg(plot_regional_cumulative(daily_pair_test, season_dates_test, sp)))
check("plot ccf", is_gg(plot_regional_ccf(ccf_test, sp)))
check("plot lead days", is_gg(plot_regional_lead_days(lead_test, sp)))
check("plot sem dados -> NULL", is.null(plot_regional_weekly_timeline(weekly_all_test, "Golden-Eagle")))
invisible(lapply(list(
  plot_regional_weekly_timeline(weekly_all_test, sp), plot_regional_annual_overlay(weekly_all_test, sp),
  plot_regional_season_series(daily_pair_test, season_dates_test, sp),
  plot_regional_cumulative(daily_pair_test, season_dates_test, sp),
  plot_regional_ccf(ccf_test, sp), plot_regional_lead_days(lead_test, sp)
), function(p) ggplot2::ggplot_build(p)))
cat("ggplot_build() de todos os plots sem erro -- OK\n")
