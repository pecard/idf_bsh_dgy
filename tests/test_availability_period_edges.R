## Regressao: tempo ANTES do 1.o / DEPOIS do ultimo heartbeat conta como offline
## (caso real ZRF GW13-8: sem heartbeats em agosto, reaparece a 16 set, mostrava 0%)
suppressPackageStartupMessages({ library(data.table); library(lubridate) })
source("R/availability_daylight.R")
tz_test <- "Asia/Samarkand"
if (!requireNamespace("suncalc", quietly = TRUE)) {
  build_daylight_calendar <- function(start_date, end_date, lat, lon, tz) {
    d <- seq(as.Date(start_date, tz = tz), as.Date(end_date, tz = tz), by = "day")
    data.table(date = d, sunrise = as.POSIXct(paste(d, "06:00:00"), tz = tz), sunset = as.POSIXct(paste(d, "18:00:00"), tz = tz), daylight_mins = 720)
  }
}
t0_test <- as.POSIXct("2026-08-01 00:00:00", tz = tz_test)
n_days_test <- 40
all_test <- seq(t0_test, t0_test + n_days_test * 86400 - 1, by = "30 min")
cal_test <- build_daylight_calendar(as.Date(t0_test, tz = tz_test), as.Date(t0_test, tz = tz_test) + n_days_test - 1, 41.5, 64.4, tz_test)
pct <- function(dt, u) {
  d <- daylight_availability(dt, cal_test, tz_test, 60, 30, data_range = range(all_test))
  s <- summarise_availability(d)$by_idf
  s[idf == u, monitoring_period_pct]
}

## A: sempre online; B: so' reaparece a meio (dia 21 -> 50% do periodo sem heartbeats)
hb_test <- rbind(data.table(idf = "A", timestamp = all_test),
                 data.table(idf = "B", timestamp = all_test[all_test >= t0_test + 20 * 86400]))
stopifnot(pct(hb_test, "A") == 0)
stopifnot(abs(pct(hb_test, "B") - 50) < 1)                        # antes: 0%

## C: para de reportar a meio (gap final); D: 25% no inicio e 25% no fim sem dados
hb2_test <- rbind(data.table(idf = "A", timestamp = all_test),
                  data.table(idf = "C", timestamp = all_test[all_test < t0_test + 20 * 86400]),
                  data.table(idf = "D", timestamp = all_test[all_test >= t0_test + 10 * 86400 & all_test < t0_test + 30 * 86400]))
stopifnot(abs(pct(hb2_test, "C") - 50) < 1)
stopifnot(abs(pct(hb2_test, "D") - 50) < 1)

## cobertura dos dados: se NENHUMA unidade tem dados depois de certa data (ficheiros
## so' ate' la'), esse tempo NAO conta como offline
cut_test <- t0_test + 30 * 86400
hb3_test <- hb_test[timestamp < cut_test]
d3_test <- daylight_availability(hb3_test, cal_test, tz_test, 60, 30, data_range = range(hb3_test$timestamp))
stopifnot(summarise_availability(d3_test)$by_idf[idf == "A", monitoring_period_pct] == 0)
## o intervalo offline devolvido esta' dentro do periodo
iv_test <- compute_offline_intervals(hb_test, 60, 30, period = calendar_period(cal_test, tz_test), data_range = range(all_test))
stopifnot(all(iv_test$off_start >= t0_test), all(iv_test$off_end <= t0_test + n_days_test * 86400))
stopifnot(iv_test[idf == "B", min(off_start)] == t0_test)
## comportamento antigo (sem period) inalterado
stopifnot(nrow(compute_offline_intervals(hb_test, 60, 30)[idf == "B"]) == 0L)
cat("test_availability_period_edges OK\n")
