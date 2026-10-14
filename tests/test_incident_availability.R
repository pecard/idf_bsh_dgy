## Teste: unidades sem heartbeats na janela -> "Not evaluated" (NA), nao 0%
suppressPackageStartupMessages({ library(data.table); library(lubridate) })
source("R/availability_daylight.R"); source("R/fatality_window_analysis.R")
tz_test <- "Asia/Samarkand"
## sem o pacote suncalc (ex: ambiente sem acesso ao CRAN), usa um calendario
## de luz fixo 06:00-18:00 -- o teste verifica a logica de "Not evaluated",
## nao o calculo astronomico
if (!requireNamespace("suncalc", quietly = TRUE)) {
  build_daylight_calendar <- function(start_date, end_date, lat, lon, tz) {
    d <- seq(as.Date(start_date, tz = tz), as.Date(end_date, tz = tz), by = "day")
    data.table(date = d, sunrise = as.POSIXct(paste(d, "06:00:00"), tz = tz), sunset = as.POSIXct(paste(d, "18:00:00"), tz = tz), daylight_mins = 720)
  }
}
t0_test <- as.POSIXct("2026-05-19 00:00:00", tz = tz_test)
slots_test <- seq(t0_test + hours(6), t0_test + days(3) - hours(1), by = "30 min")
heartb_dt_test <- rbind(
  data.table(idf = "IDF13", timestamp = slots_test),                                   # sempre online
  data.table(idf = "IDF15", timestamp = slots_test[!(slots_test >= t0_test + hours(30) & slots_test < t0_test + hours(36))]) # gap diurno
)
units_test <- c("IDF13", "IDF15", "IDF19")  # IDF19 sem heartbeats
av_test <- summarise_availability_window(heartb_dt_test, units_test, t0_test, t0_test + days(3) - seconds(1),
                                         lat = 41.58, lon = 64.39, tz = tz_test)
b_test <- av_test$by_idf
stopifnot(setequal(b_test$idf, units_test))
stopifnot(b_test[idf == "IDF13", monitoring_period_pct] == 0)
stopifnot(b_test[idf == "IDF15", monitoring_period_pct] > 0)
stopifnot(is.na(b_test[idf == "IDF19", monitoring_period_pct]), as.character(b_test[idf == "IDF19", availability_cat]) == "Not evaluated")
# todas as unidades sem heartbeats
av_none_test <- summarise_availability_window(heartb_dt_test[0], units_test, t0_test, t0_test + days(3) - seconds(1), 41.58, 64.39, tz_test)
stopifnot(nrow(av_none_test$by_idf) == 3, all(is.na(av_none_test$by_idf$monitoring_period_pct)))
# calendario: unidade sem dados -> "No data" nos slots diurnos
cal_test <- build_daylight_calendar(as.Date(t0_test), as.Date(t0_test) + 2, 41.58, 64.39, tz_test)
ev_test <- data.table(idf = character(), off_start = as.POSIXct(character(), tz = tz_test), off_end = as.POSIXct(character(), tz = tz_test), classification = character())
g_test <- offline_evidence_slot_grid(cal_test, tz_test, as.Date(t0_test), as.Date(t0_test) + 2, ev_test, idf_sel = units_test, no_data_idf = "IDF19")
stopifnot("No data" %in% g_test[idf == "IDF19", as.character(slot_status)], !"No data" %in% g_test[idf == "IDF13", as.character(slot_status)])
stopifnot("Online" %in% g_test[idf == "IDF13", as.character(slot_status)])
cat("test_incident_availability OK\n")
