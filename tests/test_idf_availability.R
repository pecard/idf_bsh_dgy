## Teste de R/idf_availability.R com dados sinteticos (varias turbinas/unidades)
suppressPackageStartupMessages({ library(data.table); library(lubridate); library(ggplot2); library(scales) })
for (f in c("R/idf_labels.R", "R/availability_daylight.R", "R/offline_curtailment_check.R",
            "R/monthly_technical_summary.R", "R/fatality_window_analysis.R", "R/idf_availability.R")) source(f)
tz_test <- "Asia/Samarkand"
## sem suncalc (ex: sem acesso ao CRAN) usa luz fixa 06:00-18:00 -- o teste cobre a logica, nao a astronomia
if (!requireNamespace("suncalc", quietly = TRUE)) {
  build_daylight_calendar <- function(start_date, end_date, lat, lon, tz) {
    d <- seq(as.Date(start_date, tz = tz), as.Date(end_date, tz = tz), by = "day")
    data.table(date = d, sunrise = as.POSIXct(paste(d, "06:00:00"), tz = tz), sunset = as.POSIXct(paste(d, "18:00:00"), tz = tz), daylight_mins = 720)
  }
}

## 1. relacao geometrica: primaria = maior %, secundarias >= limiar
cov_test <- data.table(turbine = c("T1", "T1", "T1", "T2", "T2"), idf = c("IDF01", "IDF02", "IDF03", "IDF02", "IDF03"),
                       pct_coverage = c(80, 45, 10, 70, 30))
rel_geo_test <- idf_relation_geometric(cov_test, min_pct_coverage = 20)
stopifnot(identical(rel_geo_test[turbine == "T1", idf], c("IDF01", "IDF02")))             # IDF03 (10%) abaixo do limiar
stopifnot(identical(rel_geo_test[turbine == "T1", role], c("Primary", "Secondary")))
stopifnot(identical(rel_geo_test[turbine == "T2", idf], c("IDF02", "IDF03")))
stopifnot(identical(idf_relation_geometric(cov_test, 20, max_secondary = 0)[, .N], 2L))   # so' primarias
## manual (rotulos noutro formato) + fallback geometrico para turbinas fora da matriz
man_test <- data.table(`Turbine ID` = "T1", `Primary IDF` = "IDF-03", `Secondary IDF(s)` = "GW19-01, IDF-02")
rel_res_test <- idf_relation_resolve(cov_test, man_test, 20)
stopifnot(identical(rel_res_test[turbine == "T1", idf], c("IDF03", "IDF01", "IDF02")))
stopifnot(all(rel_res_test[turbine == "T1", relation_source] == "manual"), all(rel_res_test[turbine == "T2", relation_source] == "geometric"))
stopifnot(rel_res_test[turbine == "T1" & idf == "IDF02", pct_coverage] == 45)

## 2. analise: IDF01 sempre online, IDF02 offline 70% do periodo, IDF03 sem heartbeats
map_test <- c(IDF01 = "GW10-01", IDF02 = "GW11-02")     # IDF03 sem codigo de heartbeat
relation_test <- idf_relation_add_unit(rel_geo_test, map_test)
stopifnot(identical(relation_test[idf == "IDF03", unit][1], "IDF03"))
t0_test <- as.POSIXct("2026-06-01 00:00:00", tz = tz_test)
hb_times_test <- seq(t0_test, t0_test + 4 * 86400 - 1, by = "30 min")
hb2_test <- hb_times_test[!(hour(hb_times_test) >= 6 & hour(hb_times_test) < 18 & as.Date(hb_times_test, tz = tz_test) <= as.Date(t0_test, tz = tz_test) + 2)]  # nos 3 primeiros dias nao ha' heartbeats durante o dia (gaps diurnos)
heartb_dt_test <- rbind(data.table(idf = "GW10-01", timestamp = hb_times_test), data.table(idf = "GW11-02", timestamp = hb2_test))
## curtailment em T1 as 09:00 do dia 1, disparado por um track visto pela unidade 2 (GW11-02), nao pela 1
curtl_dt_test <- data.table(turbine = "T1", track_id = "tx", start = t0_test + 9 * 3600, end = t0_test + 9 * 3600 + 120)
track_dt_test <- data.table(track_id = "tx", timestamp = t0_test + 9 * 3600 - 30, idf = 2L)
scada_dt_test <- data.table(turbinelabel = "T2", readingname = "RPM", datetime = t0_test + 10 * 3600, value = 10)
an_test <- idf_availability_analysis(relation_test, heartb_dt_test, curtl_dt_test, scada_dt_test, track_dt_test,
                                     t0_test, t0_test + 4 * 86400 - 1, 41.5, 64.4, tz_test, 60, 30)
unit_test <- idf_unit_summary(relation_test, an_test)
stopifnot(setequal(unit_test$unit, c("GW10-01", "GW11-02", "IDF03")))
stopifnot(unit_test[unit == "GW10-01", offline_pct] == 0)
stopifnot(unit_test[unit == "GW11-02", offline_pct] > 50)
stopifnot(is.na(unit_test[unit == "IDF03", offline_pct]), unit_test[unit == "IDF03", availability_cat] == "Not evaluated")
## o curtailment da turbina T1 so' conta para a unidade que o disparou (GW11-02)
ev_test <- an_test$evidence$combined
stopifnot(any(ev_test[idf == "GW11-02", classification] == "IDF unit communication failure"))
stopifnot(!any(ev_test[idf == "GW10-01", classification] == "IDF unit communication failure"))
stopifnot(unit_test[unit == "GW11-02", net_offline_pct] < unit_test[unit == "GW11-02", offline_pct])
stopifnot(unit_test[unit == "GW11-02", primary_for] == "T2", unit_test[unit == "GW11-02", secondary_for] == "T1")

## 3. alertas
alert_test <- idf_alert_table(unit_test, threshold_pct = 50, basis = "raw")
stopifnot(setequal(alert_test$unit, c("GW11-02", "IDF03")))
stopifnot(alert_test[unit == "IDF03", status] == "No heartbeat records in the period")
stopifnot(grepl("Persistent failure", alert_test[unit == "GW11-02", status]))
## T2: protegida por IDF02 (GW11-02, falha) e IDF03 (sem dados) -> sem nenhuma unidade a funcionar; T1 tem GW10-01 ok
tal_test <- idf_turbine_alert_table(relation_test, alert_test)
stopifnot(identical(tal_test$turbine, "T2"))
rtab_test <- idf_relation_table(relation_test, unit_test)
stopifnot(nrow(rtab_test) == nrow(relation_test), all(c("turbine", "role", "unit", "offline_pct") %in% names(rtab_test)))

## 4. figuras em blocos de unidades
chunks_test <- split_units(c("a", "b", "c", "d", "e"), 2L)
stopifnot(length(chunks_test) == 3L, identical(chunks_test[[3]], "e"))
cal_test <- build_daylight_calendar(as.Date(t0_test), as.Date(t0_test) + 3, 41.5, 64.4, tz_test)
figs_test <- idf_calendar_figures(function(u) offline_evidence_slot_grid(cal_test, tz_test, as.Date(t0_test), as.Date(t0_test) + 3, ev_test, idf_sel = u, slot_mins = 30, no_data_idf = "IDF03"),
                                  split_units(unit_test$unit, 2L))
stopifnot(length(figs_test) == 2L, inherits(figs_test[[1]]$p, "ggplot"))
invisible(ggplot_build(figs_test[[1]]$p))
cat("test_idf_availability OK\n")
stopifnot(identical(normalise_turbine_ids(c("T9", "T35", "DZH-1", "WTG12")), c("T09", "T35", "DZH-1", "WTG12")))
cat("test_idf_availability (ids) OK\n")

## leitura leve dos tracks (so' 3 colunas, so' os track_ids pedidos)
suppressPackageStartupMessages(library(data.table)); source("R/read_utils.R")
d_test <- tempfile("tracks_test_"); dir.create(d_test)
fwrite(data.table(TrackId = c(1, 1, 2, 3), DateTimeStamp = c("2026-06-01 09:00:00", "2026-06-01 09:00:05", "2026-06-01 10:00:00", "2026-06-01 11:00:00"),
                  Latitude = 41, Longitude = 64, TowerNumber = c(13, 13, 15, 99), SpeciesTypeName = "x"),
       file.path(d_test, "TrackReport_Default_TST_20260601.csv"))
tu_test <- read_track_units(d_test, "TrackReport_Default_TST", "Asia/Samarkand", NULL, track_ids = c("1", "3"))
stopifnot(identical(sort(unique(tu_test$track_id)), c("1", "3")), nrow(tu_test) == 3L, setequal(names(tu_test), c("track_id", "timestamp", "idf")))
stopifnot(inherits(tu_test$timestamp, "POSIXct"), attr(tu_test$timestamp, "tzone") == "Asia/Samarkand")
stopifnot(nrow(read_track_units(d_test, "TrackReport_Default_TST", "Asia/Samarkand", NULL)) == 4L)
cat("test read_track_units OK\n")

## unidade CONHECIDA mas sem heartbeats no periodo -> 100% offline (nao "Not evaluated")
an2_test <- idf_availability_analysis(relation_test, heartb_dt_test, curtl_dt_test, scada_dt_test, track_dt_test,
                                      t0_test, t0_test + 4 * 86400 - 1, 41.5, 64.4, tz_test, 60, 30,
                                      data_range = range(heartb_dt_test$timestamp), known_units = c("GW10-01", "GW11-02", "IDF03"))
u2_test <- idf_unit_summary(relation_test, an2_test)
stopifnot(u2_test[unit == "IDF03", offline_pct] == 100, u2_test[unit == "IDF03", availability_cat] == "<60%", u2_test[unit == "IDF03", n_heartbeats] == 0L)
stopifnot(u2_test[unit == "GW10-01", n_heartbeats] > 0L)
al2_test <- idf_alert_table(u2_test, 50, "raw")
stopifnot(al2_test[unit == "IDF03", status] == "Persistent failure: no heartbeat received in the period")
# o intervalo de 100% entra na evidencia (T2 protegida por IDF03 tem SCADA -> "Turbine operational")
stopifnot("IDF03" %in% an2_test$evidence$combined$idf)
# janela fora da cobertura dos dados -> nao conta (Not evaluated)
an3_test <- idf_availability_analysis(relation_test, heartb_dt_test, curtl_dt_test, scada_dt_test, track_dt_test,
                                      t0_test + 30 * 86400, t0_test + 34 * 86400, 41.5, 64.4, tz_test, 60, 30,
                                      data_range = range(heartb_dt_test$timestamp), known_units = c("GW10-01", "GW11-02", "IDF03"))
stopifnot(all(idf_unit_summary(relation_test, an3_test)$availability_cat == "Not evaluated"))
cat("test_idf_availability (known units absent = 100% offline) OK\n")
