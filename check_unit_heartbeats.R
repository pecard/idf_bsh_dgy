##
## Verificacao da disponibilidade de UNIDADES IDF concretas contra o Portal IDF
##
## Para cada unidade: primeiro/ultimo heartbeat nos dados e no periodo, n.o de
## heartbeats, gap inicial (inicio do periodo -> 1.o heartbeat), gap final,
## maior gap interno, e o % offline (so' horas de luz) pelo metodo ANTIGO
## (so' gaps entre heartbeats) e pelo NOVO (com as extremidades do periodo).
## Serve para confrontar com o que o Portal IDF mostra (ex: GW13-8 sem
## leituras em agosto).
##
## Uso: ajustar as 4 linhas abaixo e dar Source na raiz do projeto.
##

project_settings_file <- "userSettings_IDF_availability_ZRF.R"   # settings do parque (fuso, coordenadas, farm_code, limiares)
units_to_check <- c("GW7-5", "GW9-4", "GW12-7", "GW13-8")        # nomes completos (como em heartb_dt$idf)
period_ini <- "2026-08-01"                                       # datas locais, inclusive
period_end <- "2026-09-30"

library(data.table); library(lubridate); library(fst)
if (!requireNamespace("suncalc", quietly = TRUE)) stop("pacote suncalc em falta: install.packages(\"suncalc\")")
source(file.path("inputs", project_settings_file))
source("R/availability_daylight.R")

hb_all <- as.data.table(fst::read_fst(file.path("cache", farm_code, "heartb_dt_unfilt.fst")))
cat("Cobertura dos heartbeats (todas as unidades):", format(min(hb_all$timestamp)), "a", format(max(hb_all$timestamp)), "\n")
cat("Unidades pedidas sem NENHUM heartbeat nos dados:", paste(setdiff(units_to_check, unique(hb_all$idf)), collapse = ", "), "\n\n")

p_from <- as.POSIXct(paste(period_ini, "00:00:00"), tz = proj_timezone)
p_to   <- as.POSIXct(paste(as.Date(period_end) + 1, "00:00:00"), tz = proj_timezone)
cal    <- build_daylight_calendar(as.Date(period_ini), as.Date(period_end), proj_lat, proj_lon, proj_timezone)
rng    <- range(hb_all$timestamp)

rows <- lapply(units_to_check, function(u) {
  all_u <- hb_all[idf == u][order(timestamp)]
  hb <- all_u[timestamp >= p_from & timestamp < p_to]
  if (nrow(hb) == 0L) {
    return(data.table(unit = u, hb_in_period = 0L, first_hb_data = if (nrow(all_u)) format(min(all_u$timestamp)) else NA_character_,
                      last_hb_data = if (nrow(all_u)) format(max(all_u$timestamp)) else NA_character_,
                      note = "no heartbeat in the period"))
  }
  pct <- function(period) {
    d <- daylight_availability(hb, cal, proj_timezone, heartbeat_offline_gap_min, heartbeat_interval_min, data_range = rng)
    summarise_availability(d)$by_idf$monitoring_period_pct
  }
  new_pct <- pct()
  # metodo antigo: so' gaps entre heartbeats (sem period)
  old_iv <- compute_offline_intervals(hb, heartbeat_offline_gap_min, heartbeat_interval_min)
  old_day <- compute_daylight_offline(old_iv, cal, proj_timezone)
  old_pct <- round(100 * sum(old_day$offline_mins, na.rm = TRUE) / sum(cal$daylight_mins), 1)
  gaps <- as.numeric(diff(hb$timestamp), units = "mins")
  data.table(
    unit = u, hb_in_period = nrow(hb),
    first_hb_period = format(min(hb$timestamp)), last_hb_period = format(max(hb$timestamp)),
    lead_gap_h = round(as.numeric(min(hb$timestamp) - max(p_from, rng[1]), units = "hours"), 1),
    trail_gap_h = round(as.numeric(min(p_to, rng[2]) - max(hb$timestamp), units = "hours"), 1),
    longest_internal_gap_h = if (length(gaps)) round(max(gaps) / 60, 1) else NA_real_,
    offline_pct_old = old_pct, offline_pct_new = new_pct
  )
})
res <- rbindlist(rows, fill = TRUE)
print(res)
