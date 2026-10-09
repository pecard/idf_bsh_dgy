##
## Regional assessment -- Zarafshan (ZRF), Bash (BSH) e Djangeldy (DGY)
##
## 3 parques a ~100 km em triangulo (ZRF a norte, BSH a oeste, DGY a este).
## Duas abordagens (ver R/regional_assessment.R para a logica e as hipoteses):
##   1. Padrao anual: maximo SEMANAL do numero minimo de individuos (bins de
##      2 min) de Egyptian-Vulture e Steppe-Eagle, nos 3 parques.
##   2. Antecipacao: ZRF antecipa a chegada a BSH/DGY no outono, e BSH/DGY
##      antecipam a chegada a ZRF na primavera (timing da passagem,
##      correlacao cruzada defasada, e pulsos seguidos no outro parque).
##
## Le TODOS os tracks dos 3 parques (data-raw + pasta remota, via os
## userSettings de cada um, ver farm_settings abaixo) -- NAO filtra por
## ini/end de cada parque. Usa a cache fst de cada parque (cache/<farm_code>/
## track_dt_unfilt.fst, a mesma de IDF_analysis.R/run_incident_zrshan.R);
## force_reread_cache = TRUE quando ha' dados novos nas pastas brutas.
##
## NAO faz parte do pipeline dos relatorios (nunca chamado a partir de
## IDF_analysis.R/IDF_monthly_report.R) -- script AUTONOMO, mesma logica de
## explore_bsh_dgy_comparison.R.
##
## Fase 1 (este ficheiro): todos os parametros no bloco "PARAMETROS" abaixo.
## Fase 2: mover esse bloco para inputs/userSettings_regional.R e dar source
## a esse ficheiro aqui (o resto do script nao muda).
##
## Uso: source("run_regional_assessment.R") (Ctrl+Shift+S no RStudio).
## Outputs: outputs/<AAAAMMDD>_REGIONAL/ -- xlsx (em ingles) + PNGs, todos com
## o sufixo _REGIONAL_<AAAAMMDD> (R/output_paths.R).
##


##
## PACKAGES ----
##

packages <- c(
  'data.table', 'lubridate', 'ggplot2', 'scales', 'janitor', 'fst', 'rstudioapi', 'writexl'
)
for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

tryCatch({
  active_doc_path <- rstudioapi::getActiveDocumentContext()$path
  if (isTRUE(nzchar(active_doc_path))) setwd(dirname(active_doc_path))
}, error = function(e) {
  message(sprintf(
    "Aviso: nao foi possivel mudar para a pasta do script via rstudioapi (%s) -- a usar a working directory atual: %s",
    conditionMessage(e), getwd()
  ))
})

folder_input <- "inputs"
if (!dir.exists(folder_input) || !dir.exists("R")) {
  stop(sprintf(
    "Working directory nao esta na raiz do projeto (esperava 'inputs' e 'R' aqui): %s", getwd()
  ))
}

source("R/write_utils.R")
source("R/read_utils.R")
source("R/read_tracks.R")
source("R/data_cache.R")
source("R/output_paths.R")
source("R/track_min_individuals.R")
source("R/regional_assessment.R")


##
## PARAMETROS ----
##

## Parque -> settings file (de onde vem databases_dir/databases_dir_alt, os
## padroes de ficheiros, o fuso, etc.). ZRF usa o settings mais recente; os
## padroes/pastas dos tracks sao os mesmos em todos os settings ZRF.
farm_settings <- c(
  ZRF = "userSettings_ZRF_T94_20261001.R",
  BSH = "userSettings_BSH.R",
  DGY = "userSettings_DGY.R"
)

## Pastas ADICIONAIS com tracks, por parque (alem de databases_dir e
## databases_dir_alt dos settings). BSH e DGY ja' tem a pasta de rede em
## databases_dir_alt; ZRF NAO tem databases_dir_alt nos settings.
## TODO(Paulo): confirmar a pasta de rede de ZRF (ex: .../IDF_PortalData/ZRF)
## e preencher aqui, ex: ZRF = "//192.168.1.11/DadosBrutos(T2)/.../IDF_PortalData/ZRF"
extra_dirs <- list(ZRF = NULL, BSH = NULL, DGY = NULL)

if (!exists("force_reread_cache")) force_reread_cache <- FALSE # TRUE = relê os brutos (dados novos)
force_recompute_bins <- FALSE # TRUE = recalcula os bins de 2 min mesmo com cache

species_regional <- c("Egyptian-Vulture", "Steppe-Eagle")

## Bins de 2 min (R/track_min_individuals.R) -- mesmos valores dos relatorios
bin_min <- 2
merge_dist_m <- 200

## Esforco de monitorizacao (so' ha' tracks quando ha' aves, por isso o
## esforco e' inferido dos proprios tracks, de qualquer especie)
min_tracks_per_day <- 10   # tracks distintos/dia (qualquer especie) para o dia contar como monitorizado
min_monitored_days <- 4    # dias monitorizados minimos para uma semana ter valor (senao NA)

## Epocas: c("MM-DD inicio", "MM-DD fim"), aplicadas a cada ano com dados
seasons <- list(
  autumn = c("08-01", "11-30"), # movimentos pos-nupciais para sul (ZRF lidera)
  spring = c("02-15", "05-31")  # movimentos pre-nupciais para norte (BSH/DGY lideram)
)

## Pares lider -> seguidor por epoca (hipotese: o lider antecipa o seguidor)
regional_pairs <- data.table(
  season   = c("autumn", "autumn", "spring", "spring"),
  leader   = c("ZRF", "ZRF", "BSH", "DGY"),
  follower = c("BSH", "DGY", "ZRF", "ZRF")
)

## Timing da passagem: fracao acumulada para onset / mediana / fim
onset_share <- 0.10; median_share <- 0.50; end_share <- 0.90
min_season_total <- 3          # soma minima dos maximos diarios na epoca para calcular timing
min_season_coverage <- 0.7     # fracao minima de dias monitorizados da epoca

## Correlacao cruzada defasada
max_lag_days <- 21             # lags testados: -21 a +21 dias
smooth_days <- 3               # media movel centrada das series diarias (1 = sem suavizar)
min_overlap_days <- 20         # dias minimos com dados nos 2 parques para calcular r

## Pulsos (episodios de presenca)
pulse_min_individuals <- 1     # maximo diario minimo para contar como dia de pulso
pulse_merge_gap_days <- 3      # dias de intervalo ainda considerados o mesmo pulso
pulse_window_days <- 10        # janela para procurar o pulso no parque seguidor

run_date <- format(Sys.time(), "%Y%m%d")
regional_tag <- "REGIONAL"
folder_output <- incident_output_folder("outputs", run_date, regional_tag)
dir.create(folder_output, showWarnings = FALSE, recursive = TRUE)
out_file <- function(base, ext) file.path(folder_output, out_name(base, ext, regional_tag, run_date))
folder_cache_regional <- file.path("cache", "regional")


##
## 1. Leitura dos tracks e series por parque ----
##

farm_data <- list()
for (farm in names(farm_settings)) {

  fd <- regional_load_farm(
    farm_settings[[farm]], farm, folder_input = folder_input,
    extra_dirs = extra_dirs[[farm]], force_reread = force_reread_cache
  )

  mon <- regional_monitored_days(fd$tracks, fd$tz, min_tracks_per_day = min_tracks_per_day)

  bins <- regional_min_individuals_bins(
    fd$tracks, species_regional, bin_min = bin_min, merge_dist_m = merge_dist_m,
    cache_file = file.path(folder_cache_regional, sprintf("min_individuals_bins_%s.rds", farm)),
    force_recompute = force_recompute_bins
  )

  daily <- regional_daily_max(bins, species_regional, mon, fd$tz)
  daily[, farm := farm]

  farm_data[[farm]] <- list(
    daily = daily, monitored = mon, tz = fd$tz,
    n_tracks_rows = nrow(fd$tracks)
  )

  rm(fd, bins); invisible(gc())
}

daily_dt <- rbindlist(lapply(farm_data, `[[`, "daily"))
setcolorder(daily_dt, c("farm", "spec", "day", "max_individuals", "monitored"))

weekly_dt <- rbindlist(lapply(names(farm_data), function(f) {
  w <- regional_weekly_max(daily_dt[farm == f], min_monitored_days = min_monitored_days)
  w[, farm := f]
  w
}))
setcolorder(weekly_dt, c("farm", "spec", "week_start", "iso_year", "iso_week", "n_days_monitored", "max_individuals"))

data_coverage_dt <- rbindlist(lapply(names(farm_data), function(f) {
  m <- farm_data[[f]]$monitored
  data.table(farm = f, first_day = m$first_day, last_day = m$last_day,
             n_days_monitored = length(m$monitored), n_track_rows = farm_data[[f]]$n_tracks_rows)
}))
print(data_coverage_dt)


##
## 2. Epocas com dados e antecipacao entre parques ----
##

years <- seq(year(min(daily_dt$day)), year(max(daily_dt$day)))
season_dates <- regional_season_dates(seasons, years)
has_data <- vapply(seq_len(nrow(season_dates)), function(i) {
  daily_dt[day >= season_dates$from[i] & day <= season_dates$to[i], any(monitored)]
}, logical(1))
season_dates <- season_dates[has_data]
message("Epocas com dados: ", paste(sprintf("%s %d", season_dates$season, season_dates$season_year), collapse = ", "))

timing_dt <- regional_passage_timing(
  daily_dt, season_dates, onset_share = onset_share, median_share = median_share, end_share = end_share,
  min_total = min_season_total, min_season_coverage = min_season_coverage
)
lead_dt <- regional_lead_table(timing_dt, regional_pairs)

ccf_dt <- regional_ccf(
  daily_dt, regional_pairs, season_dates, max_lag_days = max_lag_days,
  smooth_days = smooth_days, min_overlap_days = min_overlap_days
)
ccf_summary_dt <- regional_ccf_summary(ccf_dt)

pulses_dt <- regional_pulses(daily_dt, min_individuals = pulse_min_individuals, merge_gap_days = pulse_merge_gap_days)
pulse_match_dt <- regional_match_pulses(pulses_dt, regional_pairs, season_dates, window_days = pulse_window_days)
pulse_summary_dt <- regional_pulse_summary(pulse_match_dt)

cat("\n--- Timing difference (days, positive = follower later, leader anticipates) ---\n"); print(lead_dt)
cat("\n--- Lagged correlation, best lag ---\n"); print(ccf_summary_dt)
cat("\n--- Pulse follow-up ---\n"); print(pulse_summary_dt)


##
## 3. Plots ----
##

save_plot <- function(p, base, width = 10, height = 7) {
  if (is.null(p)) { message("Sem dados para o plot '", base, "' -- nao gravado."); return(invisible(NULL)) }
  ggplot2::ggsave(out_file(base, "png"), p, width = width, height = height, dpi = 150)
}

n_panels <- nrow(season_dates)
for (sp in species_regional) {
  tag <- gsub("[^A-Za-z]", "", sp)
  save_plot(plot_regional_weekly_timeline(weekly_dt, sp), paste0("weekly_max_timeline_", tag), height = 8)
  save_plot(plot_regional_annual_overlay(weekly_dt, sp), paste0("annual_pattern_", tag), height = 6)
  save_plot(plot_regional_annual_overlay(weekly_dt, sp, normalise = TRUE), paste0("annual_pattern_relative_", tag), height = 6)
  save_plot(plot_regional_season_series(daily_dt, season_dates, sp, smooth_days), paste0("seasonal_series_", tag), height = 3 * max(n_panels, 1))
  save_plot(plot_regional_cumulative(daily_dt, season_dates, sp, onset_share, median_share, end_share), paste0("cumulative_passage_", tag), height = 3 * max(n_panels, 1))
  save_plot(plot_regional_ccf(ccf_dt, sp), paste0("lagged_correlation_", tag), height = 3 * max(n_panels, 1))
  save_plot(plot_regional_lead_days(lead_dt, sp), paste0("timing_difference_", tag), height = 3 * max(n_panels, 1))
}


##
## 4. Tabelas (xlsx, tudo em ingles) ----
##

parameters_dt <- data.table(
  Parameter = c("Species", "Bin (min)", "Merge distance (m)", "Min tracks per monitored day",
                "Min monitored days per week", "Seasons", "Leader -> follower pairs",
                "Passage shares (onset / median / end)", "Min season total", "Min season coverage",
                "Max lag (days)", "Smoothing (days)", "Min overlap (days)",
                "Pulse min individuals", "Pulse merge gap (days)", "Pulse follow-up window (days)", "Run date"),
  Value = c(
    paste(species_regional, collapse = ", "), bin_min, merge_dist_m, min_tracks_per_day, min_monitored_days,
    paste(sprintf("%s %s to %s", names(seasons), vapply(seasons, `[`, "", 1), vapply(seasons, `[`, "", 2)), collapse = "; "),
    paste(sprintf("%s: %s -> %s", regional_pairs$season, regional_pairs$leader, regional_pairs$follower), collapse = "; "),
    paste(onset_share, median_share, end_share, sep = " / "), min_season_total, min_season_coverage,
    max_lag_days, smooth_days, min_overlap_days, pulse_min_individuals, pulse_merge_gap_days, pulse_window_days, run_date
  )
)

write_xlsx_local(
  list(
    Parameters = parameters_dt, Data_coverage = data_coverage_dt,
    Weekly_max = weekly_dt, Daily_max = daily_dt,
    Passage_timing = timing_dt, Timing_difference = lead_dt,
    Lagged_correlation = ccf_dt, Lagged_correlation_best = ccf_summary_dt,
    Pulses = pulses_dt, Pulse_followup = pulse_match_dt, Pulse_followup_summary = pulse_summary_dt
  ),
  out_file("regional_assessment", "xlsx")
)

message("\nRegional assessment concluido -- outputs em: ", folder_output)
