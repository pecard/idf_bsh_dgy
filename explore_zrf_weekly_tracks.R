##
## Zarafshan (ZRF) -- observacoes (tracks) por semana, por especie, no periodo com dado
##
## Pedido do Paulo (2026-11): recuperar a leitura dos tracks de Zarafshan e mostrar os padroes
## temporais semanais das especies prioritarias (so' ZRF, sem comparar com outros projetos), com
## 3 graficos, mais um diagnostico das unidades IDF ativas por semana:
##   1. n.o de tracks por especie e semana (so' cores, sem numeros);
##   2. presenca/ausencia semanal (como o grafico "Weekly presence of priority species" da
##      comparacao BSH vs DGY);
##   3. intensidade semanal por unidade IDF (tracks por dia por unidade IDF ativa), 1 linha por
##      especie com escala de y propria;
##   +  diagnostico: n.o de unidades IDF ativas por semana e atividade de cada unidade -- o n.o de
##      unidades a recolher dados varia no tempo (avarias, unidades novas) e as contagens brutas
##      dependem dele; o grafico 3 normaliza por esse n.o. Uma unidade conta como ativa numa semana
##      se registou >= 1 ponto de track (mesma definicao de R/regional_assessment.R); os tracks nao
##      distinguem unidade sem aves de unidade avariada (os heartbeats dariam o funcionamento real).
##
## Reutiliza, sem codigo novo de raiz:
##   - a leitura/cache de tracks de explore_bsh_dgy_comparison.R (import_farm_data():
##     reuse_or_load_cache() + read_tracks_data(), cache/<farm_code>/track_dt_unfilt.fst), so' tracks;
##   - a logica semanal e o heatmap de weekly_species_presence() /
##     plot_species_presence_heatmap() (bins de 7 dias ancorados a uma data, mesmo tema), agora em
##     R/weekly_track_counts.R com contagens; unidades ativas: logica de
##     regional_weekly_active_units() (R/regional_assessment.R).
##
## NAO faz parte do pipeline dos relatorios -- script AUTONOMO (como explore_bsh_dgy_comparison.R).
## Semanas: bins de 7 dias a partir do 1.o dia com dados; a ultima semana pode ser parcial.
##
## Settings: por omissao os dados mais recentes de ZRF (TrackReport_Default_ZRF, ate' outubro de 2026,
## inputs/userSettings_IDF_availability_ZRF.R). Para os dados antigos do relatorio anual
## (TrackReport_, ate' agosto), definir antes de dar Source:
##   zrf_settings_file <- "userSettings_ZRF.R"
## Especies mostradas: species_filter (abaixo; editar a lista, nomes como em track_dt$spec).
##
## Uso: editar force_reread_cache abaixo se necessario e dar Source (source("explore_zrf_weekly_tracks.R")).
## Nota: o script nao foi corrido com dados reais no sandbox; as funcoes de
## R/weekly_track_counts.R estao testadas com dados sinteticos (tests/test_weekly_track_counts.R).
##

##
## PACKAGES ----
##

packages <- c('data.table', 'ggplot2', 'lubridate', 'janitor', 'fst', 'rstudioapi', 'writexl')
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
  stop("Working directory nao esta na raiz do projeto (esperava 'inputs' e 'R' em ", getwd(), ").")
}

source("R/write_utils.R")
source("R/read_utils.R")
source("R/read_tracks.R")
source("R/data_cache.R")
source("R/weekly_track_counts.R")

if (!exists("force_reread_cache")) force_reread_cache <- FALSE
if (!exists("zrf_settings_file")) zrf_settings_file <- "userSettings_IDF_availability_ZRF.R"
run_stamp <- format(Sys.time(), "%Y%m%d_%H%M")
folder_output <- file.path("outputs", "zrf_tracks_weekly", run_stamp)
dir.create(folder_output, showWarnings = FALSE, recursive = TRUE)


##
## 1. Leitura dos tracks de ZRF (com cache) ----
##

source(file.path(folder_input, zrf_settings_file))
databases_dirs <- unique(c(databases_dir, if (exists("databases_dir_alt")) databases_dir_alt))
folder_cache <- file.path("cache", farm_code)

## Especies a mostrar (nomes como em track_dt$spec)
species_filter <- c(
  'Steppe-Eagle', 'Bearded-Vulture', 'Egyptian-Vulture', 'Eurasian-Or-Himalayan-Griffon', 'Cinereous-Vulture',
  'Golden-Eagle', 'Imperial-Eagle', 'Saker-Falcon', 'White-Tailed-Eagle'
)

## current = NULL sempre: ver o comentario equivalente em import_farm_data()
## (explore_bsh_dgy_comparison.R) -- nunca reutilizar um track_dt_unfilt de outra corrida
track_dt_unfilt <- reuse_or_load_cache(
  NULL,
  "track_dt_unfilt", file.path(folder_cache, "track_dt_unfilt.fst"),
  function() read_tracks_data(databases_dirs, trackreport_pattern, tz = proj_timezone,
                              farm_pattern = if (exists("farm_pattern")) farm_pattern else NULL),
  force_reread = force_reread_cache, tz = proj_timezone
)
track_dt <- as.data.table(track_dt_unfilt)
message(sprintf("Tracks lidos: %s registos, %s tracks, de %s a %s.",
                format(nrow(track_dt), big.mark = ","), format(uniqueN(track_dt$track_id), big.mark = ","),
                format(min(track_dt$timestamp), "%Y-%m-%d"), format(max(track_dt$timestamp), "%Y-%m-%d")))


##
## 2. Contagem semanal por especie ----
##
## Periodo = 1.o ao ultimo dia com tracks (nao o ini/end dos settings): "periodo com dado".

wk <- weekly_track_counts(track_dt, species_filter, tz = proj_timezone)
weekly_counts <- wk$weekly
species_summary <- summarise_weekly_track_counts(weekly_counts)
week_starts <- sort(unique(weekly_counts$week_start))
message(sprintf("Periodo com dado: %s a %s (%d semanas, a ultima %s).",
                wk$window$start, wk$window$end, length(week_starts),
                if (any(weekly_counts$is_partial)) "parcial" else "completa"))
absent_sp <- setdiff(species_filter, unique(track_dt$spec))
if (length(absent_sp) > 0L) message("Aviso: especies do filtro sem nenhum track nos dados: ", paste(absent_sp, collapse = ", "))

## Unidades IDF ativas por semana (diagnostico) e intensidade por unidade
active_dt <- weekly_active_units(track_dt, proj_timezone, wk$window$start, wk$window$end)
active_summary <- summarise_active_units(active_dt, week_starts, partial_weeks = unique(weekly_counts[is_partial == TRUE, week_start]))
unit_activity <- summarise_unit_activity(active_dt, week_starts)
intensity <- weekly_intensity_per_unit(weekly_counts, active_summary)
message(sprintf("Unidades IDF ativas por semana: min %d, mediana %.0f, max %d (%d semana(s) abaixo de 80%% da mediana; %d unidades distintas).",
                min(active_summary$n_active_units), median(active_summary$n_active_units), max(active_summary$n_active_units),
                sum(active_summary$low_vs_median), uniqueN(active_dt$unit)))

period_txt <- sprintf("%s to %s", wk$window$start, wk$window$end)
p_counts    <- plot_weekly_track_counts_colours(weekly_counts, title = sprintf("Zarafshan -- weekly number of tracks by species (%s)", period_txt))
p_presence  <- plot_weekly_presence(weekly_counts, title = sprintf("Zarafshan -- weekly presence of priority species (%s)", period_txt))
p_intensity <- plot_weekly_intensity_per_unit(intensity, title = "Zarafshan -- weekly track intensity per IDF unit (mean tracks/day/IDF unit)")
p_units     <- plot_weekly_active_units(active_summary, title = sprintf("Zarafshan -- IDF units active per week (%s)", period_txt))
p_unit_tiles <- plot_unit_activity_tiles(active_dt, week_starts, title = sprintf("Zarafshan -- IDF unit activity by week (%s)", period_txt))

h_species <- 0.32 * length(species_filter) + 1.8
save_plot <- function(p, name, height) ggsave(file.path(folder_output, sprintf("zrf_weekly_%s_%s.png", name, run_stamp)), p, width = 11, height = height, dpi = 200, limitsize = FALSE)
save_plot(p_counts, "tracks_by_species", h_species)
save_plot(p_presence, "presence", h_species)
save_plot(p_intensity, "intensity_per_unit", 0.62 * length(species_filter) + 1.8)
save_plot(p_units, "active_units", 3.4)
save_plot(p_unit_tiles, "unit_activity", 0.2 * max(5, uniqueN(active_dt$unit)) + 1.8)


##
## 3. Exportacao (xlsx em ingles) ----
##

run_info <- data.table(
  Item = c("Farm", "Settings file", "TrackReport pattern", "Data period (first to last day with tracks)", "Weeks (7 days, from the first day)",
           "Tracks (distinct)", "Records", "Time zone", "Run"),
  Value = c(farm_code, zrf_settings_file, trackreport_pattern, sprintf("%s to %s", wk$window$start, wk$window$end),
            as.character(uniqueN(weekly_counts$week_start)), as.character(uniqueN(track_dt$track_id)),
            as.character(nrow(track_dt)), proj_timezone, run_stamp))

write_xlsx_local(
  list(
    Weekly_counts = weekly_counts,
    Species_summary = species_summary,
    Intensity_per_unit = intensity,
    Active_units_per_week = active_summary,
    Unit_activity_range = unit_activity,
    Run_info = run_info
  ),
  file.path(folder_output, sprintf("zrf_weekly_tracks_%s.xlsx", run_stamp))
)
message("Gravado em: ", normalizePath(folder_output))
print(p_counts); print(p_presence); print(p_intensity); print(p_units); print(p_unit_tiles)
