##
## Zarafshan (ZRF) -- observacoes (tracks) por semana, por especie, no periodo com dado
##
## Pedido do Paulo (2026-11): recuperar a leitura dos tracks de Zarafshan e fazer um grafico
## semelhante ao da comparacao BSH vs DGY (explore_bsh_dgy_comparison.R, secao 2: 1 tile por
## especie x semana), mas com as QUANTIDADES de observacoes por semana em vez de
## presenca/ausencia, para o periodo em que ha' dados.
##
## Reutiliza, sem codigo novo de raiz:
##   - a leitura/cache de tracks de explore_bsh_dgy_comparison.R (import_farm_data():
##     reuse_or_load_cache() + read_tracks_data(), cache/<farm_code>/track_dt_unfilt.fst), so' tracks;
##   - a logica semanal e o heatmap de weekly_species_presence() /
##     plot_species_presence_heatmap() (bins de 7 dias ancorados a uma data, mesmo tema), agora em
##     R/weekly_track_counts.R com contagens (tracks distintos, registos, dias com tracks).
##
## NAO faz parte do pipeline dos relatorios -- script AUTONOMO (como explore_bsh_dgy_comparison.R).
## Semanas: bins de 7 dias a partir do 1.o dia com dados; a ultima semana pode ser parcial (marcada).
##
## Settings: por omissao os dados mais recentes de ZRF (TrackReport_Default_ZRF, ate' outubro de 2026,
## inputs/userSettings_IDF_availability_ZRF.R). Para os dados antigos do relatorio anual
## (TrackReport_, ate' agosto), definir antes de dar Source:
##   zrf_settings_file <- "userSettings_ZRF.R"
## As especies prioritarias vem sempre de inputs/userSettings_ZRF.R (prioritysp).
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

## As especies prioritarias vem do settings anual de ZRF, num ambiente a parte (para nao
## sobrescrever databases_dir/farm_code/etc. do settings acima)
env_zrf <- new.env()
sys.source(file.path(folder_input, "userSettings_ZRF.R"), envir = env_zrf)
prioritysp <- env_zrf$prioritysp

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

wk <- weekly_track_counts(track_dt, prioritysp, tz = proj_timezone)
weekly_counts <- wk$weekly
species_summary <- summarise_weekly_track_counts(weekly_counts)
message(sprintf("Periodo com dado: %s a %s (%d semanas, a ultima %s).",
                wk$window$start, wk$window$end, uniqueN(weekly_counts$week_start),
                if (any(weekly_counts$is_partial)) "parcial" else "completa"))

p_heatmap <- plot_weekly_track_counts_heatmap(
  weekly_counts, title = sprintf("Zarafshan -- weekly number of tracks by species (%s to %s)", wk$window$start, wk$window$end))
p_bars <- plot_weekly_track_counts_bars(
  weekly_counts, prioritysp, title = sprintf("Zarafshan -- weekly number of tracks (%s to %s)", wk$window$start, wk$window$end))

ggsave(file.path(folder_output, sprintf("zrf_weekly_tracks_heatmap_%s.png", run_stamp)), p_heatmap,
       width = 11, height = 0.32 * (1 + length(prioritysp)) + 1.6, dpi = 200)
ggsave(file.path(folder_output, sprintf("zrf_weekly_tracks_bars_%s.png", run_stamp)), p_bars,
       width = 11, height = 3.6, dpi = 200)


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
    Run_info = run_info
  ),
  file.path(folder_output, sprintf("zrf_weekly_tracks_%s.xlsx", run_stamp))
)
message("Gravado em: ", normalizePath(folder_output))
print(p_heatmap)
print(p_bars)
