##
## Relatorio de DISPONIBILIDADE das unidades IDF -- varias turbinas/unidades
## em simultaneo (qualquer parque)
##
## Heartbeats cruzados com curtailments (so' os disparados pela PROPRIA
## unidade, via TrackReport) e SCADA, para todas as unidades IDF que
## protegem as turbinas pedidas: calendario de evidencia offline, tabela de
## tempo offline por unidade e tabela de alerta de falha persistente.
##
## A relacao turbina <-> unidade primaria/secundaria vem de uma matriz
## MANUAL (se existir) ou e' calculada GEOMETRICAMENTE (R/idf_availability.R).
##
## Uso: definir project_settings_file (modelo de settings:
## inputs/userSettings_IDF_availability_ZRF.R; modelo de lancador:
## run_idf_availability_TEMPLATE.R) e dar Source a este ficheiro.
##

packages <- c(
  'data.table', 'sf', 'magrittr', 'dplyr', 'janitor', 'ggplot2', 'lubridate',
  'readxl', 'writexl', 'rmarkdown', 'flextable', 'fst', 'scales', 'suncalc'
)
for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

options(error = function() message("Skipping failed step"))

folder_input <- "inputs"
tryCatch({
  active_doc_path <- rstudioapi::getActiveDocumentContext()$path
  if (isTRUE(nzchar(active_doc_path))) setwd(dirname(active_doc_path))
}, error = function(e) message("Aviso: a usar a working directory atual: ", getwd()))
if (!dir.exists(folder_input) || !dir.exists("R")) {
  stop("Working directory nao esta na raiz do projeto (esperava 'inputs' e 'R' em ", getwd(), "). Usa setwd() e dá Source (Ctrl+Shift+S).")
}

if (!exists("project_settings_file")) {
  stop("project_settings_file nao definido -- ex: project_settings_file <- \"userSettings_IDF_availability_ZRF.R\" (ver run_idf_availability_TEMPLATE.R).")
}
source(file.path(folder_input, project_settings_file))
if (!exists("force_reread_cache")) force_reread_cache <- FALSE
if (!exists("generate_report")) generate_report <- TRUE
if (!exists("report_team")) report_team <- "Paulo Cardoso; Sandra Rodrigues"

get_code_version <- function() {
  run_git <- function(args) tryCatch(system2("git", args, stdout = TRUE, stderr = FALSE), error = function(e) character(0))
  hash <- run_git(c("log", "-1", "--format=%h"))
  if (length(hash) == 0L || !nzchar(hash[1])) return("unknown (not a git checkout, or git unavailable)")
  commit_date <- run_git(c("log", "-1", "--format=%ci"))
  dirty <- length(run_git(c("status", "--porcelain"))) > 0L
  out <- if (length(commit_date) > 0L && nzchar(commit_date[1])) sprintf("%s (%s)", hash[1], commit_date[1]) else hash[1]
  if (dirty) paste0(out, " + uncommitted local changes") else out
}
code_version <- get_code_version()

## Pasta/ficheiros desta corrida: outputs/<AAAAMMDD_HHMM>_<tag>/ (R/output_paths.R)
source("R/output_paths.R")
period_from_date <- as.Date(ini, tz = proj_timezone)
period_to_date   <- as.Date(end, tz = proj_timezone)
avail_tag     <- paste(farm_code, "IDF_AVAILABILITY", format(period_from_date, "%Y%m%d"), format(period_to_date, "%Y%m%d"), sep = "_")
run_date      <- make_run_stamp()
folder_output <- incident_output_folder("outputs", run_date, avail_tag)
out_file <- function(base, ext) file.path(folder_output, out_name(base, ext, avail_tag, run_date))
dir.create(folder_output, showWarnings = FALSE, recursive = TRUE)
write_run_info(folder_output, avail_tag, run_date, project_settings_file, file.path(folder_input, project_settings_file))


##
## 1. Dados (4 datasets do portal IDF) ----
##

source("R/read_utils.R"); source("R/read_tracks.R"); source("R/read_curtailments.R")
source("R/read_scada.R"); source("R/read_heartbeats.R"); source("R/data_cache.R"); source("R/write_utils.R")

databases_dirs <- databases_dir
folder_cache   <- file.path("cache", farm_code)

curtl_dt_unfilt <- reuse_or_load_cache(
  if (exists("curtl_dt_unfilt")) curtl_dt_unfilt else NULL,
  "curtl_dt_unfilt", file.path(folder_cache, "curtl_dt_unfilt.fst"),
  function() read_curtailments_data(databases_dirs, curtailments_pattern, tz = proj_timezone, farm_pattern = farm_pattern),
  force_reread = force_reread_cache, tz = proj_timezone
)
scada_dt_unfilt <- reuse_or_load_cache(
  if (exists("scada_dt_unfilt")) scada_dt_unfilt else NULL,
  "scada_dt_unfilt", file.path(folder_cache, "scada_dt_unfilt.fst"),
  function() read_scada_data(databases_dirs, scada_pattern, tz = proj_timezone, farm_pattern = farm_pattern),
  force_reread = force_reread_cache, tz = proj_timezone
)
heartb_dt_unfilt <- reuse_or_load_cache(
  if (exists("heartb_dt_unfilt")) heartb_dt_unfilt else NULL,
  "heartb_dt_unfilt", file.path(folder_cache, "heartb_dt_unfilt.fst"),
  function() read_heartbeats_data(databases_dirs, heartbeats_pattern, tz = proj_timezone, farm_pattern = farm_pattern),
  force_reread = force_reread_cache, tz = proj_timezone
)

## So' o periodo analisado
heartb_dt <- as.data.table(heartb_dt_unfilt)[timestamp >= ini & timestamp <= end]
curtl_dt  <- as.data.table(curtl_dt_unfilt)[start >= ini & start <= end]
scada_dt  <- as.data.table(scada_dt_unfilt)[datetime >= ini & datetime <= end]

## Tracks: SO' para atribuir cada curtailment a unidade IDF que o disparou
## (TowerNumber do track) -- leitura leve (3 colunas, so' os tracks dos
## curtailments do periodo), sem cache de tracks. attribute_curtailments_by_unit
## = FALSE nos settings salta isto por completo: os curtailments passam a
## contar por turbina (qualquer unidade) -- ver nota no relatorio.
if (!exists("attribute_curtailments_by_unit")) attribute_curtailments_by_unit <- TRUE
track_dt <- NULL
if (isTRUE(attribute_curtailments_by_unit) && nrow(curtl_dt) > 0L) {
  source("R/idf_availability.R")
  track_dt <- read_track_units(databases_dirs, trackreport_pattern, proj_timezone, farm_pattern, track_ids = curtl_dt$track_id)
  message(sprintf("Tracks lidos so' para atribuicao de curtailments: %d pontos de %d tracks.", nrow(track_dt), uniqueN(track_dt$track_id)))
} else {
  message("Atribuicao de curtailments por unidade desligada (ou sem curtailments no periodo): evidencia por turbina.")
}
message(sprintf("Periodo %s a %s -- heartbeats: %d | curtailments: %d | SCADA: %d linhas",
                format(ini), format(end), nrow(heartb_dt), nrow(curtl_dt), nrow(scada_dt)))
message("Unidades IDF com heartbeats no periodo: ", paste(sort(unique(heartb_dt$idf)), collapse = ", "))


##
## 2. Turbinas, unidades IDF e relacao primaria/secundaria ----
##

source("R/turbine_idf_coverage.R")
source("R/idf_labels.R")
source("R/availability_daylight.R")
source("R/offline_curtailment_check.R")
source("R/monthly_technical_summary.R")
source("R/fatality_window_analysis.R")
source("R/idf_availability.R")

wtg <- sf::read_sf(file.path(folder_input, wtg_filename)) %>% sf::st_transform(crs_projection_plannar)
wtg$InternalNa <- normalise_turbine_ids(wtg[[wtg_source_id_col]])
idf <- sf::read_sf(file.path(folder_input, idf_filename)) %>% sf::st_transform(crs_projection_plannar)
idf$imaging_he <- idf_internal_label(idf[[idf_source_id_col]])

coverage_dt <- compute_turbine_idf_coverage(wtg, idf, buffer_m = idf_op_detection_range, wtg_id_col = "InternalNa", idf_id_col = "imaging_he")

manual_file <- file.path(folder_input, turbine_idf_matrix_filename)
manual_dt <- if (file.exists(manual_file)) readxl::read_xlsx(manual_file) else NULL
message(if (is.null(manual_dt)) "Matriz manual turbina<->IDF nao encontrada -- relacao GEOMETRICA para todas as turbinas." else paste("Matriz manual:", manual_file))

if (!exists("turbines_of_interest")) turbines_of_interest <- NULL
if (!is.null(turbines_of_interest)) turbines_of_interest <- normalise_turbine_ids(turbines_of_interest)

relation_ids_dt <- idf_relation_resolve(coverage_dt, manual_dt, relation_min_pct_coverage, relation_max_secondary, turbines_of_interest)
no_unit_turbines <- setdiff(if (is.null(turbines_of_interest)) wtg$InternalNa else turbines_of_interest, relation_ids_dt$turbine)
if (length(no_unit_turbines) > 0L) message("AVISO: turbinas sem nenhuma unidade IDF a cobri-las (>= ", relation_min_pct_coverage, "%): ", paste(no_unit_turbines, collapse = ", "))

idf_display_map <- build_idf_label_map(heartb_dt_unfilt$idf)
relation_dt <- idf_relation_add_unit(relation_ids_dt, idf_display_map)
no_code_units <- setdiff(unique(relation_dt$idf), names(idf_display_map))
if (length(no_code_units) > 0L) message("AVISO: unidades IDF sem codigo em heartb_dt_unfilt (ficam 'Not evaluated'): ", paste(no_code_units, collapse = ", "))
message(sprintf("Relacao turbina<->IDF: %d turbinas, %d unidades (%d primarias, %d secundarias).",
                uniqueN(relation_dt$turbine), uniqueN(relation_dt$unit), relation_dt[role == "Primary", uniqueN(unit)], relation_dt[role == "Secondary", uniqueN(unit)]))


##
## 3. Disponibilidade: heartbeats x curtailments x SCADA ----
##

analysis <- idf_availability_analysis(
  relation_dt, heartb_dt, curtl_dt, scada_dt, track_dt, ini, end,
  proj_lat, proj_lon, proj_timezone,
  offline_gap_min = heartbeat_offline_gap_min, online_grace_min = heartbeat_interval_min
)
unit_dt <- idf_unit_summary(relation_dt, analysis)
alert_dt <- idf_alert_table(unit_dt, persistent_failure_threshold_pct, persistent_failure_basis)
turbine_alert_dt <- idf_turbine_alert_table(relation_dt, alert_dt)
relation_table_dt <- idf_relation_table(relation_dt, unit_dt)
message(sprintf("Alertas: %d unidade(s) em falha persistente / sem dados; %d turbina(s) sem nenhuma unidade a funcionar.",
                nrow(alert_dt), nrow(turbine_alert_dt)))

## Calendarios (varias figuras, N unidades por figura)
daylight_cal <- build_daylight_calendar(period_from_date, period_to_date, proj_lat, proj_lon, proj_timezone)
not_evaluated_units <- unit_dt[availability_cat == "Not evaluated", unit]
unit_order <- unique(relation_dt[order(turbine, rank), unit])
calendar_figures <- idf_calendar_figures(
  function(u) offline_evidence_slot_grid(daylight_cal, proj_timezone, period_from_date, period_to_date,
                                         analysis$evidence$combined, idf_sel = u, slot_mins = heartbeat_interval_min,
                                         no_data_idf = intersect(u, not_evaluated_units)),
  split_units(unit_order, calendar_units_per_figure), slot_mins = heartbeat_interval_min, date_breaks = calendar_date_breaks
)

write_xlsx_local(
  list(
    Turbine_IDF_relation        = relation_table_dt,
    IDF_unit_offline            = unit_dt,
    Persistent_failure_alerts   = alert_dt,
    Turbines_without_working_unit = turbine_alert_dt,
    Offline_evidence_by_unit    = analysis$evidence$by_idf,
    Offline_evidence_overall    = analysis$evidence$overall,
    Net_availability_overall    = analysis$net_overall,
    IDF_label_map               = idf_label_table(idf_display_map)
  ),
  out_file("idf_availability", "xlsx")
)


##
## 4. Relatorio Word ----
##

report_params <- list(
  title = paste("IDF Availability Report -", project_ref),
  project_ref = project_ref, farm_code = farm_code,
  period_start = as.character(period_from_date), period_end = as.character(period_to_date),
  analysis_date = as.character(Sys.Date()), username = report_team, code_version = code_version,
  attribution_by_unit = !is.null(track_dt),
  n_turbines = uniqueN(relation_dt$turbine), n_units = uniqueN(relation_dt$unit),
  n_primary = relation_dt[role == "Primary", uniqueN(unit)], n_secondary = relation_dt[role == "Secondary", uniqueN(unit)],
  relation_sources = paste(sort(unique(relation_dt$relation_source)), collapse = " / "),
  relation_min_pct_coverage = relation_min_pct_coverage, relation_max_secondary = relation_max_secondary,
  idf_op_detection_range = idf_op_detection_range,
  heartbeat_interval_min = heartbeat_interval_min, heartbeat_offline_gap_min = heartbeat_offline_gap_min,
  persistent_failure_threshold_pct = persistent_failure_threshold_pct, persistent_failure_basis = persistent_failure_basis,
  relation_table = as.data.frame(relation_table_dt), unit_table = as.data.frame(unit_dt),
  alert_table = as.data.frame(alert_dt), turbine_alert_table = as.data.frame(turbine_alert_dt),
  net_overall = if (is.null(analysis$net_overall)) NULL else as.data.frame(analysis$net_overall),
  calendar_figures = calendar_figures,
  xlsx_name = out_name("idf_availability", "xlsx", avail_tag, run_date)
)

if (isTRUE(generate_report)) {
  source("R/report.R")
  build_idf_report(
    output_file    = out_file("IDF_Availability_Report", "docx"),
    report_params  = report_params,
    template       = "report/idf_availability_report_template.rmd",
    reference_docx = file.path(folder_input, "Mod.001.05_template_documentos_gerais.docx")
  )
} else {
  message("generate_report = FALSE -- .docx NAO gerado (tabelas/objetos disponiveis no ambiente e no xlsx em ", folder_output, ").")
}
