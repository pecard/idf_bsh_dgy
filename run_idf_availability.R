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

## Pasta/ficheiros desta corrida: criados mais abaixo, depois de saber que
## turbinas entram na analise (o codigo da turbina faz parte do nome)
source("R/output_paths.R")
period_from_date <- as.Date(ini, tz = proj_timezone)
period_to_date   <- as.Date(end, tz = proj_timezone)
run_date         <- make_run_stamp()


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
## Codigo(s) da(s) turbina(s) em TUDO o que sai: pasta, ficheiros (xlsx/docx),
## titulo e cabecalho do relatorio, titulo dos calendarios e folha "Run_info"
## de cada xlsx. Ate 3 turbinas, os codigos ("T13", "T13-T23"); mais, "<n>WTG".
turbines_analysed <- sort(unique(relation_dt$turbine))
turbine_tag <- if (length(turbines_analysed) <= 3L) paste(turbines_analysed, collapse = "-") else paste0(length(turbines_analysed), "WTG")
turbines_text <- paste(turbines_analysed, collapse = ", ")
avail_tag     <- paste(farm_code, turbine_tag, "IDF_AVAILABILITY", format(period_from_date, "%Y%m%d"), format(period_to_date, "%Y%m%d"), sep = "_")
folder_output <- incident_output_folder("outputs", run_date, avail_tag)
out_file <- function(base, ext) file.path(folder_output, out_name(base, ext, avail_tag, run_date))
dir.create(folder_output, showWarnings = FALSE, recursive = TRUE)
write_run_info(folder_output, avail_tag, run_date, project_settings_file, file.path(folder_input, project_settings_file))

## folha "Run_info" no inicio de cada xlsx: identifica projeto, turbina(s), periodo e corrida
run_info_dt <- data.table(
  Item  = c("Project", "Farm code", "Turbines", "Period start", "Period end", "Run stamp", "Settings file"),
  Value = c(project_ref, farm_code, turbines_text, as.character(period_from_date), as.character(period_to_date), run_date, project_settings_file)
)
write_xlsx_tagged <- function(x, path, ...) write_xlsx_local(c(list(Run_info = run_info_dt), x), path, ...)

message(sprintf("Relacao turbina<->IDF: %d turbinas, %d unidades (%d primarias, %d secundarias).",
                uniqueN(relation_dt$turbine), uniqueN(relation_dt$unit), relation_dt[role == "Primary", uniqueN(unit)], relation_dt[role == "Secondary", uniqueN(unit)]))


##
## 3. Disponibilidade: heartbeats x curtailments x SCADA ----
##

analysis <- idf_availability_analysis(
  relation_dt, heartb_dt, curtl_dt, scada_dt, track_dt, ini, end,
  proj_lat, proj_lon, proj_timezone,
  offline_gap_min = heartbeat_offline_gap_min, online_grace_min = heartbeat_interval_min,
  data_range = range(heartb_dt_unfilt$timestamp), # cobertura dos dados de heartbeat de TODAS as unidades
  known_units = unique(heartb_dt_unfilt$idf)      # unidades que existem nos dados: sem heartbeats no periodo = 100% offline
)
unit_dt <- idf_unit_summary(relation_dt, analysis)
alert_dt <- idf_alert_table(unit_dt, persistent_failure_threshold_pct, persistent_failure_basis)
turbine_alert_dt <- idf_turbine_alert_table(relation_dt, alert_dt)
relation_table_dt <- idf_relation_table(relation_dt, unit_dt)
message(sprintf("Alertas: %d unidade(s) em falha persistente / sem dados; %d turbina(s) sem nenhuma unidade a funcionar.",
                nrow(alert_dt), nrow(turbine_alert_dt)))

## Protecao remanescente SEM a unidade primaria (geometrica): % da area de
## raio turbine_area_radius_m em redor da turbina coberta pelas restantes
## unidades (secundarias, terciarias, ... em uniao)
if (!exists("turbine_area_radius_m")) turbine_area_radius_m <- 1000
remaining_cov_dt <- idf_remaining_coverage(relation_dt, wtg, idf, unit_buffer_m = idf_op_detection_range, turbine_area_m = turbine_area_radius_m)

## Curtailments disparados por cada unidade IDF, por turbina: especie de
## interesse (species_of_interest; NULL = todas) e todas as especies.
## Atribuicao pelo track que disparou o curtailment -- precisa dos tracks
## (attribute_curtailments_by_unit = TRUE).
if (!exists("species_of_interest")) species_of_interest <- NULL
curtl_by_unit_dt <- NULL
curtl_totals_dt <- NULL
if (!is.null(track_dt)) {
  ## SO' a especie de interesse e SO' o periodo estudado, por turbina, para as
  ## unidades da relacao (as que contribuem para a turbina). Os TrackReport
  ## costumam ser descarregados so' para essas unidades, por isso curtailments
  ## sem track ou atribuidos a outras unidades NAO sao interpretados: so'
  ## contados a parte (totais). Sem comparacao com o parque todo.
  res_list <- lapply(turbines_analysed, function(tb) {
    rel_tb <- relation_dt[turbine == tb][order(rank)]
    r <- summarise_curtailments_contributing_units(curtl_dt, track_dt, tb, ini, end, rel_tb$unit, idf_display_map, species = species_of_interest)
    by_unit <- r$by_unit
    by_unit[rel_tb, on = "unit", `:=`(role = i.role, rank = i.rank, pct_coverage = i.pct_coverage)]
    by_unit[unit_dt, on = "unit", offline_pct := i.offline_pct]   # % offline de cada unidade no periodo
    by_unit[, turbine := tb]
    setcolorder(by_unit, c("turbine", "unit", "role", "rank", "pct_coverage", "offline_pct", "n", "pct"))
    list(by_unit = by_unit, totals = cbind(turbine = tb, r$totals))
  })
  curtl_by_unit_dt <- rbindlist(lapply(res_list, `[[`, "by_unit"), fill = TRUE)
  curtl_totals_dt  <- rbindlist(lapply(res_list, `[[`, "totals"))
  message("Curtailments da especie de interesse por turbina: ", paste(sprintf("%s: %d (atribuidos a unidades da relacao: %d; outras unidades: %d; sem track: %d)",
          curtl_totals_dt$turbine, curtl_totals_dt$n_total, curtl_totals_dt$n_in_relation, curtl_totals_dt$n_other_units, curtl_totals_dt$n_no_track), collapse = " | "))
}
curtl_unattributed_note <- ""

## Calendarios (varias figuras, N unidades por figura)
daylight_cal <- build_daylight_calendar(period_from_date, period_to_date, proj_lat, proj_lon, proj_timezone)
not_evaluated_units <- unit_dt[availability_cat == "Not evaluated", unit]
unit_order <- unique(relation_dt[order(turbine, rank), unit])
calendar_figures <- idf_calendar_figures(
  function(u) offline_evidence_slot_grid(daylight_cal, proj_timezone, period_from_date, period_to_date,
                                         analysis$evidence$combined, idf_sel = u, slot_mins = heartbeat_interval_min,
                                         no_data_idf = intersect(u, not_evaluated_units)),
  split_units(unit_order, calendar_units_per_figure), slot_mins = heartbeat_interval_min, date_breaks = calendar_date_breaks,
  turbine_label = turbines_text
)


##
## 3b. Falhas reportadas pela equipa IDF: padrao das cameras WFOV em 3 periodos ----
##

malfunction_events_df <- NULL
wfov_results <- NULL
if (exists("idf_malfunctions") && !is.null(idf_malfunctions) && nrow(idf_malfunctions) > 0L) {
  source("R/idf_malfunction_wfov.R")
  mf <- as.data.frame(idf_malfunctions, stringsAsFactors = FALSE)
  if (!"turbine" %in% names(mf)) mf$turbine <- NA_character_
  if (!"days_before" %in% names(mf)) mf$days_before <- 7
  if (!"days_after" %in% names(mf)) mf$days_after <- 7
  if (!"days_pre" %in% names(mf)) mf$days_pre <- 0
  mf$days_pre[is.na(mf$days_pre)] <- 0
  if (!exists("wfov_fall_threshold")) wfov_fall_threshold <- 0.33
  if (!exists("wfov_prior_theta_sd")) wfov_prior_theta_sd <- 1
  mf$event_time <- as.POSIXct(as.character(mf$datetime), tz = proj_timezone)
  mf$is_wfov <- grepl("WFOV", mf$type, ignore.case = TRUE)
  wf <- mf[mf$is_wfov, , drop = FALSE]

  wfov_dt <- NULL
  if (nrow(wf) > 0L) {
    wfov_dt <- tryCatch(
      read_track_wfov(databases_dirs, trackreport_pattern, proj_timezone, farm_pattern, units = wf$unit,
                      from = min(wf$event_time - (wf$days_before + wf$days_pre) * 86400), to = max(wf$event_time + wf$days_after * 86400)),
      error = function(e) { message("Leitura WFOV_camera falhou: ", conditionMessage(e)); NULL })
  }
  wfov_range <- if (is.null(wfov_dt)) NULL else attr(wfov_dt, "all_range")
  no_col <- if (is.null(wfov_dt)) character(0) else attr(wfov_dt, "files_without_column")

  wfov_results <- lapply(seq_len(nrow(wf)), function(i) {
    ev <- wf$event_time[i]; d_b <- wf$days_before[i]; d_a <- wf$days_after[i]; d_p <- wf$days_pre[i]
    label <- sprintf("%s -- %s", wf$unit[i], format(ev, "%Y-%m-%d %H:%M"))
    res <- if (is.null(wfov_dt)) NULL else wfov_three_period(wfov_dt, wf$unit[i], ev, d_p, d_b, d_a)
    bayes <- if (is.null(wfov_dt)) NULL else tryCatch(
      wfov_bayes_total(wfov_dt, wf$unit[i], ev, d_p, d_b, d_a, fall_threshold = wfov_fall_threshold, prior_theta_sd = wfov_prior_theta_sd, data_range = wfov_range),
      error = function(e) { message("Modelo bayesiano WFOV falhou: ", conditionMessage(e)); NULL })
    tb <- if (!is.na(wf$turbine[i]) && nzchar(wf$turbine[i])) trimws(strsplit(wf$turbine[i], ",")[[1]]) else NULL
    ctx <- wfov_event_context(wf$unit[i], ev, d_b, d_a, heartb_dt_unfilt, curtl_dt_unfilt, scada_dt_unfilt, tb, days_pre = d_p)
    covered <- !is.null(wfov_range) && wfov_range[1L] <= ev - (d_b + d_p) * 86400 && wfov_range[2L] >= ev + d_a * 86400
    note <- if (is.null(wfov_dt) || nrow(wfov_dt) == 0L) {
      if (length(no_col) > 0L) "The TrackReport files read have no WFOV camera column." else "No TrackReport data with a WFOV camera were read for this unit and window."
    } else if (!covered) {
      sprintf("The TrackReport files cover %s to %s, which does not include the whole comparison window; the comparison is partial.",
              format(wfov_range[1L], "%Y-%m-%d"), format(wfov_range[2L], "%Y-%m-%d"))
    } else ""
    list(label = label, unit = wf$unit[i], event_time = format(ev, "%Y-%m-%d %H:%M"), type = wf$type[i],
         days_pre = d_p, days_before = d_b, days_after = d_a, has_pre = !is.null(res) && isTRUE(res$has_pre),
         rates = if (is.null(res)) NULL else res$rates, totals = if (is.null(res)) NULL else res$totals,
         bayes = if (is.null(bayes) || is.null(bayes$summary)) NULL else list(summary = as.data.frame(bayes$summary), sensitivity = as.data.frame(bayes$sensitivity),
                                                    n_days_excluded = bayes$n_days_excluded,
                                                    plain = wfov_plain_summary(bayes$summary, bayes$sensitivity)),
         context = ctx, note = note,
         fig_rates = if (is.null(res)) NULL else plot_wfov_rates(res, label),
         fig_posterior = if (is.null(bayes) || is.null(bayes$summary)) NULL else plot_wfov_posterior(bayes, label),
         fig_daily = if (is.null(res)) NULL else plot_wfov_daily(res, label))
  })

  malfunction_events_df <- data.frame(
    `IDF Unit` = mf$unit, `Reported date/time` = format(mf$event_time, "%Y-%m-%d %H:%M"), `Reported malfunction` = mf$type,
    Assessment = ifelse(mf$is_wfov, "WFOV tracks, 3 periods; Bayesian assessment of the unit total (this report)", "Not assessable from the portal data"),
    check.names = FALSE
  )
  message(sprintf("Falhas reportadas: %d (%d WFOV avaliadas).", nrow(mf), nrow(wf)))
}

write_xlsx_tagged(
  list(
    Turbine_IDF_relation        = relation_table_dt,
    IDF_unit_offline            = unit_dt,
    Remaining_coverage_without_primary = remaining_cov_dt,
    Curtailments_by_IDF_unit_totals = if (is.null(curtl_totals_dt)) data.table(Note = "n/a") else curtl_totals_dt,
    Curtailments_by_IDF_unit    = if (is.null(curtl_by_unit_dt)) data.table(Note = "Curtailment attribution by unit is off (attribute_curtailments_by_unit = FALSE) or there are no curtailments.") else curtl_by_unit_dt,
    Persistent_failure_alerts   = alert_dt,
    Turbines_without_working_unit = turbine_alert_dt,
    Offline_evidence_by_unit    = analysis$evidence$by_idf,
    Offline_evidence_overall    = analysis$evidence$overall,
    Net_availability_overall    = analysis$net_overall,
    IDF_label_map               = idf_label_table(idf_display_map)
  ) |> c(
    if (!is.null(malfunction_events_df)) list(Reported_malfunctions = malfunction_events_df),
    if (!is.null(wfov_results)) list(
      WFOV_camera_rates = data.table::rbindlist(lapply(wfov_results, function(r) if (is.null(r$rates)) NULL else cbind(`IDF Unit` = r$unit, Event = r$event_time, r$rates)), fill = TRUE),
      WFOV_periods = data.table::rbindlist(lapply(wfov_results, function(r) if (is.null(r$totals)) NULL else cbind(`IDF Unit` = r$unit, Event = r$event_time, r$totals)), fill = TRUE),
      WFOV_bayes_total = data.table::rbindlist(lapply(wfov_results, function(r) if (is.null(r$bayes)) NULL else cbind(`IDF Unit` = r$unit, Event = r$event_time, r$bayes$summary, plain_language = r$bayes$plain)), fill = TRUE),
      WFOV_bayes_sensitivity = data.table::rbindlist(lapply(wfov_results, function(r) if (is.null(r$bayes)) NULL else cbind(`IDF Unit` = r$unit, Event = r$event_time, r$bayes$sensitivity)), fill = TRUE),
      WFOV_event_context = data.table::rbindlist(lapply(wfov_results, function(r) cbind(`IDF Unit` = r$unit, Event = r$event_time, r$context)), fill = TRUE)
    )
  ),
  out_file("idf_availability", "xlsx")
)


##
## 4. Relatorio Word ----
##

## Cobertura temporal de cada dataset (anexo de metodologia). Tracks: so' os lidos para atribuicao.
fmt_dt <- function(x) if (length(x) == 0L || all(is.na(x))) NA_character_ else format(x, "%Y-%m-%d %H:%M")
data_sources_df <- data.frame(
  Dataset = c("Heartbeats", "Curtailments", "SCADA", "Tracks (only those that triggered curtailments)"),
  First = c(fmt_dt(min(heartb_dt_unfilt$timestamp)), fmt_dt(min(curtl_dt_unfilt$start)), fmt_dt(min(scada_dt_unfilt$datetime)),
            if (is.null(track_dt)) NA_character_ else fmt_dt(min(track_dt$timestamp))),
  Last = c(fmt_dt(max(heartb_dt_unfilt$timestamp)), fmt_dt(max(curtl_dt_unfilt$start)), fmt_dt(max(scada_dt_unfilt$datetime)),
           if (is.null(track_dt)) NA_character_ else fmt_dt(max(track_dt$timestamp))),
  Rows = c(format(nrow(heartb_dt_unfilt), big.mark = ","), format(nrow(curtl_dt_unfilt), big.mark = ","), format(nrow(scada_dt_unfilt), big.mark = ","),
           if (is.null(track_dt)) "not read" else format(nrow(track_dt), big.mark = ",")),
  check.names = FALSE
)

report_params <- list(
  data_sources = data_sources_df, timezone_name = proj_timezone,
  title = paste("IDF Availability Report -", project_ref, "-", turbines_text),
  project_ref = project_ref, farm_code = farm_code,
  period_start = as.character(period_from_date), period_end = as.character(period_to_date),
  analysis_date = as.character(Sys.Date()), username = report_team, code_version = code_version,
  turbines_text = turbines_text, avail_tag = avail_tag,
  attribution_by_unit = !is.null(track_dt),
  n_turbines = uniqueN(relation_dt$turbine), n_units = uniqueN(relation_dt$unit),
  n_primary = relation_dt[role == "Primary", uniqueN(unit)], n_secondary = relation_dt[role == "Secondary", uniqueN(unit)],
  relation_sources = paste(sort(unique(relation_dt$relation_source)), collapse = " / "),
  relation_min_pct_coverage = relation_min_pct_coverage, relation_max_secondary = relation_max_secondary,
  idf_op_detection_range = idf_op_detection_range,
  heartbeat_interval_min = heartbeat_interval_min, heartbeat_offline_gap_min = heartbeat_offline_gap_min,
  persistent_failure_threshold_pct = persistent_failure_threshold_pct, persistent_failure_basis = persistent_failure_basis,
  relation_table = as.data.frame(relation_table_dt), unit_table = as.data.frame(unit_dt),
  remaining_table = as.data.frame(remaining_cov_dt), turbine_area_radius_m = turbine_area_radius_m,
  curtl_by_unit_table = if (is.null(curtl_by_unit_dt)) NULL else as.data.frame(curtl_by_unit_dt), curtl_totals_table = if (is.null(curtl_totals_dt)) NULL else as.data.frame(curtl_totals_dt), species_of_interest = if (is.null(species_of_interest)) "" else paste(species_of_interest, collapse = ", "), curtl_unattributed_note = curtl_unattributed_note,
  alert_table = as.data.frame(alert_dt), turbine_alert_table = as.data.frame(turbine_alert_dt),
  net_overall = if (is.null(analysis$net_overall)) NULL else as.data.frame(analysis$net_overall),
  calendar_figures = calendar_figures,
  malfunction_events = malfunction_events_df,
  wfov_fall_threshold = if (exists("wfov_fall_threshold")) wfov_fall_threshold else 0.33, wfov_prior_theta_sd = if (exists("wfov_prior_theta_sd")) wfov_prior_theta_sd else 1,
  wfov_results = wfov_results,
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
