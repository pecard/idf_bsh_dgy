## ----------- HEADER -----------
##**********************************************
##
##   Identiflight (IDF) data analysis -- MONTHLY REPORT
##
##**********************************************
##
## Reusa os mesmos modulos R/*.R de IDF_analysis.R, mas restringe a analise
## a 1 mes de calendario (report_month, abaixo) e a um subconjunto de
## seccoes -- ver inputs/monthlyReportSettings.R para a lista completa e a
## justificacao de cada exclusao (fatality investigation, coverage 3D,
## curtailment removal risk, turbine clustering, etc. ficam fora deste
## relatorio).
##
## Le os MESMOS 4 datasets em cache (cache/*.fst) que IDF_analysis.R --
## correr IDF_analysis.R pelo menos 1 vez antes (ou definir
## force_reread_cache_monthly = TRUE abaixo depois de descarregar dados
## novos) para a cache existir.
##
## Property of Bioinsight, Lda.
## Reproduction or sharing of any part of this
## script is prohibited without explicit permission.
##
##**********************************************


#coment this line for debug!!!
options(error = function() message("Skipping failed step"))

##
## PACKAGES ----
##

## Subconjunto do IDF_analysis.R -- este script nao usa terra/RANN/plotly/
## cluster (coverage 3D e turbine clustering ficam fora do relatorio
## mensal); sf e' usado so' para localizar as turbinas no plot espacial de
## disponibilidade (secção 1); officedown/officer suportam a secção em
## landscape da tabela 7.1 (report/monthly_report_template.rmd) -- nao sao
## precisos por IDF_analysis.R (usa rmarkdown::word_document simples, sem
## secções landscape)
packages <- c('purrr', 'rstudioapi',
              'tidyverse', 'lubridate', 'ggplot2',
              'scales', 'readxl', 'janitor', 'sf',
              'flextable', 'systemfonts',
              'openxlsx', 'writexl', 'rmarkdown',
              'officedown', 'officer',
              'data.table', 'suncalc',
              'fst')

for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}


##
## SETTINGS ----
##

## report_month: definir ANTES de dar source a este script, ex:
##   report_month <- "2026-01"
##   source("IDF_monthly_report.R")
## Verificado ja aqui, antes de qualquer uso de report_month abaixo (ex:
## folder_output) -- monthlyReportSettings.R confia que ja esta definido e
## nao repete esta verificacao.
if (!exists("report_month")) {
  stop('Definir report_month (formato "YYYY-MM", ex: "2026-07") antes de correr IDF_monthly_report.R')
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
folder_input  <- "inputs"
folder_output <- file.path("outputs", "monthly", report_month)

dir.create(folder_output, showWarnings = FALSE, recursive = TRUE)
dir.create(folder_input,  showWarnings = FALSE, recursive = TRUE)

folder_script <- "scripts\\"
Rfiles <- list.files(folder_script, pattern = '.R', full.names = TRUE)
lapply(Rfiles, function(x) source(x))

source(file.path(folder_input, "monthlyReportSettings.R"))


##
## QUALITY STANDARDS ----
##

username <- check_username()

sink(file.path(folder_output, "R_analysis_info.txt"))
cat(paste0('Analysis technician: ', username, '\n'))
cat(paste0('Analysis date: ', format(Sys.time(), "%Y-%m-%d"), '\n'))
cat(paste0('Report month: ', report_month, '\n'))
sink()


##
## ANALYSIS ----
##

##
## 0. Import data (le a MESMA cache que IDF_analysis.R) ----
##

source("R/write_utils.R")
source("R/read_utils.R")
source("R/read_tracks.R")
source("R/read_curtailments.R")
source("R/read_scada.R")
source("R/read_heartbeats.R")
source("R/data_cache.R")

databases_dirs <- unique(c(databases_dir, if (exists("databases_dir_alt")) databases_dir_alt))

folder_cache <- "cache"
force_reread_cache_monthly <- FALSE # -> TRUE so depois de descarregar dados novos (a cache e' partilhada com IDF_analysis.R)

track_dt_unfilt <- load_or_read_cache(
  file.path(folder_cache, "track_dt_unfilt.fst"),
  function() read_tracks_data(databases_dirs, trackreport_pattern, tz = proj_timezone),
  force_reread = force_reread_cache_monthly, tz = proj_timezone
)
curtl_dt_unfilt <- load_or_read_cache(
  file.path(folder_cache, "curtl_dt_unfilt.fst"),
  function() read_curtailments_data(databases_dirs, curtailments_pattern, tz = proj_timezone),
  force_reread = force_reread_cache_monthly, tz = proj_timezone
)
scada_dt_unfilt <- load_or_read_cache(
  file.path(folder_cache, "scada_dt_unfilt.fst"),
  function() read_scada_data(databases_dirs, scada_pattern, tz = proj_timezone),
  force_reread = force_reread_cache_monthly, tz = proj_timezone
)
heartb_dt_unfilt <- load_or_read_cache(
  file.path(folder_cache, "heartb_dt_unfilt.fst"),
  function() read_heartbeats_data(databases_dirs, heartbeats_pattern, tz = proj_timezone),
  force_reread = force_reread_cache_monthly, tz = proj_timezone
)

## Localizacao das turbinas + matriz manual turbina<->IDF -- so' para o plot
## espacial de disponibilidade (secção 1); nenhuma outra seccao do relatorio
## mensal precisa do shapefile ou da matriz
wtg <- sf::read_sf(file.path(folder_input, wtg_filename))
wtg <- sf::st_transform(wtg, crs_projection_plannar)

turbine_idf_matrix_file <- file.path(folder_input, turbine_idf_matrix_filename)
if (file.exists(turbine_idf_matrix_file)) {
  turbine_idf_manual_dt <- readxl::read_xlsx(turbine_idf_matrix_file)
} else {
  message("Matriz turbina<->IDF nao disponivel -- plot espacial de disponibilidade (secção 1) sera saltado.")
  turbine_idf_manual_dt <- NULL
}


##
## 0. Filter data for the report month (ini/end vem de monthlyReportSettings.R) ----
##

report_start <- as.Date(ini)
report_end   <- as.Date(end)

scada_dt <- scada_dt_unfilt # NAO FILTRAR -- mesma logica de IDF_analysis.R

## Todos os filtros de data abaixo usam a MESMA sintaxe (data.table `[...]`,
## nao dplyr::filter()) e as MESMAS 2 variaveis auxiliares (filter_ini/
## filter_end), nunca `ini`/`end` diretamente dentro de um `DT[...]` --
## curtl_dt_unfilt TEM uma coluna chamada "end" (fim do proprio curtailment,
## ver R/read_curtailments.R), e dentro de DT[...] nomes de coluna tomam
## precedencia sobre variaveis do ambiente com o mesmo nome. Usar `ini`/`end`
## bare dentro do filtro de curtl_dt_unfilt fazia "start <= end" comparar
## com a COLUNA end (sempre >= o seu proprio start -- quase sempre TRUE),
## nao com o limite do periodo do relatorio -- bug real, encontrado 2026-08
## via monthly_data_summary_dt a mostrar Curtailments date_to muito alem do
## mes do relatorio. filter_ini/filter_end nao colidem com nenhuma coluna
## de curtl_dt_unfilt/track_dt_unfilt/heartb_dt_unfilt, nem com
## report_start/report_end (Date, definidos acima, usados em nomes de
## ficheiro) -- usados por omissao em TODOS os filtros, mesmo nos que hoje
## nao tem risco de colisao, para nao depender de confirmar caso a caso.
filter_ini <- ini
filter_end <- end

curtl_dt <- as.data.table(curtl_dt_unfilt)[start >= filter_ini & start <= filter_end]

track_dt <- as.data.table(track_dt_unfilt)[timestamp >= filter_ini & timestamp <= filter_end]
# count (nº de pontos por track) recalculado so' para o mes do relatorio --
# mesma correcao de IDF_analysis.R (0. Filter data), ver R/read_tracks.R
track_dt[, count := .N, by = track_id]

## Sem filtro por unidade IDF -- ao contrario de IDF_analysis.R
## (heartbeat_idf_units, um subconjunto historico com cobertura parcial), o
## relatorio mensal quer a disponibilidade de TODAS as unidades (heartbeats
## disponiveis para as 79 turbinas)
heartb_dt <- as.data.table(heartb_dt_unfilt)[timestamp >= filter_ini & timestamp <= filter_end]

# janela de SCADA disponivel para este mes -- interseccao entre a janela fixa
# de disponibilidade de SCADA (scada_ini/scada_end, monthlyReportSettings.R)
# e o mes do relatorio, para nao incluir historico fora do mes nem assumir
# SCADA antes de scada_ini
scada_ini_monthly <- max(scada_ini, ini)
scada_end_monthly <- min(scada_end, end)


##
## 0. Data extent / quantities table (tabela inicial de quantidades e janela temporal) ----
##

source("R/dataset_summary.R")

monthly_data_summary_dt <- summarise_dataset_extent(list(
  Tracks       = list(dt = track_dt,  date_col = "timestamp"),
  Curtailments = list(dt = curtl_dt,  date_col = "start"),
  SCADA        = list(dt = scada_dt[datetime >= scada_ini_monthly & datetime <= scada_end_monthly], date_col = "datetime"),
  Heartbeats   = list(dt = heartb_dt, date_col = "timestamp")
))

write_xlsx_local(
  list(Monthly_data_summary = monthly_data_summary_dt),
  file.path(folder_output, sprintf("data_summary_%s.xlsx", report_month))
)


##
## 1. System availability (heartbeats) ----
##

if (exists("heartb_dt") && isTRUE(run_sections_monthly$system_availability)) {

  source("R/availability_daylight.R")

  daylight_cal <- build_daylight_calendar(ini, end, proj_lat, proj_lon, proj_timezone)

  idf_availability_dt <- daylight_availability(
    heartb_dt, daylight_cal, proj_timezone,
    offline_gap_min  = heartbeat_offline_gap_min,
    online_grace_min = heartbeat_interval_min
  )

  idf_availability_summary <- summarise_availability(idf_availability_dt)

  idf_sel <- idf_availability_summary$by_idf[
    order(-offline_mins_total)][seq_len(min(idf_availability_top_n, .N)), idf]

  p_availability_cal <- plot_availability_calendar(
    idf_availability_dt, idf_availability_summary$by_idf,
    idf_sel = idf_sel, top_n = idf_availability_top_n
  )
  ggsave(
    file.path(folder_output, sprintf("idf_availability_calendar_%s.png", report_month)),
    plot = p_availability_cal, width = 15, height = 20, units = "cm", dpi = 300, bg = "white"
  )

  p_availability_freq <- plot_availability_frequency(idf_availability_summary$by_idf)
  ggsave(
    file.path(folder_output, sprintf("idf_availability_frequency_%s.png", report_month)),
    plot = p_availability_freq, width = 8, height = 8, units = "cm", dpi = 300, bg = "white"
  )

  write_xlsx_local(
    list(By_idf = idf_availability_summary$by_idf, By_month = idf_availability_summary$by_month),
    file.path(folder_output, sprintf("idf_availability_%s.xlsx", report_month))
  )

  ## 1b. Spatial unavailability -- 1 ponto por turbina (localizacao real),
  ## tamanho/cor = % offline da sua unidade IDF primaria neste mes (ver
  ## R/availability_daylight.R, join_availability_to_turbine()/
  ## plot_availability_spatial()). So' corre se a matriz manual
  ## turbina<->IDF estiver disponivel (ver "0. Import data").
  if (!is.null(turbine_idf_manual_dt)) {

    turbine_availability_dt <- join_availability_to_turbine(
      idf_availability_summary$by_idf, wtg, turbine_idf_manual_dt, wtg_id_col = "InternalNa"
    )

    write_xlsx_local(
      list(Turbine_availability = turbine_availability_dt),
      file.path(folder_output, sprintf("idf_availability_spatial_%s.xlsx", report_month))
    )

    p_availability_spatial <- plot_availability_spatial(turbine_availability_dt)
    ggsave(
      file.path(folder_output, sprintf("idf_availability_spatial_%s.png", report_month)),
      plot = p_availability_spatial, width = 220, height = 180, units = "mm", dpi = 300, bg = "white"
    )

  } else {message("Matriz turbina<->IDF nao disponivel -- 1b (spatial unavailability) saltada nesta ronda.")}

} else {message("heartb_dt nao disponivel ou run_sections_monthly$system_availability = FALSE -- 1 (system availability) saltada nesta ronda.")}


##
## 2. Observed species (riqueza de especies observadas) ----
##

if (exists("track_dt") && isTRUE(run_sections_monthly$observed_species)) {

  source("R/id_transitions.R")

  monthly_richness_dt <- track_species_summary(track_dt)
  monthly_richness_summary <- summarise_species_richness(monthly_richness_dt)

  write_xlsx_local(
    list(
      Track_species_richness = monthly_richness_dt,
      Richness_by_n_species  = monthly_richness_summary$by_n_species,
      Richness_rate          = monthly_richness_summary$rate,
      Richness_entropy       = monthly_richness_summary$entropy
    ),
    file.path(folder_output, sprintf("observed_species_%s.xlsx", report_month))
  )

  p_monthly_richness <- plot_species_richness_hist(monthly_richness_dt)
  ggsave(
    file.path(folder_output, sprintf("observed_species_richness_hist_%s.png", report_month)),
    plot = p_monthly_richness, width = 8, height = 8, units = "cm", dpi = 300, bg = "white"
  )

} else {message("track_dt nao disponivel ou run_sections_monthly$observed_species = FALSE -- 2 (observed species) saltada nesta ronda.")}


##
## 3. Curtailments by species ----
##

if (exists("curtl_dt") && isTRUE(run_sections_monthly$curtailment_by_species)) {

  source("R/curtailment_species.R")

  monthly_species_curt_dt <- classify_curtailment_species(curtl_dt, prioritysp, nonprioritysp, othersp)
  monthly_species_curt_by_species_dt <- summarise_curtailment_species(monthly_species_curt_dt)
  monthly_species_curt_by_group_dt   <- summarise_curtailment_species_group(monthly_species_curt_dt)

  if (monthly_species_curt_by_group_dt[species_group == "uncategorized", .N] > 0) {
    message(sprintf(
      "Aviso: %d curtailments com species fora de prioritysp/nonprioritysp/othersp neste mes.",
      monthly_species_curt_by_group_dt[species_group == "uncategorized", n]
    ))
  }

  write_xlsx_local(
    list(
      Curtailments_by_species = monthly_species_curt_by_species_dt,
      Curtailments_by_group   = monthly_species_curt_by_group_dt
    ),
    file.path(folder_output, sprintf("curtailment_species_%s.xlsx", report_month))
  )

} else {message("curtl_dt nao disponivel ou run_sections_monthly$curtailment_by_species = FALSE -- 3 (curtailments by species) saltada nesta ronda.")}


##
## 4. Short-track curtailments ----
##

if (exists("track_dt") && exists("curtl_dt") && isTRUE(run_sections_monthly$short_track_curtailment)) {

  source("R/curtailment_short_track.R")

  monthly_short_track_dt <- classify_short_track_curtailments(
    track_dt, curtl_dt, min_points = shorttrack_min_points, eval_range_m = shorttrack_eval_range
  )
  monthly_short_track_summary_dt <- summarise_short_track_curtailments(track_dt, monthly_short_track_dt, shorttrack_min_points)
  monthly_short_track_by_species_dt <- summarise_short_track_by_species(monthly_short_track_dt, prioritysp)

  write_xlsx_local(
    list(
      Short_track_summary    = monthly_short_track_summary_dt,
      Short_track_by_species = monthly_short_track_by_species_dt,
      Short_track_detail     = monthly_short_track_dt
    ),
    file.path(folder_output, sprintf("curtailment_short_track_%s.xlsx", report_month))
  )

} else {message("track_dt/curtl_dt nao disponiveis ou run_sections_monthly$short_track_curtailment = FALSE -- 4 (short-track curtailment) saltada nesta ronda.")}


##
## 5. Curtailment response, shutdown time and safe distance (delays) ----
##

if (exists("scada_dt") && isTRUE(run_sections_monthly$curtailment_response_delays)) {

  source("R/curtailment_response.R")
  source("R/curtailment_shutdown_time.R")
  source("R/curtailment_safe_distance.R")

  curtl_scada_dt <- curtl_dt[
    turbine %in% turbinas_scada & start >= scada_ini_monthly & start <= scada_end_monthly
  ]

  ### 5.1 Avaliacao principal (baseline apertado + resposta imediata + delta start->end)
  assess_dt <- assess_curtailment_response(
    curtl_scada_dt, scada_dt,
    start_end_gap_sec = curtailment_start_end_gap_sec, max_next_gap_sec = curtailment_max_next_gap_sec,
    drop_pct_threshold = curtailment_drop_pct_threshold, rpm_threshold = safe_shutdown_rpm
  )
  summary_assess <- summarise_curtailment_assessment(assess_dt)

  write_xlsx_local(
    list(
      Assessment   = assess_dt,
      By_status    = summary_assess$by_status,
      By_immediate = summary_assess$by_immediate,
      By_turbine   = summary_assess$by_turbine
    ),
    file.path(folder_output, sprintf("curtailment_response_assessment_%s.xlsx", report_month))
  )

  ### 5.2 Tempo ate atingir limiares de RPM (2, 1, 0), por curtailment
  tt_dt <- time_to_rpm_thresholds(
    curtl_scada_dt, scada_dt, thresholds = shutdown_time_thresholds,
    start_end_gap_sec = curtailment_start_end_gap_sec
  )
  summary_tt_by_turbine <- summarise_time_to_threshold(tt_dt)
  summary_tt_bands      <- summarise_time_to_threshold_bands(
    tt_dt, low_cut = shutdown_time_low_cut, high_cut = shutdown_time_high_cut
  )

  write_xlsx_local(
    list(
      Time_to_threshold = tt_dt,
      By_turbine        = summary_tt_by_turbine,
      Bands_40_50s      = summary_tt_bands
    ),
    file.path(folder_output, sprintf("curtailment_shutdown_time_%s.xlsx", report_month))
  )

  p_shutdown_time <- plot_time_to_threshold(tt_dt)
  ggsave(
    file.path(folder_output, sprintf("curtailment_shutdown_time_hist_%s.png", report_month)),
    plot = p_shutdown_time, width = 180, height = 200, units = "mm", dpi = 300, bg = "white"
  )

  ### 5.3 Safe distance (metodologia KNE)
  safe_dist_dt <- compute_safe_distance(
    curtl_scada_dt, scada_dt, track_dt,
    start_end_gap_sec = curtailment_start_end_gap_sec,
    rpm_threshold = safe_dist_rpm_threshold,
    speed_trim_q = safe_dist_speed_trim_q,
    already_slowing_rpm_threshold = safe_dist_already_slowing_rpm
  )
  summary_safe_dist <- summarise_safe_distance(safe_dist_dt, prioritysp)

  write_xlsx_local(
    list(
      Safe_distance = safe_dist_dt,
      Overall       = summary_safe_dist$overall,
      By_species    = summary_safe_dist$by_species
    ),
    file.path(folder_output, sprintf("curtailment_safe_distance_%s.xlsx", report_month))
  )

  p_safe_dist_hist <- plot_safe_distance_hist(
    safe_dist_dt, species_sel = prioritysp, ref_line_m = safe_dist_reference_line_m, facet = TRUE
  )
  ggsave(
    file.path(folder_output, sprintf("curtailment_safe_distance_hist_%s.png", report_month)),
    plot = p_safe_dist_hist, width = 8, height = 8, dpi = 300, bg = "white"
  )

} else {message("scada_dt nao disponivel ou run_sections_monthly$curtailment_response_delays = FALSE -- 5 (response/shutdown/safe distance) saltada nesta ronda.")}


##
## 6. Bi-directional ID transitions (P<->NP), global (todas as especies) ----
##
## So' o risco global de transicao P<->NP (direcao, atraso, numeros gerais) --
## NAO inclui matriz de confusao por especie (essa e' a secção 6b, abaixo,
## desligada por omissao). O metodo e' o mesmo usado na analise de remocao
## do Kestrel (R/curtailment_removal_risk.R), aqui aplicado ao conjunto
## completo de especies nao-prioritarias, nao a uma unica especie.
##

if (exists("track_dt") && exists("curtl_dt") && isTRUE(run_sections_monthly$id_transitions)) {

  source("R/id_transitions.R")

  ## reutiliza monthly_richness_dt calculado na secção 2 (mesmo track_dt);
  ## recalcula aqui de forma defensiva, caso a secção 2 tenha sido saltada
  if (!exists("monthly_richness_dt")) monthly_richness_dt <- track_species_summary(track_dt)

  monthly_id_risk_dt <- classify_id_transition_risk(
    monthly_richness_dt, track_dt, curtl_dt, prioritysp,
    late_time_threshold_sec = id_transition_late_time_sec,
    late_dist_threshold_m = track_proximity_threshold_m
  )
  monthly_id_risk_summary <- summarise_id_transition_risk(monthly_id_risk_dt, curtl_dt)
  monthly_id_late_cases_dt <- id_transition_late_cases(monthly_id_risk_dt, curtl_dt)

  write_xlsx_local(
    list(
      ID_transition_risk    = monthly_id_risk_dt,
      Risk_by_direction     = monthly_id_risk_summary$by_direction,
      Risk_PNP_curtailments = monthly_id_risk_summary$pnp_curtailments,
      Late_criteria_compare = monthly_id_risk_summary$late_criteria_compare,
      Late_cases_detail     = monthly_id_late_cases_dt
    ),
    file.path(folder_output, sprintf("id_transitions_%s.xlsx", report_month))
  )

  p_monthly_late_time <- plot_late_time_distribution(monthly_id_risk_dt, threshold_sec = id_transition_late_time_sec)
  ggsave(
    file.path(folder_output, sprintf("id_transition_late_time_dist_%s.png", report_month)),
    plot = p_monthly_late_time, width = 7, height = 4, dpi = 300, bg = "white"
  )

  p_monthly_late_dist <- plot_late_dist_distribution(monthly_id_risk_dt, threshold_m = track_proximity_threshold_m)
  ggsave(
    file.path(folder_output, sprintf("id_transition_late_dist_dist_%s.png", report_month)),
    plot = p_monthly_late_dist, width = 7, height = 4, dpi = 300, bg = "white"
  )

} else {message("track_dt/curtl_dt nao disponiveis ou run_sections_monthly$id_transitions = FALSE -- 6 (ID transitions) saltada nesta ronda.")}


##
## 6b. Species confusion matrix (opcional, DESLIGADA por omissao) ----
##
## Que outras especies aparecem no mesmo track que
## id_confusion_species_of_interest (ver inputs/monthlyReportSettings.R) --
## analise pontual para uma especie especifica (ex: Kestrel, no contexto da
## discussao de remocao do curtailment trigger), nao uma metrica mensal de
## rotina. Ligar via run_sections_monthly$id_confusion = TRUE e ajustar
## id_confusion_species_of_interest quando for preciso.
##

if (exists("track_dt") && exists("curtl_dt") && isTRUE(run_sections_monthly$id_confusion)) {

  source("R/id_transitions.R")

  if (!exists("monthly_richness_dt")) monthly_richness_dt <- track_species_summary(track_dt)

  monthly_id_confusion_summary <- summarise_species_confusion(
    track_dt, monthly_richness_dt, curtl_dt, id_confusion_species_of_interest
  )

  write_xlsx_local(
    list(
      Confusion_rate_compare = monthly_id_confusion_summary$rate_compare,
      Confusion_general      = monthly_id_confusion_summary$confusion_general,
      Confusion_curtailments = monthly_id_confusion_summary$confusion_curtailments
    ),
    file.path(folder_output, sprintf("id_confusion_%s_%s.xlsx", id_confusion_species_of_interest, report_month))
  )

} else {message("run_sections_monthly$id_confusion = FALSE (por omissao) -- 6b (species confusion matrix) saltada nesta ronda.")}


##
## 7. Biological flight metrics (velocidade/altura de voo por especie) ----
##

if (exists("track_dt") && isTRUE(run_sections_monthly$bio_flight_metrics)) {

  source("R/bio_flight_metrics.R")

  monthly_flight_base_dt <- flight_metrics_base(
    track_dt, prioritysp, min_track_points = flight_min_track_points,
    speed_ms_min = flight_speed_ms_min, speed_ms_max = flight_speed_ms_max
  )
  monthly_flight_speed_summary_dt  <- summarise_flight_speed(monthly_flight_base_dt)
  monthly_flight_height_summary_dt <- summarise_flight_height(monthly_flight_base_dt)

  write_xlsx_local(
    list(
      Flight_speed_by_species  = monthly_flight_speed_summary_dt,
      Flight_height_by_species = monthly_flight_height_summary_dt
    ),
    file.path(folder_output, sprintf("bio_flight_metrics_%s.xlsx", report_month))
  )

  n_species_flight_monthly <- length(unique(monthly_flight_base_dt$spec))
  p_monthly_flight_metrics <- plot_flight_metrics_distribution(monthly_flight_base_dt, riskHeight_lower, riskHeight_upper)
  # 8x8cm por painel (1 especie x 1 metrica) -- facet_grid(spec_abbr ~ metric)
  # tem sempre 2 colunas (speed_ms, height) e 1 linha por especie
  ggsave(
    file.path(folder_output, sprintf("bio_flight_metrics_distribution_%s.png", report_month)),
    plot = p_monthly_flight_metrics, width = 2 * 8, height = n_species_flight_monthly * 8, units = "cm", dpi = 300, bg = "white"
  )

} else {message("track_dt nao disponivel ou run_sections_monthly$bio_flight_metrics = FALSE -- 7 (bio flight metrics) saltada nesta ronda.")}


##
## 8. Minimum individuals per time bin ----
##

if (exists("track_dt") && isTRUE(run_sections_monthly$min_individuals)) {

  source("R/track_min_individuals.R")

  monthly_min_indiv_bins_dt <- count_min_individuals_per_bin(
    track_dt, species = prioritysp,
    bin_min = min_individuals_bin_min, merge_dist_m = min_individuals_merge_dist_m
  )
  monthly_min_indiv_summary_dt <- summarise_min_individuals(monthly_min_indiv_bins_dt)
  monthly_min_indiv_daily_dt   <- summarise_daily_max_individuals(monthly_min_indiv_bins_dt)

  write_xlsx_local(
    list(
      Min_individuals_bins    = monthly_min_indiv_bins_dt,
      Min_individuals_summary = monthly_min_indiv_summary_dt,
      Min_individuals_daily   = monthly_min_indiv_daily_dt
    ),
    file.path(folder_output, sprintf("min_individuals_per_bin_%s.xlsx", report_month))
  )

  n_species_min_indiv_monthly <- length(unique(monthly_min_indiv_bins_dt$spec))

  # species_sel = prioritysp (nao o default da funcao, so' 2 especies) --
  # mostrar todas as especies prioritarias, 1 painel por especie
  p_monthly_min_indiv_daily <- plot_daily_max_individuals(monthly_min_indiv_daily_dt, species_sel = prioritysp)
  # 15cm de largura (facet_wrap ncol=1 -- so' 1 coluna) x 8cm de altura por
  # especie/painel
  ggsave(
    file.path(folder_output, sprintf("min_individuals_daily_max_%s.png", report_month)),
    plot = p_monthly_min_indiv_daily, width = 15, height = n_species_min_indiv_monthly * 8, units = "cm", dpi = 300, bg = "white"
  )

} else {message("track_dt nao disponivel ou run_sections_monthly$min_individuals = FALSE -- 8 (min individuals) saltada nesta ronda.")}


##
## Export monthly report (Word, via Rmd) ----
##

source("R/report.R")

monthly_report_params <- list(
  title         = report_title_monthly,
  project_ref   = project_ref,
  report_month  = report_month,
  report_start  = as.character(report_start),
  report_end    = as.character(report_end),
  analysis_date = format(Sys.time(), "%Y-%m-%d"),
  username      = username,

  data_summary = if (exists("monthly_data_summary_dt")) monthly_data_summary_dt else NULL,

  availability_by_idf   = if (exists("idf_availability_summary")) idf_availability_summary$by_idf else NULL,
  availability_plot_cal = if (exists("p_availability_cal")) p_availability_cal else NULL,
  availability_plot_freq = if (exists("p_availability_freq")) p_availability_freq else NULL,
  availability_plot_spatial = if (exists("p_availability_spatial")) p_availability_spatial else NULL,

  richness_dt         = if (exists("monthly_richness_summary")) monthly_richness_summary$by_n_species else NULL,
  richness_plot        = if (exists("p_monthly_richness")) p_monthly_richness else NULL,

  curtl_by_species_dt = if (exists("monthly_species_curt_by_species_dt")) monthly_species_curt_by_species_dt else NULL,
  curtl_by_group_dt   = if (exists("monthly_species_curt_by_group_dt")) monthly_species_curt_by_group_dt else NULL,

  short_track_summary_dt = if (exists("monthly_short_track_summary_dt")) monthly_short_track_summary_dt else NULL,

  assess_by_status  = if (exists("summary_assess")) summary_assess$by_status else NULL,
  assess_by_turbine = if (exists("summary_assess")) summary_assess$by_turbine else NULL,

  shutdown_by_turbine = if (exists("summary_tt_by_turbine")) summary_tt_by_turbine else NULL,
  shutdown_bands      = if (exists("summary_tt_bands")) summary_tt_bands else NULL,
  shutdown_plot       = if (exists("p_shutdown_time")) p_shutdown_time else NULL,

  safe_dist_overall     = if (exists("summary_safe_dist")) summary_safe_dist$overall else NULL,
  safe_dist_by_species  = if (exists("summary_safe_dist")) summary_safe_dist$by_species else NULL,
  safe_dist_plot        = if (exists("p_safe_dist_hist")) p_safe_dist_hist else NULL,

  id_risk_by_direction = if (exists("monthly_id_risk_summary")) monthly_id_risk_summary$by_direction else NULL,
  id_confusion_species = if (exists("monthly_id_confusion_summary")) id_confusion_species_of_interest else NULL,
  id_confusion_general = if (exists("monthly_id_confusion_summary")) monthly_id_confusion_summary$confusion_general else NULL,

  flight_speed_by_species  = if (exists("monthly_flight_speed_summary_dt")) monthly_flight_speed_summary_dt else NULL,
  flight_height_by_species = if (exists("monthly_flight_height_summary_dt")) monthly_flight_height_summary_dt else NULL,
  flight_metrics_plot      = if (exists("p_monthly_flight_metrics")) p_monthly_flight_metrics else NULL,

  min_indiv_summary = if (exists("monthly_min_indiv_summary_dt")) monthly_min_indiv_summary_dt else NULL,
  min_indiv_daily_plot = if (exists("p_monthly_min_indiv_daily")) p_monthly_min_indiv_daily else NULL,

  ## Literais de configuracao (monthlyReportSettings.R) -- so' para texto
  ## descritivo no Rmd (ver report/monthly_report_template.rmd), NAO
  ## controlam nenhum calculo aqui. Sempre definidos independentemente dos
  ## switches de run_sections_monthly (sao literais de settings, nao
  ## objetos calculados).
  heartbeat_interval_min    = heartbeat_interval_min,
  heartbeat_offline_gap_min = heartbeat_offline_gap_min,
  idf_availability_top_n    = idf_availability_top_n,

  shorttrack_min_points   = shorttrack_min_points,
  shorttrack_eval_range_m = shorttrack_eval_range,

  curtailment_start_end_gap_sec  = curtailment_start_end_gap_sec,
  curtailment_max_next_gap_sec   = curtailment_max_next_gap_sec,
  curtailment_drop_pct_threshold = curtailment_drop_pct_threshold,
  safe_shutdown_rpm               = safe_shutdown_rpm,

  shutdown_time_low_cut  = shutdown_time_low_cut,
  shutdown_time_high_cut = shutdown_time_high_cut,

  safe_dist_reference_line_m    = safe_dist_reference_line_m,
  safe_dist_rpm_threshold        = safe_dist_rpm_threshold,
  safe_dist_already_slowing_rpm  = safe_dist_already_slowing_rpm,

  id_transition_late_time_sec = id_transition_late_time_sec,
  track_proximity_threshold_m = track_proximity_threshold_m,

  flight_min_track_points = flight_min_track_points,
  flight_speed_ms_min     = flight_speed_ms_min,
  flight_speed_ms_max     = flight_speed_ms_max,
  riskHeight_lower         = riskHeight_lower,
  riskHeight_upper         = riskHeight_upper,

  min_individuals_bin_min       = min_individuals_bin_min,
  min_individuals_merge_dist_m  = min_individuals_merge_dist_m
)

build_idf_report(
  output_file   = file.path(folder_output, sprintf("IDF_monthly_report_%s.docx", report_month)),
  report_params = monthly_report_params,
  template      = "report/monthly_report_template.rmd"
)
