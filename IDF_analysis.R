## ----------- HEADER -----------
##**********************************************
##
##   Identiflight (IDF) data analysis
##
##**********************************************
##
## Author: Paulo Cardoso
## Date: 2024
##
## Updates: 
##    - v0.2024-mm-dd: (Paulo Cardoso) Criação dos scripts de analise
##    - v1.2026-02-23: (Sandra) Passado para template e estrutura atual dos scripts IDI; retirado todo hard coding; etc
##    - v2.2026-02-26: (Sandra) Ajustes nos outputs; criada pasta com outputs de data quality/data control (DQ/DC)
##
##
## Property of Bioinsight, Lda. 
## Reproduction or sharing of any part of this 
## script is prohibited without explicit permission.
##
##
## R version to use:
## v R4.3.3 (tested and working)
##
##**********************************************


#coment this line for debug!!!
options(error = function() message("Skipping failed step"))

## 
## PACKAGES ----
## 


##Needed packages
packages <- c('purrr','rstudioapi', #purrr needed for citation; rstudioapi needed for dynamic atribution of working directory with getActiveDocumentContext()
              'tidyverse', 'lubridate', 'hms', 'ggplot2',
              'scales', 'readxl', 'janitor', 'sf', 'geosphere',
              'gt', 'skimr', 'vtable', 'data.table', 'htmlwidgets',
              'ggTimeSeries', 'suncalc', #,'patchwork','arrow'
              'openxlsx','writexl','rmarkdown','flextable','systemfonts',
              'terra', 'RANN', 'plotly') #terra/RANN/plotly: coverage 3D com topografia (DEM)

##Check and install packages that are missing + call library()
for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE) }

#OR

##Use project library saved using renv lock.file
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set wd first
# renv::restore() #make sure project has renv folder and lock.file


##View installed package versions (it will be saved later in a txt file in the output folder)
# sessionInfo(package=NULL) 



## 
## SETTINGS ----
## 


##SET INPUT/OUTPUT FOLDERS##
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Automatically set wd to the folder where the script is located
folder_input <- "inputs"
folder_output <- file.path("outputs",format(Sys.time(), "%Y%m%d"))
folder_output_DC <- file.path("outputs",format(Sys.time(), "%Y%m%d"),"DC")

dir.create(folder_output, showWarnings = FALSE, recursive = TRUE)
dir.create(folder_input, showWarnings = FALSE, recursive = TRUE)
dir.create(folder_output_DC, showWarnings = FALSE, recursive = TRUE)

##Import scripts
folder_script <- "scripts\\"
Rfiles <- list.files(folder_script, pattern = '.R', full.names = T)
lapply(Rfiles, function(x) source(x))

#scripts especificos do IDF (modulos de analise ainda nao migrados para R/)
folder_script_IDF <- "scripts_IDF"


##USER SETTINGS##
#Alterar para o ficheiro de settings do projeto a analisar (ex: "userSettings_BSH.R", "userSettings_DGY.R")
project_settings_file <- "userSettings_BSH.R"
source(file.path(folder_input, project_settings_file)) #Import user defined settings #(e.g. model parameters, etc)
#databases_dir <- file.path("..") #get files from dir that is one level up
#folder_subsample <- file.path(databases_dir,"subsample_last_tracksonly")


## 
## QUALITY STANDARDS ----
## 


#Get script and packages version -> script folder must contain "_v"+ script version
script_version <- BBmisc::explode(dirname(rstudioapi::getActiveDocumentContext()$path), sep = "/")
script_version <- tail(script_version, 1)

#Get username
username <- check_username()

#Save R version and package versions for reproducibility
quality_standards(username, folder_output, packages)

#Save username, analysis date and script version
sink(file.path(folder_output,"R_analysis_info.txt"))
cat(paste0('Analysis technician: ', username, '\n'))
cat(paste0('Analysis date: ', format(Sys.time(), "%Y-%m-%d"), '\n'))
cat(paste0('Script version: ', script_version, '\n'))
sink()



## 
## ANALYSIS ----
##

##  
## 0. Import data ----
##

#WTG
wtg <- sf::read_sf(file.path(folder_input, wtg_filename))

#IDF
idf <- sf::read_sf(file.path(folder_input, idf_filename))

#Tier scheme
tier <- read_xlsx(file.path(folder_input, tier_start_scheme_filename))


#Tier3 scheme - Starting date
tier3 <- read_xlsx(file.path(folder_input, tier3_start_scheme_filename),
                   sheet = 'tier3')


#Databases
source("R/read_utils.R")
source("R/read_tracks.R")
source("R/read_curtailments.R")
source("R/read_scada.R")
source("R/read_heartbeats.R")

# databases_dir (local) + databases_dir_alt (servidor), quando definido --
# procura ficheiros em ambos, sem duplicar; ordem = precedencia quando o
# mesmo nome de ficheiro existe nos dois (ver R/read_utils.R)
databases_dirs <- unique(c(databases_dir, if (exists("databases_dir_alt")) databases_dir_alt))

# As 4 bases de dados sao lidas em UTC na origem e convertidas aqui para a
# hora local do projeto (proj_timezone) -- so muda a exibicao/agrupamento
# por dia calendario, nao os instantes usados nos roll joins (curtailment_response.R)
track_dt_unfilt  <- read_tracks_data(databases_dirs, trackreport_pattern, tz = proj_timezone)   # Tracks data
curtl_dt_unfilt  <- read_curtailments_data(databases_dirs, curtailments_pattern, tz = proj_timezone) # Curtailments data
scada_dt_unfilt  <- read_scada_data(databases_dirs, scada_pattern, tz = proj_timezone)          # SCADA data
heartb_dt_unfilt <- read_heartbeats_data(databases_dirs, heartbeats_pattern, tz = proj_timezone) # Heartbeat data

#Project-specific corrections --> handle_script.R
#if(file.exists('handle_script.R')) {source('handle_script.R')}


tier_dt <- setDT(tier)
tier3_dt <- setDT(tier3)
tier3_dt[, idate := as.IDate(timestamp)]
set_tier3_dt <- tier3_dt[idate < date(end)]

#check if turbines labels match across all datasets
Reduce(setdiff, list(
  unique(wtg$InternalNa),
  unique(scada_dt_unfilt$turbinelabel),
  unique(curtl_dt_unfilt$turbine),
  unique(track_dt_unfilt$turbine)
))

#verificar qual falta
# collect all turbine labels
turbines <- sort(unique(c(
  wtg$InternalNa,
  scada_dt_unfilt$turbinelabel,
  curtl_dt_unfilt$turbine,
  track_dt_unfilt$turbine
)))

# build presence table
labels_table <- data.frame(
  turbine = turbines,
  wtg   = as.integer(turbines %in% wtg$InternalNa),
  curtl = as.integer(turbines %in% curtl_dt_unfilt$turbine),
  track = as.integer(turbines %in% track_dt_unfilt$turbine),
  scada = as.integer(turbines %in% scada_dt_unfilt$turbinelabel)
)
View(labels_table) #ver se nomes turbinas estao corretos entre todas as bases de dados


safe_date <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return(as.Date(x))
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "-", "NULL")] <- NA
  suppressWarnings(as.Date(x))
}


#Colocar em projecao planar
idf <- st_transform(idf, crs_projection_plannar)
wtg <- st_transform(wtg, crs_projection_plannar)

##
## 0. Data Quality/Data Control (DQ/DC) ----
##

###
### Check data gaps - legado, ainda nao migrado para R/
###

#Check for data gaps - Graphs
#source(file.path(folder_script_IDF, 'DQ_check_data_gaps_graphs.R'))

#Check for data gaps - excel file with data gaps and range
#source(file.path(folder_script_IDF, 'DQ_check_data_gaps_excel.R'))


###
### Data coverage: Curtailments vs SCADA (por turbina) e Heartbeats (por unidade IDF)
###
### NOTA: usa dados NAO filtrados (_unfilt) para ver o historico completo
### descarregado, antes de recortarmos ao periodo de reporte [ini, end].
### Turbinas e unidades IDF sao entidades diferentes (uma turbina pode
### ser protegida por varias unidades IDF) -- por isso sao duas tabelas
### e dois plots independentes, sem tentar cruzar os dois eixos.
###

source("R/data_coverage.R")

## Turbinas: sobreposicao Curtailments vs SCADA
if (!is.null(scada_dt_unfilt) && nrow(curtl_dt_unfilt) > 0) {
  
  coverage_turbine_dt <- daily_overlap_by_turbine(
    curtl_dt_unfilt, "start", "turbine", "Curtailments",
    scada_dt_unfilt, "datetime", "turbinelabel", "SCADA"
  )
  coverage_turbine_summary <- overlap_summary_by_turbine(coverage_turbine_dt, "Curtailments", "SCADA")
  
  p_coverage_turbine <- plot_daily_overlap_by_turbine(
    coverage_turbine_dt, "Curtailments", "SCADA", turbine_sel = turbinas_scada
  )
  ggsave(
    file.path(folder_output, "data_coverage_turbine_curtailments_scada.png"),
    plot = p_coverage_turbine, width = 250, height = max(60, length(turbinas_scada) * 40),
    units = "mm", dpi = 300, bg = "white", limitsize = FALSE
  )
  
  writexl::write_xlsx(
    list(Overlap = coverage_turbine_dt, Summary = coverage_turbine_summary),
    file.path(folder_output, "data_coverage_turbine_curtailments_scada.xlsx")
  )
  
} else {print("SCADA or Curtailments data not available - Turbine data coverage check was skipped")}

## Unidades IDF: cobertura de Heartbeats (entidade diferente de turbina, tratada em separado)
if (!is.null(heartb_dt_unfilt) && nrow(heartb_dt_unfilt) > 0) {
  
  coverage_idf_dt      <- daily_presence_by_idf(heartb_dt_unfilt, "timestamp", "idf", "Heartbeats")
  coverage_idf_summary <- presence_summary_by_idf(coverage_idf_dt)
  
  p_coverage_idf <- plot_daily_presence_by_idf(coverage_idf_dt)
  ggsave(
    file.path(folder_output, "data_coverage_idf_heartbeats.png"),
    plot = p_coverage_idf, width = 250, height = max(60, uniqueN(coverage_idf_dt$idf) * 6),
    units = "mm", dpi = 300, bg = "white", limitsize = FALSE
  )
  
  writexl::write_xlsx(
    list(Presence = coverage_idf_dt, Summary = coverage_idf_summary),
    file.path(folder_output, "data_coverage_idf_heartbeats.xlsx")
  )
  
} else {print("Heartbeat data not available - IDF data coverage check was skipped")}

## Plots
p_coverage_turbine
p_coverage_idf

## Printed Summaries for all 4 main datasets ---
unique(heartb_dt_unfilt$idf)

coverage_turbine_dt[turbine == "BSH54", .(
  n_days      = .N,
  n_curtl     = sum(Curtailments),
  n_scada     = sum(SCADA),
  n_overlap   = sum(overlap)
), by = .(ym = format(date, "%Y-%m"))]

##
## 0. Filter data ----
## 

###
### Filter data for temporal range
###

#NOTA: 
#ini --> definido no userSettings.txt
#end --> definido no userSettings.txt

report_start <- as.Date(ini)
report_end   <- as.Date(end)

#SCADA data
scada_dt <- scada_dt_unfilt #NAO FILTRAR
#check 
scada_dt[, .(
  min_start = min(datetime , na.rm = TRUE),
  max_start = max(datetime , na.rm = TRUE)
)]

#Curtail data
curtl_dt <- as.data.table(curtl_dt_unfilt)[
  start >= ini & start <= end
]
#check 
curtl_dt[, .(
  min_start = min(start, na.rm = TRUE),
  max_start = max(start, na.rm = TRUE)
)]

#Track data
track_dt <- track_dt_unfilt %>%
  filter(timestamp >= ini & timestamp <= end)
#check
track_dt[, .(
  min_datetime = min(timestamp, na.rm = TRUE),
  max_datetime = max(timestamp, na.rm = TRUE)
)]

#Heartbeat data
heartb_dt <- heartb_dt_unfilt %>%
  filter(idf %in% heartbeat_idf_units) %>%
  filter(timestamp >= ini & timestamp <= end)
#check
heartb_dt[, .(
  min_datetime = min(timestamp, na.rm = TRUE),
  max_datetime = max(timestamp, na.rm = TRUE)
)]
unique(heartb_dt$idf)

###
### Tier 3 data only
###

tier3_track_dt <-
  track_dt[set_tier3_dt, 
           on = .(turbine), 
           nomatch = 0][
             timestamp > i.timestamp, # filter tier 3 only
             .SD, 
             .SDcols = names(track_dt)]



##
## 0. Other ----
##
## DESATIVADO por agora - depende de scripts_IDF/, ainda nao migrado para R/

#create tier calendar
#source(file.path(folder_script_IDF, 'wtg_tier_starting_date_calendar.R'))


#Create tier 3 only calendar
#source(file.path(folder_script_IDF, 'wtg_tier3_starting_date_calendar.R'))


#General metrics
#source(file.path(folder_script_IDF, 'general_metrics.R'))

#Total curtailments & spatial distribution
#source(file.path(folder_script_IDF, 'curtailments_spatial.R'))



##  
## 1. Performance ----
##

### 1.2. False positive/False negative rates and other
# 1. É preciso fazer uma subamostragem de 10% dos frames --> ver IDF_script_subsample 
# 2. colocar pasta subsample na RAIZ
# 3. Validar subsample por ortinologo --> ornitologo tem de preencher "subsample_valid.xlsx"
# 4. Depois entao fazer esta analise:

## DESATIVADO por agora - depende de scripts_IDF/, ainda nao migrado para R/
if (exists("folder_subsample") && dir.exists(folder_subsample)) { #Apenas correr se existir folder da subsample
  #source(file.path(folder_script_IDF, 'ID_confusion_matrix.R')) ##FALTA DESENVOLVER
} else {print("Subsample folder does not exist - Confusion matrix analysis was skipped")}

### 1.2. ID transitions (stability in ID)
#source(file.path(folder_script_IDF, 'ID_transitions.R'))



##  
## 2. Efficacy ----
##   

# Efficacy was evaluated empirically, using data obtained from PCFM carcass searches.
#source(file.path(folder_script_IDF, 'mortality_efficacy.R')) 
###FALTA DESENVOLVER ----



##  
## 3. Effectiveness ----
##  



### 3.1. System Availability ----

# Obtained by IDF Team. Agreed that would be calculated for tier 3 WTG only

# IDF Availability (validacao independente para identificar "Down periods" - Apenas corre se tivermos dados de heartbeat)
if (exists("heartb_dt")) {
  
  source("R/availability_daylight.R")
  
  daylight_cal <- build_daylight_calendar(ini, end, proj_lat, proj_lon, proj_timezone)
  
  idf_availability_dt <- daylight_availability(
    heartb_dt, daylight_cal, proj_timezone,
    offline_gap_min  = heartbeat_offline_gap_min,
    online_grace_min = heartbeat_interval_min
  )
  
  idf_availability_summary <- summarise_availability(idf_availability_dt)
  
  # writexl::write_xlsx(idf_availability_summary$by_idf,
  #                     file.path(folder_output, "idf_availability_summary_by_idf.xlsx"))
  # writexl::write_xlsx(idf_availability_summary$by_month,
  #                     file.path(folder_output, "idf_availability_summary_by_month.xlsx"))
  
  # Unidades com mais tempo offline - usado nos 2 graficos abaixo, para
  # manterem as mesmas unidades e ficarem legiveis
  #idf_availability_top_n --> definido no userSettings_BSH.R
  idf_sel <- idf_availability_summary$by_idf[
    order(-offline_mins_total)][seq_len(min(idf_availability_top_n, .N)), idf]
  
  p_availability_cal <- plot_availability_calendar(
    idf_availability_dt, idf_availability_summary$by_idf,
    idf_sel = idf_sel, top_n = idf_availability_top_n
  )
  ggsave(
    file.path(folder_output, paste0("idf_availability_calendar_", report_start, "to", report_end, ".png")),
    plot = p_availability_cal, width = 200, height = 90, units = "mm", dpi = 300, bg = "white"
  )
  
  p_availability_freq <- plot_availability_frequency(idf_availability_summary$by_idf)
  ggsave(
    file.path(folder_output, paste0("idf_availability_frequency_", report_start, "to", report_end, ".png")),
    plot = p_availability_freq, width = 6, height = 3, units = "in", dpi = 300, bg = "white"
  )
  
  # Grelha de heartbeats (dia/noite, presente/em falta) por slot de heartbeat_interval_min,
  # para as mesmas unidades com mais tempo offline
  heartbeat_slots_dt <- heartbeat_slot_grid(
    heartb_dt, daylight_cal, proj_timezone,
    start_date = report_start, end_date = report_end,
    idf_sel = idf_sel, slot_mins = heartbeat_interval_min
  )
  
  n_report_days <- as.numeric(report_end - report_start) + 1
  slot_date_breaks <- if (n_report_days <= 31) "2 days" else if (n_report_days <= 92) "1 week" else "1 month"
  
  p_heartbeat_slots <- plot_heartbeat_slots(heartbeat_slots_dt, date_breaks = slot_date_breaks)
  ggsave(
    file.path(folder_output, paste0("idf_heartbeat_slots_", report_start, "to", report_end, ".png")),
    plot = p_heartbeat_slots,
    width = max(150, n_report_days * 3), height = max(60, length(idf_sel) * 40),
    units = "mm", dpi = 300, bg = "white", limitsize = FALSE
  )
  
} else {print("Heartbeat data not available - IDF availability analysis was skipped")}


# System availability - data sent by IDF team
#source(file.path(folder_script_IDF, 'WTG_protect_time.R')) 
# TEM HARD CODE #



## Secções 3.2 a 5 (abaixo) e as exportações finais estão DESATIVADAS por agora -
## dependem de scripts_IDF/, ainda nao migrado para R/. Vamos reativando
## bloco a bloco à medida que cada módulo for trazido para R/.

### 3.2. Curtailments due to ID transitions ----
#source(file.path(folder_script_IDF, 'curtailments_ID_transitions.R'))



### 3.3. Species-specific curtailment ----
#source(file.path(folder_script_IDF, 'curtailments_species.R'))



### 3.4. Short-track curtailment ----

# shorttrack_min_points  --> definido no userSettings.txt
# shorttrack_eval_range  --> definido no userSettings.txt
#source(file.path(folder_script_IDF, 'curtailments_short-track.R'))



### 3.5. Curtailment validation metrics ----


# NOTA:
# Analise sera feita apenas para range temporal e turbinas que tiverem dados de SCADA

#scada_ini  --> definido no userSettings_BSH.R
#scada_end  --> definido no userSettings_BSH.R
#turbinas_scada --> definido no userSettings_BSH.R
#safe_shutdown_rpm --> definido no userSettings_BSH.R
#curtailment_start_end_gap_sec, curtailment_max_next_gap_sec, curtailment_drop_pct_threshold --> definidos no userSettings_BSH.R
#curtailment_window_sec, curtailment_window_max_gap_sec --> definidos no userSettings_BSH.R

# os três sub-resumos em summary_assess cobrem exatamente o mesmo universo, 
# só que cada um agrega por um critério diferente. Vale a pena somar para confirmar:
#   
# Os três totais batem certo — não há inconsistência, só parece diferente porque as categorias são outras.
# 
# O universo comum: curtl_scada_dt
# Todos os três vêm de assess_dt, que por sua vez vem de 
# assess_curtailment_response(curtl_scada_dt, scada_dt, ...). 
# E curtl_scada_dt é definido assim, na secção 3.5:
#   
# curtl_scada_dt <- curtl_dt[turbine %in% turbinas_scada & start >= scada_ini & start <= scada_end]
# Ou seja: todos os curtailments (sucesso, falha, ou sem dados — todos) das turbinas em turbinas_scada, 
# dentro da janela [scada_ini, scada_end]. Cada curtailment aparece nos três sumários, 
# só agrupado de forma diferente:
#   
# $by_status — agrupa esses curtailments totais pelo final_status (o resultado global: parou / não parou / já estava parada / sem dados).
# $by_immediate — agrupa os mesmos totais pelo no_immediate_response (só a verificação da leitura seguinte ao sinal — um critério diferente, independente do final_status).
# $by_turbine — agrupa os mesmos totais por turbina, cruzando os dois critérios numa linha por turbina.
# 
if (exists("scada_dt")) { #Apenas corre se tiver dados de SCADA
  
  source("R/curtailment_response.R")
  
  curtl_scada_dt <- curtl_dt[
    turbine %in% turbinas_scada & start >= scada_ini & start <= scada_end
  ]
  
  ###... B.1 Avaliacao principal: baseline apertado + resposta imediata + delta start->end ----
  assess_dt <- assess_curtailment_response(
    curtl_scada_dt, scada_dt,
    start_end_gap_sec = curtailment_start_end_gap_sec, max_next_gap_sec = curtailment_max_next_gap_sec,
    drop_pct_threshold = curtailment_drop_pct_threshold, rpm_threshold = safe_shutdown_rpm
  )
  summary_assess <- summarise_curtailment_assessment(assess_dt)
  
  writexl::write_xlsx(
    list(
      Assessment   = assess_dt,
      By_status    = summary_assess$by_status,
      By_immediate = summary_assess$by_immediate,
      By_turbine   = summary_assess$by_turbine
    ),
    file.path(folder_output, paste0("curtailment_response_assessment_", date(scada_ini), "to", date(scada_end), ".xlsx"))
  )
  
  ###... B.2 Vista complementar: janela larga de 90s, tolerancia mais permissiva (15s) ----
  response_dt <- classify_curtailment_response(
    curtl_scada_dt, scada_dt,
    monitor_window_sec = curtailment_window_sec, rpm_threshold = safe_shutdown_rpm,
    max_gap_sec = curtailment_window_max_gap_sec
  )
  summary_response <- summarise_curtailment_response(response_dt)
  
  writexl::write_xlsx(
    list(
      Window_response = response_dt,
      By_status       = summary_response$by_status,
      By_turbine      = summary_response$by_turbine
    ),
    file.path(folder_output, paste0("curtailment_response_window_", date(scada_ini), "to", date(scada_end), ".xlsx"))
  )
  
  ### 3.6. Tempo ate atingir limiares de RPM (2, 1, 0), por curtailment ----
  
  #shutdown_time_thresholds, shutdown_time_low_cut, shutdown_time_high_cut --> definidos no userSettings_BSH.R
  
  source("R/curtailment_shutdown_time.R")
  
  tt_dt <- time_to_rpm_thresholds(
    curtl_scada_dt, scada_dt, thresholds = shutdown_time_thresholds,
    start_end_gap_sec = curtailment_start_end_gap_sec
  )
  summary_tt_by_turbine <- summarise_time_to_threshold(tt_dt)
  summary_tt_bands      <- summarise_time_to_threshold_bands(
    tt_dt, low_cut = shutdown_time_low_cut, high_cut = shutdown_time_high_cut
  )
  
  writexl::write_xlsx(
    list(
      Time_to_threshold = tt_dt,
      By_turbine        = summary_tt_by_turbine,
      Bands_40_50s      = summary_tt_bands
    ),
    file.path(folder_output, paste0("curtailment_shutdown_time_", date(scada_ini), "to", date(scada_end), ".xlsx"))
  )
  
  p_shutdown_time <- plot_time_to_threshold(tt_dt)
  ggsave(
    file.path(folder_output, paste0("curtailment_shutdown_time_hist_", date(scada_ini), "to", date(scada_end), ".png")),
    plot = p_shutdown_time, width = 180, height = 200, units = "mm", dpi = 300, bg = "white"
  )

  ### 3.7. Safe distance (metodologia KNE) ----

  #safe_dist_rpm_threshold, safe_dist_speed_trim_q, safe_dist_reference_line_m,
  #safe_dist_already_slowing_rpm --> definidos no userSettings_BSH.R

  source("R/curtailment_safe_distance.R")

  safe_dist_dt <- compute_safe_distance(
    curtl_scada_dt, scada_dt, track_dt,
    start_end_gap_sec = curtailment_start_end_gap_sec,
    rpm_threshold = safe_dist_rpm_threshold,
    speed_trim_q = safe_dist_speed_trim_q,
    already_slowing_rpm_threshold = safe_dist_already_slowing_rpm
  )
  summary_safe_dist <- summarise_safe_distance(safe_dist_dt, prioritysp)

  # cenario mais gravoso (turbina a velocidade normal no disparo) vs cenario
  # de beneficio (turbina ja a abrandar de outro curtailment) -- ver
  # documentacao de turbine_state em R/curtailment_safe_distance.R
  summary_safe_dist_full_speed <- summarise_safe_distance(
    safe_dist_dt[turbine_state == "full_speed"], prioritysp
  )
  summary_safe_dist_already_slowing <- summarise_safe_distance(
    safe_dist_dt[turbine_state == "already_slowing"], prioritysp
  )

  writexl::write_xlsx(
    list(
      Safe_distance                = safe_dist_dt,
      Overall                      = summary_safe_dist$overall,
      By_species                   = summary_safe_dist$by_species,
      Overall_full_speed           = summary_safe_dist_full_speed$overall,
      By_species_full_speed        = summary_safe_dist_full_speed$by_species,
      Overall_already_slowing      = summary_safe_dist_already_slowing$overall,
      By_species_already_slowing   = summary_safe_dist_already_slowing$by_species
    ),
    file.path(folder_output, paste0("curtailment_safe_distance_", date(scada_ini), "to", date(scada_end), ".xlsx"))
  )

  p_safe_dist_hist <- plot_safe_distance_hist(
    safe_dist_dt, species_sel = prioritysp, ref_line_m = safe_dist_reference_line_m, facet = TRUE
  )
  ggsave(
    file.path(folder_output, paste0("curtailment_safe_distance_hist_", date(scada_ini), "to", date(scada_end), ".png")),
    plot = p_safe_dist_hist, width = 8, height = 8, dpi = 300, bg = "white"
  )

  p_trigger_dist_status <- plot_trigger_distance_status(
    safe_dist_dt, species_sel = prioritysp, ref_line_m = safe_dist_reference_line_m, facet = TRUE
  )
  ggsave(
    file.path(folder_output, paste0("curtailment_trigger_distance_", date(scada_ini), "to", date(scada_end), ".png")),
    plot = p_trigger_dist_status, width = 8, height = 8, dpi = 300, bg = "white"
  )

} else {print("SCADA data not available - Curtailment response assessment was skipped")}


##
## 4. Coverage ----
##


### 4.1. WF coverage ----
#source(file.path(folder_script_IDF, 'coverage_analysis_WF.R'))


### 4.2. WTG coverage 3D (topografia + deteções de aves) ----

# dem_filename, wtg_3d_coverage, coverage_min_sample_records --> definidos no userSettings_BSH.R (coloca o .tif em databases_dir)
dem_file <- file.path(databases_dir, dem_filename)

# confirmar que os nomes de wtg_3d_coverage existem mesmo no shapefile wtg
# (coluna InternalNa) antes de correr a analise -- nomes trocados ficam
# silenciosamente de fora do cov_all se nao verificarmos isto
# (wtg_3d_coverage = "all" corre para todas as turbinas do shapefile, sem check de nomes)
if (identical(wtg_3d_coverage, "all")) {
  cat(sprintf("wtg_3d_coverage = \"all\" -- vao ser analisadas todas as %d turbinas do shapefile wtg.\n", nrow(wtg)))
} else {
  cat("Turbinas em wtg_3d_coverage NAO encontradas no shapefile wtg:\n")
  print(setdiff(wtg_3d_coverage, wtg$InternalNa))
}

if (file.exists(dem_file)) {

  source("R/coverage_3d_topography.R")
  # usa a totalidade dos dados de track sem filtros
  cov_all <- run_coverage_3d_all_turbines(
    wtg, track_dt_unfilt, dem_file,
    radius = coverage_cylinder_wider_radius, cyl_height = coverage_cylinder_height,
    step_xy = coverage_mesh_step_xy, step_z = coverage_mesh_step_z,
    prox_thresh_m = coverage_prox_thresh_m,
    wtg_sel = wtg_3d_coverage,
    min_sample_records = coverage_min_sample_records
  )

  summary_cov <- summarise_mesh_coverage(lapply(cov_all, `[[`, "coverage"))

  writexl::write_xlsx(
    list(By_turbine = summary_cov$by_turbine, By_turbine_risk_band = summary_cov$by_turbine_risk_band),
    file.path(folder_output, "coverage_3d_summary.xlsx")
  )

  # Plots individuais por turbina, em HTML (cobertura + o inverso: pontos
  # da malha "air" SEM deteções de aves dentro de prox_thresh_m)
  save_coverage_3d_plots(
    cov_all, file.path(folder_output, "coverage_3d"),
    radius = coverage_cylinder_wider_radius, cyl_height = coverage_cylinder_height
  )

  # Para ver o plot interativo de UMA turbina no Viewer do RStudio, mesmo que
  # cov_all tenha varias -- correr manualmente, ex:
  # plot_coverage_3d_for_turbine(cov_all, "BSH54", coverage_cylinder_wider_radius, coverage_cylinder_height)
  # plot_coverage_3d_for_turbine(cov_all, "BSH54", coverage_cylinder_wider_radius, coverage_cylinder_height, not_covered = TRUE)

} else {print("DEM file not available - 3D coverage analysis was skipped")}

plot_coverage_3d_for_turbine(cov_all, "BSH54", coverage_cylinder_wider_radius, coverage_cylinder_height)


##
## 5. Biological (supporting info) ----
##


### 5.1. Flight speed per species ----
#source(file.path(folder_script_IDF, 'bio_flight_speed.R'))


### 5.2. Flight height per species ----
#source(file.path(folder_script_IDF, 'bio_flight_height.R'))

###Plot flight height and speed per species
#source(file.path(folder_script_IDF, 'bio_distrib_flight_height_speed_per_species.R'))



### 5.3. Risk per species ----

#NOTA:
# riskHeight_lower --> definido no userSettings.txt
# riskHeight_upper --> definido no userSettings.txt
#source(file.path(folder_script_IDF, 'bio_risk_per_species.R'))


##
## Export various metrics in a single excel file  ----
## DESATIVADO - depende dos objetos criados pelas seccoes 3.2-5 acima
##

# writexl::write_xlsx(
#   list(
#     Crtl_prior = curtails_priority,
#     Crtl_nprio = curtails_nonpriority,
#     Crtl_other = curtails_others,
#     Ntrk_prior = summary_priority,
#     Ntrk_nprio = summary_nonpriority,
#     Ntrk_other = summary_other,
#     speed = mspeed,
#     height = fheight,
#     crtl_wtg = curtails_per_wtg
#   ),
#   file.path(folder_output, paste0("Tracks&Curtls_summary.xlsx"))
# )


##
## Export report  ----
##

source("R/report.R")

report_params <- list(
  title         = paste("IDF Analysis Report -", project_ref),
  project_ref   = project_ref,
  report_start  = as.character(report_start),
  report_end    = as.character(report_end),
  analysis_date = format(Sys.time(), "%Y-%m-%d"),
  username      = username,

  availability_by_idf    = if (exists("idf_availability_summary")) idf_availability_summary$by_idf else NULL,
  availability_plot_cal  = if (exists("p_availability_cal")) p_availability_cal else NULL,
  availability_plot_freq = if (exists("p_availability_freq")) p_availability_freq else NULL,

  coverage_turbine_summary = if (exists("coverage_turbine_summary")) coverage_turbine_summary else NULL,
  coverage_idf_summary     = if (exists("coverage_idf_summary")) coverage_idf_summary else NULL,

  assess_by_status  = if (exists("summary_assess")) summary_assess$by_status else NULL,
  assess_by_turbine = if (exists("summary_assess")) summary_assess$by_turbine else NULL,

  shutdown_by_turbine = if (exists("summary_tt_by_turbine")) summary_tt_by_turbine else NULL,
  shutdown_bands      = if (exists("summary_tt_bands")) summary_tt_bands else NULL,
  shutdown_plot       = if (exists("p_shutdown_time")) p_shutdown_time else NULL,

  coverage3d_by_turbine = if (exists("summary_cov")) summary_cov$by_turbine else NULL
)

build_idf_report(
  output_file   = file.path(folder_output, paste0("IDF_report_", report_start, "to", report_end, ".docx")),
  report_params = report_params
)



## ----------- END SCRIPT -----------