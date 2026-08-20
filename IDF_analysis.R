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
              'terra', 'RANN', 'plotly', #terra/RANN/plotly: coverage 3D com topografia (DEM)
              'fst', #fst: cache dos datasets grandes (ver R/data_cache.R)
              'cluster') #cluster: silhouette() para validar clusters espaciais de turbinas (secção 10)

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

# write_xlsx_local() -- corrige a exportacao de colunas POSIXct para xlsx
# (writexl::write_xlsx() nao preserva tzone, sai 5h atrasado -- ver
# R/write_utils.R); usada em vez de writexl::write_xlsx() em todo o script
source("R/write_utils.R")

#WTG
wtg <- sf::read_sf(file.path(folder_input, wtg_filename))

#IDF
idf <- sf::read_sf(file.path(folder_input, idf_filename))

#Turbine <-> IDF unit coverage (matriz manual + validacao geometrica) --
#turbine_idf_matrix_filename --> definido no userSettings_BSH.R
#idf_op_detection_range --> definido no userSettings_BSH.R (reutilizado como
#raio do buffer -- e' o mesmo conceito, o raio de deteção operacional do IDF)
source("R/turbine_idf_coverage.R")

turbine_idf_coverage_dt <- compute_turbine_idf_coverage(
  wtg, idf, buffer_m = idf_op_detection_range,
  wtg_id_col = "InternalNa", idf_id_col = "imaging_he"
)
turbine_idf_coverage_wide_dt <- pivot_turbine_idf_coverage_wide(turbine_idf_coverage_dt)

turbine_idf_matrix_file <- file.path(folder_input, turbine_idf_matrix_filename)
if (file.exists(turbine_idf_matrix_file)) {

  turbine_idf_manual_dt <- read_xlsx(turbine_idf_matrix_file)
  turbine_idf_comparison_dt <- compare_turbine_idf_matrix(turbine_idf_manual_dt, turbine_idf_coverage_dt)

  write_xlsx_local(
    list(
      Geometric_long   = turbine_idf_coverage_dt,
      Geometric_wide   = turbine_idf_coverage_wide_dt,
      Manual_matrix    = turbine_idf_manual_dt,
      Comparison       = turbine_idf_comparison_dt
    ),
    file.path(folder_output, "turbine_idf_coverage.xlsx")
  )

} else {
  print("Turbine-IDF manual matrix not available - comparison was skipped (geometric coverage still computed)")
  write_xlsx_local(
    list(Geometric_long = turbine_idf_coverage_dt, Geometric_wide = turbine_idf_coverage_wide_dt),
    file.path(folder_output, "turbine_idf_coverage.xlsx")
  )
}

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
source("R/data_cache.R")

# databases_dir (local) + databases_dir_alt (servidor), quando definido --
# procura ficheiros em ambos, sem duplicar; ordem = precedencia quando o
# mesmo nome de ficheiro existe nos dois (ver R/read_utils.R)
databases_dirs <- unique(c(databases_dir, if (exists("databases_dir_alt")) databases_dir_alt))

##
## Cache (fst) dos 4 datasets grandes -- evita reler os ficheiros brutos (pode
## demorar muito, datasets na ordem dos milhoes de linhas, so crescem ao
## longo do projeto). Por omissao usa a cache local (pasta cache/, fora do
## repositorio) se existir; so relê os ficheiros brutos e regrava a cache
## quando force_reread_cache = TRUE (usar depois de descarregar dados novos).
##
folder_cache <- "cache"
force_reread_cache <- TRUE # -> TRUE depois de descarregar dados novos, so 1 corrida

# As 4 bases de dados vem do portal IdentiFlight ja em hora LOCAL do projeto
# (confirmado -- nao UTC como se assumia antes); read_*_data() reinterpreta
# (force_tz(), nao with_tz()) os timestamps brutos diretamente como
# proj_timezone, sem deslocar o instante. Os roll joins (curtailment_response.R)
# usam sempre diferencas relativas entre os nossos proprios timestamps, por
# isso nao foram afetados mesmo enquanto este bug existiu -- so as horas
# absolutas (comparacao com o portal, agrupamento por dia calendario) e' que
# estavam erradas antes desta correcao.
track_dt_unfilt <- load_or_read_cache(
  file.path(folder_cache, "track_dt_unfilt.fst"),
  function() read_tracks_data(databases_dirs, trackreport_pattern, tz = proj_timezone),
  force_reread = force_reread_cache, tz = proj_timezone
)
curtl_dt_unfilt <- load_or_read_cache(
  file.path(folder_cache, "curtl_dt_unfilt.fst"),
  function() read_curtailments_data(databases_dirs, curtailments_pattern, tz = proj_timezone),
  force_reread = force_reread_cache, tz = proj_timezone
)
scada_dt_unfilt <- load_or_read_cache(
  file.path(folder_cache, "scada_dt_unfilt.fst"),
  function() read_scada_data(databases_dirs, scada_pattern, tz = proj_timezone),
  force_reread = force_reread_cache, tz = proj_timezone
)
heartb_dt_unfilt <- load_or_read_cache(
  file.path(folder_cache, "heartb_dt_unfilt.fst"),
  function() read_heartbeats_data(databases_dirs, heartbeats_pattern, tz = proj_timezone),
  force_reread = force_reread_cache, tz = proj_timezone
)

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
  
  write_xlsx_local(
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
  
  write_xlsx_local(
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
track_dt <- data.table::as.data.table(track_dt)
# count (nº de pontos por track) foi calculado em R/read_tracks.R sobre
# track_dt_unfilt (NAO filtrado) -- o filtro de datas acima so remove
# LINHAS, nao recalcula count, por isso um track que atravessa a fronteira
# do periodo do relatorio ficava com count inflacionado (a incluir pontos
# fora da janela em analise). Recalculado aqui para refletir so o periodo
# do relatorio -- usado em R/curtailment_short_track.R (3.4) e
# R/bio_flight_metrics.R (5.1-5.2).
track_dt[, count := .N, by = track_id]
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

# tier3_track_dt <-
#   track_dt[set_tier3_dt, 
#            on = .(turbine), 
#            nomatch = 0][
#              timestamp > i.timestamp, # filter tier 3 only
#              .SD, 
#              .SDcols = names(track_dt)]



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
## Ja migrado para R/id_transitions.R -- ver secção 3.2, mais abaixo.



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
p_availability_cal
p_availability_freq
p_heartbeat_slots
# System availability - data sent by IDF team
#source(file.path(folder_script_IDF, 'WTG_protect_time.R')) 
# TEM HARD CODE #



## Secções 3.2-3.4 e 6.1-6.2 (abaixo) já migradas para R/ (ver cabeçalhos de
## cada bloco). So a secção "Risk per species" (6.3, R/bio_risk_per_species.R
## no scripts_IDF/) continua por migrar.

### 3.2. Curtailments due to ID transitions ----
## Migrado de scripts_IDF/ID_transitions.R + curtailments_ID_transitions.R
## -- ver R/id_transitions.R para a definicao de risco P->NP (curtailment
## disparado, especie final nao-prioritaria -- custo de producao) e das 2
## direcoes NP->P (sem curtailment / curtailment tardio -- risco biologico),
## incluindo os 2 criterios de "tardio" (tempo e distancia) a comparar antes
## de decidir qual manter.

if (exists("track_dt") && exists("curtl_dt")) {

  source("R/id_transitions.R")

  id_richness_dt <- track_species_summary(track_dt)
  id_richness_summary <- summarise_species_richness(id_richness_dt)

  id_risk_dt <- classify_id_transition_risk(
    id_richness_dt, track_dt, curtl_dt, prioritysp,
    late_time_threshold_sec = id_transition_late_time_sec,
    late_dist_threshold_m = track_proximity_threshold_m
  )
  id_risk_summary <- summarise_id_transition_risk(id_risk_dt, curtl_dt)

  # vista detalhada dos casos "late_curtailment", ordenada por gravidade
  # (both > dist_only > time_only), para inspecao caso a caso -- pedido do
  # Paulo depois de ver os resultados reais (146 casos)
  id_late_cases_dt <- id_transition_late_cases(id_risk_dt, curtl_dt)

  # sensibilidade dos 2 limiares "tarde demais" -- despistar se algum e'
  # pouco efetivo/irrelevante antes de decidir a versao final (Paulo,
  # 2026-08: decidiu manter os 2, mas quer confirmar que ambos discriminam
  # alguma coisa)
  id_late_time_sensitivity_dt <- id_transition_late_time_sensitivity(id_risk_dt)
  id_late_dist_sensitivity_dt <- id_transition_late_dist_sensitivity(id_risk_dt)

  write_xlsx_local(
    list(
      Track_species_richness = id_richness_dt,
      Richness_by_n_species  = id_richness_summary$by_n_species,
      Richness_rate          = id_richness_summary$rate,
      Richness_entropy       = id_richness_summary$entropy,
      ID_transition_risk     = id_risk_dt,
      Risk_by_direction      = id_risk_summary$by_direction,
      Risk_PNP_curtailments  = id_risk_summary$pnp_curtailments,
      Late_criteria_compare  = id_risk_summary$late_criteria_compare,
      Late_cases_detail      = id_late_cases_dt,
      Late_time_sensitivity  = id_late_time_sensitivity_dt,
      Late_dist_sensitivity  = id_late_dist_sensitivity_dt
    ),
    file.path(folder_output, "id_transitions.xlsx")
  )

  p_id_richness <- plot_species_richness_hist(id_richness_dt)
  p_id_richness
  ggsave(
    file.path(folder_output, "id_transition_richness_hist.png"),
    plot = p_id_richness, width = 6, height = 4, dpi = 300, bg = "white"
  )

  p_id_entropy <- plot_entropy_hist(id_richness_dt)
  p_id_entropy
  ggsave(
    file.path(folder_output, "id_transition_entropy_hist.png"),
    plot = p_id_entropy, width = 6, height = 4, dpi = 300, bg = "white"
  )

  p_late_time <- plot_late_time_distribution(id_risk_dt, threshold_sec = id_transition_late_time_sec)
  p_late_time
  ggsave(
    file.path(folder_output, "id_transition_late_time_dist.png"),
    plot = p_late_time, width = 7, height = 4, dpi = 300, bg = "white"
  )

  p_late_dist <- plot_late_dist_distribution(id_risk_dt, threshold_m = track_proximity_threshold_m)
  p_late_dist
  ggsave(
    file.path(folder_output, "id_transition_late_dist_dist.png"),
    plot = p_late_dist, width = 7, height = 4, dpi = 300, bg = "white"
  )

} else {message("track_dt/curtl_dt nao disponiveis -- 3.2 (ID transitions) saltada nesta ronda.")}



### 3.3. Species-specific curtailment ----
## Migrado de scripts_IDF/curtailments_species.R -- ver R/curtailment_species.R
## para a diferenca face a R/id_transitions.R (3.2): aqui e' "que especies
## estao a disparar curtailments", nao "esta classificacao mudou a meio do
## track". Grupo "uncategorized" (species fora de prioritysp/nonprioritysp/
## othersp) fica visivel -- o script original descartava-o em silencio.

if (exists("curtl_dt")) {

  source("R/curtailment_species.R")

  species_curt_dt <- classify_curtailment_species(curtl_dt, prioritysp, nonprioritysp, othersp)
  species_curt_by_species_dt <- summarise_curtailment_species(species_curt_dt)
  species_curt_by_group_dt   <- summarise_curtailment_species_group(species_curt_dt)

  if (species_curt_by_group_dt[species_group == "uncategorized", .N] > 0) {
    message(sprintf(
      "Aviso: %d curtailments com species fora de prioritysp/nonprioritysp/othersp -- ver folha Curtailments_by_species (grupo 'uncategorized').",
      species_curt_by_group_dt[species_group == "uncategorized", n]
    ))
  }

  write_xlsx_local(
    list(
      Curtailments_by_species = species_curt_by_species_dt,
      Curtailments_by_group   = species_curt_by_group_dt
    ),
    file.path(folder_output, "curtailment_species.xlsx")
  )

} else {message("curtl_dt nao disponivel -- 3.3 (species-specific curtailment) saltada nesta ronda.")}



### 3.4. Short-track curtailment ----
## Migrado de scripts_IDF/curtailments_short-track.R -- ver
## R/curtailment_short_track.R para as 2 correcoes feitas: x2d recalculado
## localmente (a coluna curtl_dt$x2d do script original ja nao existe) e
## n_points recalculado a partir do track_dt filtrado (nao do track_dt$count
## desatualizado -- ver correcao em "0. Filter data" acima).

if (exists("track_dt") && exists("curtl_dt")) {

  source("R/curtailment_short_track.R")

  short_track_dt <- classify_short_track_curtailments(
    track_dt, curtl_dt, min_points = shorttrack_min_points, eval_range_m = shorttrack_eval_range
  )
  short_track_summary_dt <- summarise_short_track_curtailments(track_dt, short_track_dt, shorttrack_min_points)
  short_track_by_species_dt <- summarise_short_track_by_species(short_track_dt, prioritysp)

  write_xlsx_local(
    list(
      Short_track_summary    = short_track_summary_dt,
      Short_track_by_species = short_track_by_species_dt,
      Short_track_detail     = short_track_dt
    ),
    file.path(folder_output, "curtailment_short_track.xlsx")
  )

} else {message("track_dt/curtl_dt nao disponiveis -- 3.4 (short-track curtailment) saltada nesta ronda.")}



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
if (exists("scada_dt") && isTRUE(run_sections$curtailment_response)) { #Apenas corre se tiver dados de SCADA e o switch estiver ligado

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
  
  write_xlsx_local(
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
  
  write_xlsx_local(
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
  
  write_xlsx_local(
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

  write_xlsx_local(
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

} else {message("run_sections$curtailment_response = FALSE ou SCADA nao disponivel -- 3.5-3.7 saltadas nesta ronda.")}

summary_safe_dist
p_safe_dist_hist
p_trigger_dist_status

##
## 4. Fatality investigation (tracks + disponibilidade + resposta na janela) ----
##

# track_proximity_threshold_m, fatality_incidents --> definidos no userSettings_BSH.R
# (seccao "Fatality investigation", consolidada -- ver comentario la)
# turbine_idf_manual_dt (se existir) vem da seccao 0 (matriz turbina<->IDF)

if (isTRUE(run_sections$fatality_investigation)) {

  # 1) tracks da especie perto da turbina, na janela -- nao depende de
  #    scada_dt, so precisa de track_dt, curtl_dt e wtg.
  source("R/fatality_track_investigation.R")

  fatality_tracks_dt <- investigate_fatality_incidents(
    fatality_incidents, track_dt, curtl_dt, wtg,
    proximity_threshold_m = track_proximity_threshold_m
  )

  # sumario -- contagens por sinal (quantidades por tipo de track classificado)
  # e os tracks candidatos mais provaveis a colisao (ver signal ==
  # no_curtailment_lost_near_turbine / curtailment_lost_near_turbine em
  # R/fatality_track_investigation.R)
  fatality_summary <- summarise_fatality_tracks(fatality_tracks_dt, top_n = 10)

  # 2) disponibilidade das unidades IDF da turbina + resposta a curtailments,
  #    na mesma janela -- reutiliza os thresholds de 3.1/3.5-3.6 (ver
  #    R/fatality_window_analysis.R). Precisa de heartb_dt; a parte de
  #    resposta a curtailments so produz resultado se scada_dt tambem existir
  #    (fica com detail/by_flag vazios caso contrario, sem gerar erro).
  if (exists("heartb_dt")) {

    source("R/availability_daylight.R")
    source("R/curtailment_response.R")
    source("R/curtailment_shutdown_time.R")
    source("R/curtailment_response_classify.R")
    source("R/track_min_individuals.R")
    source("R/fatality_window_analysis.R")

    fatality_windows <- summarise_fatality_windows(
      fatality_incidents, heartb_dt,
      curtl_dt = curtl_dt,
      scada_dt = if (exists("scada_dt")) scada_dt else data.table::data.table(),
      manual_matrix_dt = if (exists("turbine_idf_manual_dt")) turbine_idf_manual_dt else NULL,
      lat = proj_lat, lon = proj_lon, tz = proj_timezone,
      offline_gap_min = heartbeat_offline_gap_min, online_grace_min = heartbeat_interval_min,
      start_end_gap_sec = curtailment_start_end_gap_sec, max_next_gap_sec = curtailment_max_next_gap_sec,
      drop_pct_threshold = curtailment_drop_pct_threshold, rpm_threshold = safe_shutdown_rpm,
      shutdown_thresholds = shutdown_time_thresholds, shutdown_high_cut_sec = shutdown_time_high_cut,
      fallback_idf_units = heartbeat_idf_units,
      track_dt = track_dt, post_days = fatality_post_incident_days,
      min_indiv_bin_min = min_individuals_bin_min, min_indiv_merge_dist_m = min_individuals_merge_dist_m,
      global_avail_from = ini, global_avail_to = end,
      global_response_from = if (exists("scada_ini")) scada_ini else NULL,
      global_response_to   = if (exists("scada_end")) scada_end else NULL
    )

    # achatar para exportacao -- 1 linha por incidente+unidade (disponibilidade),
    # 1 linha por incidente+curtailment (resposta), com incident_id anexado
    fatality_window_availability_dt <- data.table::rbindlist(lapply(names(fatality_windows), function(id) {
      dt <- fatality_windows[[id]]$availability$by_idf
      if (nrow(dt) == 0L) return(NULL)
      dt[, incident_id := id]
      dt[]
    }), fill = TRUE)

    fatality_window_response_dt <- data.table::rbindlist(lapply(names(fatality_windows), function(id) {
      dt <- fatality_windows[[id]]$curtailment_response$detail
      if (nrow(dt) == 0L) return(NULL)
      dt[, incident_id := id]
      dt[]
    }), fill = TRUE)

    fatality_window_response_summary_dt <- data.table::rbindlist(lapply(names(fatality_windows), function(id) {
      dt <- fatality_windows[[id]]$curtailment_response$by_flag
      if (nrow(dt) == 0L) return(NULL)
      dt[, incident_id := id]
      dt[]
    }), fill = TRUE)

    # baseline "global" -- mesma estrutura, para comparar lado a lado com as
    # tabelas de janela acima (degradou ou melhorou na janela do incidente?)
    fatality_global_availability_dt <- data.table::rbindlist(lapply(names(fatality_windows), function(id) {
      dt <- fatality_windows[[id]]$availability_global$by_idf
      if (is.null(dt) || nrow(dt) == 0L) return(NULL)
      dt[, incident_id := id]
      dt[]
    }), fill = TRUE)

    fatality_global_response_summary_dt <- data.table::rbindlist(lapply(names(fatality_windows), function(id) {
      dt <- fatality_windows[[id]]$curtailment_response_global$by_flag
      if (is.null(dt) || nrow(dt) == 0L) return(NULL)
      dt[, incident_id := id]
      dt[]
    }), fill = TRUE)

    # abundancia pre/pos-incidente -- descritivo, ver
    # summarise_individuals_pre_post() em R/fatality_window_analysis.R
    fatality_abundance_pre_post_dt <- data.table::rbindlist(lapply(names(fatality_windows), function(id) {
      dt <- fatality_windows[[id]]$abundance
      if (is.null(dt) || nrow(dt) == 0L) return(NULL)
      dt[, incident_id := id]
      dt[]
    }), fill = TRUE)

    write_xlsx_local(
      list(
        Fatality_tracks               = fatality_tracks_dt,
        Fatality_signal_counts        = fatality_summary$counts_by_signal,
        Fatality_top_candidates       = fatality_summary$top_candidates,
        Window_availability_by_idf    = fatality_window_availability_dt,
        Window_response_detail        = fatality_window_response_dt,
        Window_response_summary       = fatality_window_response_summary_dt,
        Global_availability_by_idf    = fatality_global_availability_dt,
        Global_response_summary       = fatality_global_response_summary_dt,
        Abundance_pre_post             = fatality_abundance_pre_post_dt
      ),
      file.path(folder_output, "fatality_track_investigation.xlsx")
    )

  } else {
    message("heartb_dt nao disponivel -- analises de janela (disponibilidade/resposta) da secção 4 saltadas, so os tracks foram exportados.")
    write_xlsx_local(
      list(
        Fatality_tracks         = fatality_tracks_dt,
        Fatality_signal_counts  = fatality_summary$counts_by_signal,
        Fatality_top_candidates = fatality_summary$top_candidates
      ),
      file.path(folder_output, "fatality_track_investigation.xlsx")
    )
  }

} else {
  message("run_sections$fatality_investigation = FALSE -- 4 saltada nesta ronda.")
}

fatality_global_response_summary_dt
fatality_abundance_pre_post_dt

attr(curtl_scada_dt$start, "tzone")  # expected "Asia/Samarkand"
curtl_scada_dt[track_id == "F96FD9F7-E742-4588-B495-DEA851EB5495", start]  # esperado: 06:44:52



##
## 5. Coverage ----
##


### 5.1. WF coverage ----
#source(file.path(folder_script_IDF, 'coverage_analysis_WF.R'))


### 5.2. WTG coverage 3D (topografia + deteções de aves) ----

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

if (file.exists(dem_file) && isTRUE(run_sections$coverage_3d)) {

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

  write_xlsx_local(
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

} else {message("run_sections$coverage_3d = FALSE ou DEM nao disponivel -- 5.2 saltada nesta ronda.")}

## Plots interativos de turbinas especificas, so' quando cov_all foi mesmo
## (re)criado nesta ronda -- caso contrario "object 'cov_all' not found"
## (cov_all so' existe dentro do if acima; se run_sections$coverage_3d =
## FALSE ou o DEM nao existir, nao ha cov_all para usar aqui)
if (exists("cov_all")) {

  plot_coverage_3d_for_turbine(cov_all, "BSH61", coverage_cylinder_wider_radius,
                               coverage_cylinder_height)

  plot_coverage_3d_for_turbine(cov_all, "BSH61", coverage_cylinder_wider_radius,
                               coverage_cylinder_height, not_covered = TRUE)

  plot_coverage_3d_for_turbine(cov_all, "BSH14", coverage_cylinder_wider_radius,
                               coverage_cylinder_height)

  plot_coverage_3d_for_turbine(cov_all, "BSH14", coverage_cylinder_wider_radius,
                               coverage_cylinder_height, not_covered = TRUE)

} else {message("cov_all nao disponivel nesta ronda -- plots individuais BSH61/BSH14 saltados.")}

##
## 6. Biological (supporting info) ----
##


### 6.1. Flight speed per species ----
### 6.2. Flight height per species ----
## Migrado de scripts_IDF/bio_flight_speed.R, bio_flight_height.R e
## bio_distrib_flight_height_speed_per_species.R -- ver R/bio_flight_metrics.R
## para a unificacao dos filtros de qualidade (os 3 scripts originais usavam
## bases ligeiramente diferentes entre si) e o novo diagnostico de alturas
## negativas (flight_height_qa(), visivel em vez de descartado em silencio).

if (exists("track_dt")) {

  source("R/bio_flight_metrics.R")

  flight_base_dt <- flight_metrics_base(
    track_dt, prioritysp, min_track_points = flight_min_track_points,
    speed_ms_min = flight_speed_ms_min, speed_ms_max = flight_speed_ms_max
  )
  flight_speed_summary_dt  <- summarise_flight_speed(flight_base_dt)
  flight_height_summary_dt <- summarise_flight_height(flight_base_dt)
  flight_height_qa_dt      <- flight_height_qa(
    track_dt, prioritysp, min_track_points = flight_min_track_points,
    speed_ms_min = flight_speed_ms_min, speed_ms_max = flight_speed_ms_max
  )

  write_xlsx_local(
    list(
      Flight_speed_by_species  = flight_speed_summary_dt,
      Flight_height_by_species = flight_height_summary_dt,
      Flight_height_QA         = flight_height_qa_dt
    ),
    file.path(folder_output, "bio_flight_metrics.xlsx")
  )

  p_flight_metrics <- plot_flight_metrics_distribution(flight_base_dt, riskHeight_lower, riskHeight_upper)
  p_flight_metrics
  n_species_flight <- length(unique(flight_base_dt$spec))
  ggsave(
    file.path(folder_output, "bio_flight_metrics_distribution.png"),
    plot = p_flight_metrics, width = 8, height = max(4, 2.2 * n_species_flight), dpi = 300, bg = "white"
  )

} else {message("track_dt nao disponivel -- 6.1-6.2 (flight speed/height) saltadas nesta ronda.")}



### 6.3. Risk per species ----

#NOTA:
# riskHeight_lower --> definido no userSettings.txt
# riskHeight_upper --> definido no userSettings.txt
#source(file.path(folder_script_IDF, 'bio_risk_per_species.R'))


### 6.4. Minimum individuals per time bin ----

# min_individuals_bin_min, min_individuals_merge_dist_m --> definidos no userSettings_BSH.R
# Modulo geral e reutilizavel (qualquer especie, qualquer periodo, farm-wide)
# -- ver R/track_min_individuals.R. Por omissao corre para as especies
# prioritarias (prioritysp) em todo o periodo do projeto (ini/end); para uma
# janela ou especie especifica, chamar count_min_individuals_per_bin()
# diretamente (ex: species = "Steppe-Eagle", date_from/date_to = janela de 8
# dias antes de uma fatalidade).

if (isTRUE(run_sections$min_individuals)) {

source("R/track_min_individuals.R")

min_indiv_bins_dt <- count_min_individuals_per_bin(
  track_dt, species = prioritysp,
  bin_min = min_individuals_bin_min, merge_dist_m = min_individuals_merge_dist_m
)

min_indiv_summary_dt <- summarise_min_individuals(min_indiv_bins_dt)

# sintese diaria (maximo diario por especie) -- leitura fenologica/sazonal,
# menos densa que os bins de 2min para todo o periodo do projeto
min_indiv_daily_dt <- summarise_daily_max_individuals(min_indiv_bins_dt)

write_xlsx_local(
  list(
    Min_individuals_bins    = min_indiv_bins_dt,
    Min_individuals_summary = min_indiv_summary_dt,
    Min_individuals_daily   = min_indiv_daily_dt
  ),
  file.path(folder_output, "min_individuals_per_bin.xlsx")
)

# altura ajustada ao nº de especies (1 facet por especie, ncol = 1)
n_species_min_indiv <- length(unique(min_indiv_bins_dt$spec))

p_min_indiv <- plot_min_individuals_per_bin(min_indiv_bins_dt)
p_min_indiv
ggsave(
  file.path(folder_output, "min_individuals_per_bin.png"),
  plot = p_min_indiv, width = 8, height = max(4, 2.2 * n_species_min_indiv), dpi = 300, bg = "white"
)

p_min_indiv_daily <- plot_daily_max_individuals(min_indiv_daily_dt)
p_min_indiv_daily
ggsave(
  file.path(folder_output, "min_individuals_daily_max.png"),
  plot = p_min_indiv_daily, width = 8, height = max(4, 2.2 * n_species_min_indiv), dpi = 300, bg = "white"
)

} else {message("run_sections$min_individuals = FALSE -- 6.4 saltada nesta ronda.")}

inspect_min_individuals_bin(track_dt, species = "Steppe-Eagle", bin_start = "2026-02-18 14:34:00")

##
## 7. Report support -- data extent summary ----
##
## Duas tabelas de apoio a escrita de relatorios (ver CLAUDE.md: cada
## seccao deve ser auto-explicativa quanto ao universo de dados que usa) --
## n de linhas e janela temporal de cada conjunto/subconjunto analisado, um
## sumario para o panorama geral do parque, outro para a analise especifica
## de turbinas (BSH54/BSH62). Ver R/dataset_summary.R.
##

source("R/dataset_summary.R")

general_data_summary_dt <- summarise_dataset_extent(list(
  Tracks       = list(dt = track_dt,  date_col = "timestamp"),
  Curtailments = list(dt = curtl_dt,  date_col = "start"),
  SCADA        = list(dt = scada_dt,  date_col = "datetime"),
  Heartbeats   = list(dt = heartb_dt, date_col = "timestamp")
))

turbine_summary_list <- list()
if (exists("curtl_scada_dt")) turbine_summary_list$Curtailments_BSH54_BSH62 <- list(dt = curtl_scada_dt, date_col = "start")
if (exists("assess_dt"))      turbine_summary_list$Response_assessment     <- list(dt = assess_dt, date_col = "start")
if (exists("tt_dt"))          turbine_summary_list$Shutdown_time           <- list(dt = tt_dt, date_col = "start")
if (exists("safe_dist_dt"))   turbine_summary_list$Safe_distance           <- list(dt = safe_dist_dt, date_col = "start")
if (exists("fatality_tracks_dt")) turbine_summary_list$Fatality_tracks     <- list(dt = fatality_tracks_dt, date_col = "first_time")

turbine_data_summary_dt <- summarise_dataset_extent(turbine_summary_list)

write_xlsx_local(
  list(General_data_summary = general_data_summary_dt, Turbine_data_summary = turbine_data_summary_dt),
  file.path(folder_output, "data_extent_summary.xlsx")
)


##
## 8. System performance vs. bird phenology (evolucao temporal) ----
##
## Evolucao temporal (por omissao semanal, response_timeline_unit --
## definido no userSettings_BSH.R) da qualidade de resposta a curtailments
## (missed/delayed) em curtl_scada_dt, sobreposta a abundancia (min
## individuals) das especies dos incidentes de fatalidade -- para dar
## contexto a se a performance do sistema varia com os periodos de maior
## movimento migratorio. Ver R/curtailment_response_timeline.R. So corre se
## a seccao 3.5-3.7 (curtl_scada_dt) e a 6.4 (min_indiv_bins_dt) tiverem
## corrido (run_sections + dados disponiveis).
##

if (exists("curtl_scada_dt") && exists("min_indiv_bins_dt")) {

  source("R/curtailment_response_classify.R")
  source("R/curtailment_response_timeline.R")

  response_flag_dt <- classify_response_flag(
    curtl_scada_dt, scada_dt,
    start_end_gap_sec = curtailment_start_end_gap_sec, max_next_gap_sec = curtailment_max_next_gap_sec,
    drop_pct_threshold = curtailment_drop_pct_threshold, rpm_threshold = safe_shutdown_rpm,
    shutdown_thresholds = shutdown_time_thresholds, shutdown_high_cut_sec = shutdown_time_high_cut
  )

  response_timeline_dt  <- summarise_response_timeline(response_flag_dt, unit = response_timeline_unit)
  abundance_timeline_dt <- summarise_abundance_timeline(min_indiv_bins_dt, unit = response_timeline_unit)

  write_xlsx_local(
    list(Response_timeline = response_timeline_dt, Abundance_timeline = abundance_timeline_dt),
    file.path(folder_output, "response_vs_phenology_timeline.xlsx")
  )

  # um par de plots por especie de fatality_incidents -- contexto direto
  # para a seccao nova do relatorio
  for (sp in unique(fatality_incidents$species)) {
    p_pair <- plot_response_vs_phenology(response_timeline_dt, abundance_timeline_dt, species_sel = sp)
    sp_slug <- gsub("[^A-Za-z0-9]+", "_", sp)
    ggsave(
      file.path(folder_output, paste0("response_timeline_", sp_slug, ".png")),
      plot = p_pair$response_plot, width = 8, height = 4, dpi = 300, bg = "white"
    )
    ggsave(
      file.path(folder_output, paste0("abundance_timeline_", sp_slug, ".png")),
      plot = p_pair$abundance_plot, width = 8, height = 4, dpi = 300, bg = "white"
    )
  }

} else {
  message("curtl_scada_dt e/ou min_indiv_bins_dt nao disponiveis -- seccao 8 (performance vs. fenologia) saltada.")
}


##
## 9. Turbine recent activity (apoio a matriz de decisao do protocolo de
##    resposta a outages do IdentiFlight) ----
##
## recent_activity_days --> definido no userSettings_BSH.R. Farm-wide (79
## turbinas), nao so' as que tem SCADA -- ver R/turbine_recent_activity.R.
##

source("R/turbine_recent_activity.R")

turbine_recent_activity_dt <- summarise_turbine_recent_activity(
  track_dt, wtg, species = prioritysp,
  date_from = end - lubridate::days(recent_activity_days), date_to = end,
  proximity_threshold_m = track_proximity_threshold_m
)

write_xlsx_local(
  list(Turbine_recent_activity = turbine_recent_activity_dt),
  file.path(folder_output, "turbine_recent_activity.xlsx")
)


##
## 10. Turbine spatial/temporal clustering ----
##
## cluster_max_dist_m, cluster_threshold_sweep_m, cluster_perm_n,
## manual_turbine_clusters, cluster_species_sel, curtl_cluster_date_from/to
## --> definidos no userSettings_BSH.R.
##

## isTRUE(run_sections$turbine_clustering) fica FALSE tambem se run_sections
## nao tiver este elemento (ex: userSettings_BSH.R desatualizado ainda em
## memoria, sem re-fazer source() depois de adicionar este switch) -- por
## isso o guard fica separado em 3 mensagens especificas, para nao dar a
## entender que track_dt_unfilt/curtl_dt_unfilt e' que faltam quando pode
## ser so' o run_sections desatualizado
if (!exists("track_dt_unfilt")) {
  message("10 saltada: track_dt_unfilt nao existe (ver secção 0 -- Import data).")
} else if (!exists("curtl_dt_unfilt")) {
  message("10 saltada: curtl_dt_unfilt nao existe (ver secção 0 -- Import data).")
} else if (!isTRUE(run_sections$turbine_clustering)) {
  message("10 saltada: run_sections$turbine_clustering != TRUE (confirma se o userSettings_BSH.R em memoria esta atualizado -- corre source() outra vez se tiveres duvidas).")
} else {

  source("R/turbine_spatial_clusters.R")
  source("R/curtailment_cluster_patterns.R")
  source("R/track_species_clusters.R")

  turbine_dist_mat <- turbine_distance_matrix(wtg)

  cluster_dt_stat   <- cluster_turbines_by_distance(turbine_dist_mat, max_dist_m = cluster_max_dist_m)
  cluster_sens_dt   <- turbine_cluster_threshold_sensitivity(turbine_dist_mat, thresholds_m = cluster_threshold_sweep_m)
  cluster_dt_manual <- manual_turbine_clusters_dt(manual_turbine_clusters)

  # clusters que contem as turbinas de fatality_incidents -- para destacar
  # nos plots (a vermelho) em vez de ter de os procurar a olho na legenda
  highlight_clusters_stat   <- unique(cluster_dt_stat[turbine %in% fatality_incidents$turbine, cluster_id])
  highlight_clusters_manual <- unique(cluster_dt_manual[turbine %in% fatality_incidents$turbine, cluster_id])

  # mapas de referencia -- turbinas coloridas pelo cluster a que pertencem,
  # para confirmar a olho que os clusters fazem sentido geograficamente
  p_map_stat <- plot_turbine_clusters_map(
    wtg, cluster_dt_stat, highlight_turbines = fatality_incidents$turbine,
    title = sprintf("Statistical turbine clusters (max_dist_m=%g)", cluster_max_dist_m)
  )
  ggsave(
    file.path(folder_output, "turbine_clusters_map_stat.png"),
    plot = p_map_stat, width = 10, height = 8, dpi = 300, bg = "white"
  )

  p_map_manual <- plot_turbine_clusters_map(
    wtg, cluster_dt_manual, highlight_turbines = fatality_incidents$turbine,
    title = "Manual turbine sectors"
  )
  ggsave(
    file.path(folder_output, "turbine_clusters_map_manual.png"),
    plot = p_map_manual, width = 10, height = 8, dpi = 300, bg = "white"
  )


  ### 10.1. Curtailments por cluster de turbinas (estatistico + manual) ----

  curtl_cl_stat_dt <- join_curtailments_to_clusters(
    curtl_dt_unfilt, cluster_dt_stat, date_from = curtl_cluster_date_from, date_to = curtl_cluster_date_to
  )
  if (curtl_cl_stat_dt[is.na(cluster_id), .N] > 0) {
    message(sprintf(
      "Aviso: %d curtailments de turbinas fora do cluster estatistico (turbina nao existe em wtg?) -- turbinas: %s",
      curtl_cl_stat_dt[is.na(cluster_id), .N],
      paste(sort(unique(curtl_cl_stat_dt[is.na(cluster_id), turbine])), collapse = ", ")
    ))
  }
  cluster_summary_stat    <- summarise_cluster_curtailments(curtl_cl_stat_dt)
  cluster_weekly_stat_dt  <- summarise_cluster_curtailments_weekly(curtl_cl_stat_dt)
  marginal_stat_dt        <- summarise_turbine_marginal_contribution(curtl_cl_stat_dt, fatality_incidents$turbine)
  perm_stat_dt            <- permutation_test_marginal_contribution_all(
    curtl_cl_stat_dt, fatality_incidents$turbine, n_perm = cluster_perm_n
  )

  curtl_cl_manual_dt <- join_curtailments_to_clusters(
    curtl_dt_unfilt, cluster_dt_manual, date_from = curtl_cluster_date_from, date_to = curtl_cluster_date_to
  )
  if (curtl_cl_manual_dt[is.na(cluster_id), .N] > 0) {
    message(sprintf(
      "Aviso: %d curtailments de turbinas fora dos setores manuais (manual_turbine_clusters incompleto?) -- turbinas: %s",
      curtl_cl_manual_dt[is.na(cluster_id), .N],
      paste(sort(unique(curtl_cl_manual_dt[is.na(cluster_id), turbine])), collapse = ", ")
    ))
  }
  cluster_summary_manual   <- summarise_cluster_curtailments(curtl_cl_manual_dt)
  cluster_weekly_manual_dt <- summarise_cluster_curtailments_weekly(curtl_cl_manual_dt)
  marginal_manual_dt       <- summarise_turbine_marginal_contribution(curtl_cl_manual_dt, fatality_incidents$turbine)
  perm_manual_dt           <- permutation_test_marginal_contribution_all(
    curtl_cl_manual_dt, fatality_incidents$turbine, n_perm = cluster_perm_n
  )

  write_xlsx_local(
    list(
      Cluster_threshold_sweep    = cluster_sens_dt,
      Stat_cluster_by_turbine    = cluster_summary_stat$by_turbine,
      Stat_cluster_by_cluster    = cluster_summary_stat$by_cluster,
      Stat_cluster_weekly        = cluster_weekly_stat_dt,
      Stat_marginal_contribution = marginal_stat_dt,
      Stat_permutation_test      = perm_stat_dt,
      Manual_cluster_by_turbine  = cluster_summary_manual$by_turbine,
      Manual_cluster_by_cluster  = cluster_summary_manual$by_cluster,
      Manual_cluster_weekly      = cluster_weekly_manual_dt,
      Manual_marginal_contrib    = marginal_manual_dt,
      Manual_permutation_test    = perm_manual_dt
    ),
    file.path(folder_output, "curtailment_cluster_patterns.xlsx")
  )

  p_curtl_stat_total <- plot_cluster_curtailments_total(
    cluster_summary_stat, highlight_clusters = highlight_clusters_stat,
    title = "Curtailments by statistical cluster (full period)"
  )
  ggsave(
    file.path(folder_output, "cluster_curtailments_stat_total.png"),
    plot = p_curtl_stat_total, width = 8, height = 5, dpi = 300, bg = "white"
  )

  p_curtl_stat_weekly <- plot_cluster_curtailments_weekly(
    cluster_weekly_stat_dt, highlight_clusters = highlight_clusters_stat,
    title = "Weekly curtailments by statistical cluster"
  )
  ggsave(
    file.path(folder_output, "cluster_curtailments_stat_weekly.png"),
    plot = p_curtl_stat_weekly, width = 9, height = 5, dpi = 300, bg = "white"
  )

  p_curtl_manual_total <- plot_cluster_curtailments_total(
    cluster_summary_manual, highlight_clusters = highlight_clusters_manual,
    title = "Curtailments by manual sector (full period)"
  )
  ggsave(
    file.path(folder_output, "cluster_curtailments_manual_total.png"),
    plot = p_curtl_manual_total, width = 8, height = 5, dpi = 300, bg = "white"
  )

  p_curtl_manual_weekly <- plot_cluster_curtailments_weekly(
    cluster_weekly_manual_dt, highlight_clusters = highlight_clusters_manual,
    title = "Weekly curtailments by manual sector"
  )
  ggsave(
    file.path(folder_output, "cluster_curtailments_manual_weekly.png"),
    plot = p_curtl_manual_weekly, width = 9, height = 5, dpi = 300, bg = "white"
  )


  ### 10.2. Ocorrencia de tracks de especie por cluster de turbinas ----

  species_tracks_dt <- assign_tracks_to_nearest_turbine(track_dt_unfilt, wtg, species_sel = cluster_species_sel)
  species_label_txt  <- paste(cluster_species_sel, collapse = "/")

  species_weekly_dt     <- summarise_track_occurrence_weekly(species_tracks_dt)
  species_by_turbine_dt <- summarise_track_occurrence_by_turbine(species_tracks_dt)
  species_by_cluster_stat   <- summarise_track_occurrence_by_cluster(species_tracks_dt, cluster_dt_stat)
  species_by_cluster_manual <- summarise_track_occurrence_by_cluster(species_tracks_dt, cluster_dt_manual)

  write_xlsx_local(
    list(
      Species_weekly            = species_weekly_dt,
      Species_by_turbine        = species_by_turbine_dt,
      Species_stat_cluster      = species_by_cluster_stat$by_cluster,
      Species_stat_weekly       = species_by_cluster_stat$weekly,
      Species_manual_cluster    = species_by_cluster_manual$by_cluster,
      Species_manual_weekly     = species_by_cluster_manual$weekly
    ),
    file.path(folder_output, "track_species_clusters.xlsx")
  )

  p_species_weekly <- plot_species_occurrence_weekly(species_weekly_dt, species_label = species_label_txt)
  ggsave(
    file.path(folder_output, "species_occurrence_weekly.png"),
    plot = p_species_weekly, width = 9, height = 4, dpi = 300, bg = "white"
  )

  p_species_stat_total <- plot_cluster_species_total(
    species_by_cluster_stat, highlight_clusters = highlight_clusters_stat, species_label = species_label_txt
  )
  ggsave(
    file.path(folder_output, "cluster_species_stat_total.png"),
    plot = p_species_stat_total, width = 8, height = 5, dpi = 300, bg = "white"
  )

  p_species_stat_weekly <- plot_cluster_species_weekly(
    species_by_cluster_stat, highlight_clusters = highlight_clusters_stat, species_label = species_label_txt
  )
  ggsave(
    file.path(folder_output, "cluster_species_stat_weekly.png"),
    plot = p_species_stat_weekly, width = 9, height = 5, dpi = 300, bg = "white"
  )

  p_species_manual_total <- plot_cluster_species_total(
    species_by_cluster_manual, highlight_clusters = highlight_clusters_manual, species_label = species_label_txt
  )
  ggsave(
    file.path(folder_output, "cluster_species_manual_total.png"),
    plot = p_species_manual_total, width = 8, height = 5, dpi = 300, bg = "white"
  )

  p_species_manual_weekly <- plot_cluster_species_weekly(
    species_by_cluster_manual, highlight_clusters = highlight_clusters_manual, species_label = species_label_txt
  )
  ggsave(
    file.path(folder_output, "cluster_species_manual_weekly.png"),
    plot = p_species_manual_weekly, width = 9, height = 5, dpi = 300, bg = "white"
  )


  ### 10.3. Sumario "zona critica" -- comparacao expedita por turbina de
  ###       interesse: cluster com muitos curtailments E muito movimento
  ###       da especie, para estatistico e manual ----

  source("R/turbine_critical_zone_summary.R")

  species_cluster_rank_stat_dt <- summarise_turbine_species_cluster_rank(
    species_tracks_dt, cluster_dt_stat, fatality_incidents$turbine
  )
  species_cluster_rank_manual_dt <- summarise_turbine_species_cluster_rank(
    species_tracks_dt, cluster_dt_manual, fatality_incidents$turbine
  )

  critical_zone_stat_dt   <- summarise_turbine_critical_zone(marginal_stat_dt, species_cluster_rank_stat_dt)
  critical_zone_manual_dt <- summarise_turbine_critical_zone(marginal_manual_dt, species_cluster_rank_manual_dt)

  write_xlsx_local(
    list(
      Critical_zone_statistical = critical_zone_stat_dt,
      Critical_zone_manual      = critical_zone_manual_dt
    ),
    file.path(folder_output, "turbine_critical_zone_summary.xlsx")
  )

}


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
