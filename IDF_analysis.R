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
                'openxlsx','writexl','rmarkdown','flextable','systemfonts') 

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
  
<<<<<<< HEAD
  #scripts especificos do IDF
  folder_script_IDF <- "R"
  
  ##USER SETTINGS##
  source(file.path(folder_input,"userSettings_BSH.R")) #Import user defined settings #(e.g. model parameters, etc)
=======
  #scripts especificos do IDF (modulos de analise ainda nao migrados para R/)
  folder_script_IDF <- "scripts_IDF"


  ##USER SETTINGS##
  #Alterar para o ficheiro de settings do projeto a analisar (ex: "userSettings_BSH.R", "userSettings_DGY.R")
  project_settings_file <- "userSettings_BSH.R"
  source(file.path(folder_input, project_settings_file)) #Import user defined settings #(e.g. model parameters, etc)
>>>>>>> 78877ecc773b8a6e11621bad97750903912ccd16
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
<<<<<<< HEAD
  source(file.path(folder_script_IDF, 'read_tracks.R'))   # Tracks data
  source(file.path(folder_script_IDF, 'read_curtailments.R'))  # Curtailments data
  source(file.path(folder_script_IDF, 'read_scada.R'))         # SCADA data
  source(file.path(folder_script_IDF, 'read_heartbeats.R'))     # Heartbeat data
=======
  source("R/read_tracks.R")
  source("R/read_curtailments.R")
  source("R/read_scada.R")
  source("R/read_heartbeats.R")

  track_dt_unfilt  <- read_tracks_data(databases_dir, trackreport_pattern)   # Tracks data
  curtl_dt_unfilt  <- read_curtailments_data(databases_dir, curtailments_pattern) # Curtailments data
  scada_dt_unfilt  <- read_scada_data(databases_dir, scada_pattern)          # SCADA data
  heartb_dt_unfilt <- read_heartbeats_data(databases_dir, heartbeats_pattern, tz = proj_timezone) # Heartbeat data
>>>>>>> 78877ecc773b8a6e11621bad97750903912ccd16
  
  
  #Project-specific corrections --> handle_script.R
  #if(file.exists('handle_script.R')) {source('handle_script.R')}
  
  
  tier_dt <- setDT(tier)
  tier3_dt <- setDT(tier3)
  tier3_dt[, idate := as.IDate(timestamp)]
  set_tier3_dt <- tier3_dt[idate < date(end)]
  
  #check if turbines labels match across all datasets
  Reduce(setdiff, list(
    unique(wtg$ID),
    unique(scada_dt_unfilt$turbinelabel),
    unique(curtl_dt_unfilt$turbine),
    unique(track_dt_unfilt$turbine)
  ))
  #verificar qual falta
  # collect all turbine labels
  turbines <- sort(unique(c(
    wtg$ID,
    scada_dt_unfilt$turbinelabel,
    curtl_dt_unfilt$turbine,
    track_dt_unfilt$turbine
  )))
  
  # build presence table
  labels_table <- data.frame(
    turbine = turbines,
    wtg   = as.integer(turbines %in% wtg$ID),
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
  ## DESATIVADO por agora - depende de scripts_IDF/, ainda nao migrado para R/
  ## Reativar (remover #) quando estes modulos forem migrados


    ###
    ### Check data gaps
    ###

    #Check for data gaps - Graphs
    #source(file.path(folder_script_IDF, 'DQ_check_data_gaps_graphs.R'))

    #Check for data gaps - excel file with data gaps and range
    #source(file.path(folder_script_IDF, 'DQ_check_data_gaps_excel.R'))




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
    curtl_dt <- curtl_dt_unfilt[
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
      filter(timestamp >= ini & timestamp <= end)
    #check
    heartb_dt[, .(
      min_datetime = min(timestamp, na.rm = TRUE),
      max_datetime = max(timestamp, na.rm = TRUE)
    )]
    
    
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

        writexl::write_xlsx(idf_availability_summary$by_idf,
                            file.path(folder_output, "idf_availability_summary_by_idf.xlsx"))
        writexl::write_xlsx(idf_availability_summary$by_month,
                            file.path(folder_output, "idf_availability_summary_by_month.xlsx"))

        # Unidades com mais tempo offline - usado nos 2 graficos abaixo, para
        # manterem as mesmas unidades e ficarem legiveis
        idf_availability_top_n <- 12L
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
      #### TEM HARD CODE ####
  

    
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
      # Analise sera feita apenas para range temporal que tiver dados de SCADA

      #scada_ini  --> definido no userSettings.txt
      #scada_end  --> definido no userSettings.txt
      #turbinas_scada --> definido no userSettings.txt
      #safe_shutdown_rpm --> definido no userSettings.txt

      if (FALSE && exists("scada_dt")) { #Apenas corre se tiver dados de SCADA (desativado)

          turbinas_teste <- turbinas_scada
          turbine_list <- turbinas_scada

          #Preparar tabela CURTAILMENTS+SCADA que vai ser necessario para os prox analises
          source(file.path(folder_script_IDF, 'curtailments_scada_roll_join.R'))
          source(file.path(folder_script_IDF, 'curtailments_time_to_curtail_calc.R'))

          ###... B.1 Response time (time to -10% rpm) ----
          source(file.path(folder_script_IDF, 'response_time.R'))

          ###... B.2 Shutdown time (time to < safe_shutdown_rpm e.g. <1rpm) ----
          source(file.path(folder_script_IDF, 'shutdown_time.R'))

          ###... B.3. Missed  curtailments (if safe_shutdown_rpm is reached e.g. <1rpm is reached) ----
          source(file.path(folder_script_IDF, 'curtailments_missed.R'))

          ###... B.4. Curtailment safe distances (distance to reach rotor at safe_shutdown_rpm) ----
          source(file.path(folder_script_IDF, 'curtailments_safe_distances.R'))

          ###... B.5. Delayed curtailments ----
          source(file.path(folder_script_IDF, 'curtailments_delayed.R'))
          source(file.path(folder_script_IDF, 'curtailments_threshold_dist.R'))
      }

##
## 4. Coverage ----
##


      ### 4.1. WF coverage ----
      #source(file.path(folder_script_IDF, 'coverage_analysis_WF.R'))


      ### 4.2. WTG coverage ----
      #source(file.path(folder_script_IDF, 'coverage_analysis_WTG.R'))




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
## DESATIVADO - depende de scripts_IDF/report.R e dos outputs das seccoes acima
##

    #source(file.path(folder_script_IDF, 'report.R'))



## ----------- END SCRIPT -----------