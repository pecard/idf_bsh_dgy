##
## Reporting in .docx using markdown
##



##Needed packages
packages <- c('rmarkdown','flextable') 

##Check and install packages that are missing + call library()
for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE) }



### Export to doc ----
    
    
    #rmarkdown::render() works relative to the input file location
    output_dir_abs <- normalizePath(folder_output, winslash = "/", mustWork = TRUE)
    scripts_dir_abs <- normalizePath(folder_script_IDF, winslash = "/", mustWork = TRUE)

      rmarkdown::render( 
        input = file.path(scripts_dir_abs,"report_template.rmd"),
        output_file = file.path(output_dir_abs,paste0("IDF_auto-report_",project_ref,"_",report_start,"to",report_end,".docx")),
        params = list(
          title = paste0("IDF validation report - ",project_ref),
          project_ref = project_ref,
          analysis_date = format(Sys.time(), "%Y-%m-%d"),
          start_date = report_start,
          end_date = report_end,
          username = username,
          script_version = script_version,
          
          #Geral
          num_IDF = total_idf_units,
          num_WTG = nrow(wtg),
          total_wtg_covered = total_turbines_covered,
          prop_wtg_covered = round((total_turbines_covered/nrow(wtg))*100,2),
          riskHeight_lower = riskHeight_lower,
          riskHeight_upper = riskHeight_upper,
          idf_op_detection_range = idf_op_detection_range,
          shorttrack_min_points = shorttrack_min_points,
          shorttrack_eval_range = shorttrack_eval_range,
          safe_shutdown_threshold = safe_shutdown_rpm,
          turbinas_scada = turbinas_scada,
          
          #ID transitions
          ID_transitions_table = read_excel(file.path(output_dir_abs,"ID_transition_metrics.xlsx"),sheet = 1),
          ID_entropy_table = read_excel(file.path(output_dir_abs,"ID_entropy_metrics.xlsx"),sheet = 1),
          ID_transition_graph = file.path(output_dir_abs,"ID_transition_plot.png"),
          
          #Curtailments
          curtail_spatial_graph = file.path(output_dir_abs,"curtails_all_spatial(colored).png"),
          curtail_sp_table = read_excel(file.path(output_dir_abs,"curtls_prioritysp.xlsx"),sheet = 1),
          curtail_stats_table = read_excel(file.path(output_dir_abs,"summary_tracks_curtailments.xlsx"),sheet = 1),
          short_track_curtail_table = read_excel(file.path(output_dir_abs,"short-track_curtails_prop.xlsx"),sheet = 1),
          short_track_curtail_priority_table = read_excel(file.path(output_dir_abs,"short-track_curtails_per_prioritysp.xlsx"),sheet = 1),
          curtail_ID_transitions_table = read_excel(file.path(output_dir_abs,"ID_transition_curt_P-NP_summary.xlsx"),sheet = 1),
          curtail_missed_table = read_excel(file.path(output_dir_abs,paste0("curtail_Missed_summary_",date(scada_ini),"to",date(scada_end),".xlsx")),sheet = 1),
          missed_events_table = read_excel(file.path(output_dir_abs,paste0("curtail_Missed_events_",date(scada_ini),"to",date(scada_end),".xlsx")),sheet = 1),
          
          #
          response_time_table = read_excel(file.path(output_dir_abs, paste0("response_time_summary_",date(scada_ini),"to",date(scada_end),".xlsx")),sheet = 1),
          shutdown_time_graph = file.path(output_dir_abs,paste0("shutdown_time_categ_",date(scada_ini),"to",date(scada_end),".png")),
          shutdown_time_table = read_excel(file.path(output_dir_abs, paste0("shutdown_time_class_summary_",date(scada_ini),"to",date(scada_end),".xlsx")),sheet = 1),
          shutdown_time_distrib = file.path(output_dir_abs,paste0("shutdown_time_dist_",date(scada_ini),"to",date(scada_end),".png")),
          
          #Delayed curtailments
          delayed_curtail_graph = file.path(output_dir_abs,paste0("curtail_Falling&Exceed_allPriority_",date(scada_ini),"to",date(scada_end),".png")),
          delayed_curtail_per_priority_graph = file.path(output_dir_abs,paste0("curtail_Falling&Exceed_perPriority_",date(scada_ini),"to",date(scada_end),".png")),
          delayed_curtail_per_priority_prop_graph = file.path(output_dir_abs,paste0("curtail_PropCrit_distance_allPriority_",date(scada_ini),"to",date(scada_end),".png")),
          
          #Safe distance
          safe_dist_graph = file.path(output_dir_abs,paste0("curtail_SafeDist_allPriority",date(scada_ini),"to",date(scada_end),".png")),
          safe_dist_per_priority_graph = file.path(output_dir_abs,paste0("curtail_SafeDist_perPriority",date(scada_ini),"to",date(scada_end),".png")),
          
          #Coverage
          coverage_wf_graph = file.path(output_dir_abs, "IDF_coverage_WF.png"),
          IDF_coverage_WF_table = read_excel(file.path(output_dir_abs,"IDF_coverage_WF_summary.xlsx"),sheet = 1),
          coverage_per_turbine_wider_graph = file.path(output_dir_abs,"idf_3Dcoverage_c100r1000","idf_coverage&cov-risk_freq_c100m-r1000m.png"),
          coverage_per_turbine_wider_spatial_graph = file.path(output_dir_abs,"idf_3Dcoverage_c100r1000","idf_coverage_spatial(colored2).png"),
          
          coverage_per_turbine_inner_graph = file.path(output_dir_abs,"idf_3Dcoverage_c100r600","idf_coverage&cov-risk_freq_c100m-r1000m.png"),
          coverage_per_turbine_inner_spatial_graph = file.path(output_dir_abs,"FlightSpeed&Height_prioritysp.png"),
          
          flight_risk_table = read_excel(file.path(output_dir_abs,"TracksRisk_summary-priority.xlsx"),sheet = 1),
          flight_risk_per_sp_table = read_excel(file.path(output_dir_abs,"TracksRisk_per_sp-priority.xlsx"),sheet = 1),
          flight_height_sp_table = read_excel(file.path(output_dir_abs,"Flight_height_sp.xlsx"),sheet = 1),
          flight_speed_sp_table = read_excel(file.path(output_dir_abs,"Flight_speed_sp-priority.xlsx"),sheet = 1),
          flight_height_per_sp_graph = file.path(output_dir_abs,"FlightSpeed&Height_prioritysp.png")
        ),
        envir = new.env(parent = globalenv())
      )
      
      
      remove(output_dir_abs,scripts_dir_abs)
