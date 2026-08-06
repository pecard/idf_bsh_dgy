###
### General metrics
###

    
    ##
    ## Tracks & track records ----
    ##
    
    #Total tracks
    total_tracks <- nrow(track_dt) #Total num tracks
    #Total Records and Tracks
    tracks_summary <- track_dt[, .(records = .N, tracks = uniqueN(track_id))]
    #Total Records and Tracks for priority birds
    priority_tracks_summary <- track_dt[spec %in% prioritysp,][, .(records = .N, tracks = uniqueN(track_id))]
    #Total Records and Tracks for non-priority raptors
    nonpriority_tracks_summary <- track_dt[spec %in% nonprioritysp,][, .(records = .N, tracks = uniqueN(track_id))]
    #Total Records and Tracks for other
    other_tracks_summary <- track_dt[spec %in% othersp,][, .(records = .N, tracks = uniqueN(track_id))]

    
    ##
    ## IDFs ----
    ##
    total_idf_units <- length(track_dt[!duplicated(idf)][,idf]) # Total num of IDF units
 
    
    ##
    ## Turbines ----
    ##
    total_turbines_covered <- length(track_dt[!duplicated(turbine)][,turbine]) # Total num of turbines 
    
    
    ##
    ## Curtailments ----
    ##
    
    #Total curtailments
    total_curtailments <- nrow(curtl_dt) #Total num curtailments
    #Total curtailments for priority birds
    priority_curtailments <- setDT(curtl_dt)[species %in% prioritysp, ][, .(records = .N)]
    #Total curtailments for non-priority birds
    nonpriority_curtailments <- setDT(curtl_dt)[species %in% nonprioritysp, ][, .(records = .N)]
    #Total curtailments for other
    other_curtailments <- setDT(curtl_dt)[species %in% othersp, ][, .(records = .N)]
    
    ##
    ## Curatilment duration
    ##
    
    curtl_dt[, curtail_duration_s := as.numeric(difftime(end, start, units = "secs"))]
    curtl_dt[, curtail_duration_m := as.numeric(difftime(end, start, units = "mins"))] 
    
    # Summary of curtailment duration
    dur_curtail <- setDT(curtl_dt)[,
                                            .(
                                              mean_duration = mean(curtail_duration_m, na.rm = TRUE),
                                              sd_duration   = sd(curtail_duration_m, na.rm = TRUE),
                                              min_duration  = min(curtail_duration_m, na.rm = TRUE),
                                              max_duration  = max(curtail_duration_m, na.rm = TRUE)
                                            )
    ] 
    
    # Summary of curtailment duration for priority birds
    priority_dur_curtail <- setDT(curtl_dt)[species %in% prioritysp,
                                             .(
                                               mean_duration = mean(curtail_duration_m, na.rm = TRUE),
                                               sd_duration   = sd(curtail_duration_m, na.rm = TRUE),
                                               min_duration  = min(curtail_duration_m, na.rm = TRUE),
                                               max_duration  = max(curtail_duration_m, na.rm = TRUE)
                                             )
    ]
    
    # Summary of curtailment duration for non-priority birds
    nonpriority_dur_curtail <- setDT(curtl_dt)[species %in% nonprioritysp,
                                                .(
                                                  mean_duration = mean(curtail_duration_m, na.rm = TRUE),
                                                  sd_duration   = sd(curtail_duration_m, na.rm = TRUE),
                                                  min_duration  = min(curtail_duration_m, na.rm = TRUE),
                                                  max_duration  = max(curtail_duration_m, na.rm = TRUE)
                                                )
    ]
    
    # Summary of curtailment duration for other species
    other_dur_curtail <- setDT(curtl_dt)[species %in% othersp,
                                          .(
                                            mean_duration = mean(curtail_duration_m, na.rm = TRUE),
                                            sd_duration   = sd(curtail_duration_m, na.rm = TRUE),
                                            min_duration  = min(curtail_duration_m, na.rm = TRUE),
                                            max_duration  = max(curtail_duration_m, na.rm = TRUE)
                                          )
    ]
    
    
    ##
    ## Total curtailment duration
    ##
    
    total_curt_duration <- curtl_dt[,
                               .(
                                 curtail_duration_m = sum(curtail_duration_m, na.rm = TRUE),
                                 curtail_duration_h = sum(curtail_duration_m, na.rm = TRUE) / 60
                               )
    ]
    
    priority_total_curt_duration <- curtl_dt[species %in% prioritysp,
                                    .(
                                      curtail_duration_m = sum(curtail_duration_m, na.rm = TRUE),
                                      curtail_duration_h = sum(curtail_duration_m, na.rm = TRUE) / 60
                                    )
    ]
    
    nonpriority_total_curt_duration <- curtl_dt[species %in% nonprioritysp,
                                             .(
                                               curtail_duration_m = sum(curtail_duration_m, na.rm = TRUE),
                                               curtail_duration_h = sum(curtail_duration_m, na.rm = TRUE) / 60
                                             )
    ]
    
  other_total_curt_duration <- curtl_dt[species %in% othersp,
                                                .(
                                                  curtail_duration_m = sum(curtail_duration_m, na.rm = TRUE),
                                                  curtail_duration_h = sum(curtail_duration_m, na.rm = TRUE) / 60
                                                )
    ]
    
    
      
      
    
      # Build wide summary table
      results_table <- data.frame(
        Metric = c(
          "Total curtailments",
          "Total track records",
          "Total unique tracks"
        ),
        Total = c(
          total_curtailments,
          tracks_summary$records,
          tracks_summary$tracks
        ),
        `Priority sp` = c(
          priority_curtailments$records,
          priority_tracks_summary$records,
          priority_tracks_summary$tracks
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      # Calculate proportion (% of priority species over total)
      results_table$Prop <- round(
        (results_table$`Priority sp` / results_table$Total) * 100,
        0
      )
      
      # Keep a numeric version if needed later
      results_table_num <- results_table
      
      # Format numbers for presentation/export
      results_table$Total <- format(
        results_table$Total,
        big.mark = " ",
        scientific = FALSE,
        trim = TRUE
      )
      
      results_table$`Priority sp` <- format(
        results_table$`Priority sp`,
        big.mark = " ",
        scientific = FALSE,
        trim = TRUE
      )
      
      results_table$Prop <- format(
        results_table$Prop,
        big.mark = " ",
        scientific = FALSE,
        trim = TRUE
      )
      
      
      
      
    wb <- createWorkbook()
    addWorksheet(wb, "Summary")
    writeData(wb, "Summary", results_table)
    
    numStyle <- createStyle(numFmt = "#,##0")
    
    # Apply formatting to all data rows (excluding header)
    addStyle(
      wb,
      sheet = "Summary",
      style = numStyle,
      rows = 2:(nrow(results_table) + 1),  # all rows except header
      cols = 2,                            # assuming Value is column 2
      gridExpand = TRUE
    )
    
    saveWorkbook(wb, file.path(folder_output,"summary_tracks_curtailments.xlsx"), overwrite = TRUE)

