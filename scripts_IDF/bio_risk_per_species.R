##
## Risk per species ----
##
  
    ###
    ### Priority species ----
    ###

    summary_priority <- 
      track_dt[spec %in% prioritysp, ][
        , .(records = .N, tracks = uniqueN(track_id)), by = spec
      ][
        # Juntar com os dados de risco de colisão
        track_dt[spec %in% prioritysp, ][
          , risk := cut(height, 
                        breaks = c(-Inf, riskHeight_lower, riskHeight_upper, +Inf),
                        labels = c("below", "at risk", "above"),
                        include.lowest = TRUE)
        ][
          , .(count = .N), by = .(spec, risk)
        ][
          , prop := count / sum(count) * 100, by = spec
        ],
        on = "spec"
      ][
        order(spec)
      ][
        risk == "at risk", # Filtrar apenas os voos na zona de risco
        .(spec, records, tracks, prop) # Manter apenas as colunas desejadas
      ]
    
    writexl::write_xlsx(summary_priority, 
                        path = file.path(folder_output,  
                                         paste0('TracksRisk_per_sp-priority.xlsx')) )
    
    
    #Stats per risk type: below, at risk, above
    setDT(track_dt)
    
    dt <- track_dt[spec %in% prioritysp]
    
    # Create risk category
    dt[, risk := cut(
      height,
      breaks = c(-Inf, riskHeight_lower, riskHeight_upper, Inf),
      labels = c("Below", "At risk", "Above"),
      include.lowest = TRUE
    )]
    
    # Count per risk
    risk_summary <- dt[, .(count = .N), by = risk][
      , prop := round(count / sum(count) * 100, 1)
    ]
    
    # Add totals row properly
    totals_row <- data.table(
      risk  = "Totals",
      count = sum(risk_summary$count),
      prop  = 100.0
    )
    risk_summary <- rbind(risk_summary, totals_row)
    # Order rows
    risk_summary[, risk := factor(risk,
                                  levels = c("At risk", "Above", "Below", "Total"))]
    setorder(risk_summary, risk)
    
    writexl::write_xlsx(risk_summary,
                        file.path(folder_output, "TracksRisk_summary-priority.xlsx"))
    
    
    ###
    ### Non-Priority species ----
    ###

      summary_nonpriority <- 
      track_dt[spec %in% nonprioritysp, ][
        , .(records = .N, tracks = uniqueN(track_id)), by = spec
      ][
        # Juntar com os dados de risco de colisão
        track_dt[spec %in% nonprioritysp, ][
          , risk := cut(height, 
                        breaks = c(-Inf, riskHeight_lower, riskHeight_upper, +Inf),
                        labels = c("below", "at risk", "above"),
                        include.lowest = TRUE)
        ][
          , .(count = .N), by = .(spec, risk)
        ][
          , prop := count / sum(count) * 100, by = spec
        ],
        on = "spec"
      ][
        order(spec)
      ][
        risk == "at risk", # Filtrar apenas os voos na zona de risco
        .(spec, records, tracks, prop) # Manter apenas as colunas desejadas
      ]
    
    writexl::write_xlsx(summary_nonpriority, 
                        path = file.path(folder_output,  
                                         paste0('TracksRisk_per_sp-nonpriority.xlsx')) )
    
    
    ###
    ### Other species ----
    ###
    
    summary_other <- 
      track_dt[spec %in% othersp, ][
        , .(records = .N, tracks = uniqueN(track_id)), by = spec
      ][
        # Juntar com os dados de risco de colisão
        track_dt[spec %in% othersp, ][
          , risk := cut(height, 
                        breaks = c(-Inf, riskHeight_lower, riskHeight_upper, +Inf),
                        labels = c("below", "at risk", "above"),
                        include.lowest = TRUE)
        ][
          , .(count = .N), by = .(spec, risk)
        ][
          , prop := count / sum(count) * 100, by = spec
        ],
        on = "spec"
      ][
        order(spec)
      ][
        risk == "at risk", # Filtrar apenas os voos na zona de risco
        .(spec, records, tracks, prop) # Manter apenas as colunas desejadas
      ]
    
    writexl::write_xlsx(summary_other, 
                        path = file.path(folder_output,  
                                         paste0('TracksRisk_per_sp-other.xlsx')) )
