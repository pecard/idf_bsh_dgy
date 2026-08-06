##
## Protect time ----
##
    
    calendar <-
      setDT(data.frame(date = seq(as.Date(ini), 
                                  as.Date(end), 
                                  by=1)))
    
    calendarmin <-
      setDT(data.frame(date = seq(as.POSIXct(ini), 
                                  as.POSIXct(end), 
                                  by="min")))
    
    # Sunlight hours
    sun <- suncalc::getSunlightTimes(date = calendar$date, 
                                     lat = proj_lat, lon = proj_lon, 
                                     tz = proj_timezone, 
                                     keep = c("sunrise","sunset"))
    
    sun$dayl <- as.numeric(difftime(sun$sunset, sun$sunrise, 'hours'))
    
    # Total daylight hours for the reporting period
    cat("\nHoras totais de luz no período:", round(sum(sun$dayl), 2), "h\n")
    
    track_calendar <-
      track_dt[, date := as.Date(timestamp )][, .(tracks = .N), by = c('date')]
    
    idf_calendar <-
      track_dt[, `:=`(
        date = as.Date(timestamp),
        year = format(as.Date(timestamp), "%Y")
      )
      ][, .(idfs = uniqueN(idf)), by = .(date, year)   # keep date for heatmap
      ][, vol := (idfs * (1000^2 * 1000 * pi)) / 1000000
      ]
    
    nrow(na.omit(idf_calendar)) # monitoring days
    

    ### Calendar with number IDFs  ----
    p <-
      ggTimeSeries::ggplot_calendar_heatmap(
        idf_calendar,
        'date',
        'idfs') +
      facet_wrap(~ year(date), ncol = 1) +   # <-- stack vertically
      xlab('') + 
      ylab('') + 
      scale_fill_distiller("IDF Units", palette = 'Spectral')
    
    ggsave(p, filename = file.path(folder_output, paste0('Calendar-NumIDFs_data_',date(ini),"to",date(end),'.png')),
           width = 150, height = 100, units = "mm", dpi = 300)  
    
    
  
    
    ### Calendar with mean tracks ----
        tracksummary <- 
      calendar[track_calendar, on = 'date', tracks := i.tracks][
        idf_calendar, on = 'date', idfs := i.idfs][, mean := tracks/idfs]
    
    setnafill(tracksummary, cols = c('idfs', 'tracks', 'mean'), fill = 0)
    
    tracksummary[]
    
    writexl::write_xlsx(tracksummary, 
                        path = file.path(folder_output,  
                                         paste0('NumIDFs&Tracks_per_date_',date(ini),'to',date(end),'.xlsx')) )
    
    
    p <- 
      ggTimeSeries::ggplot_calendar_heatmap(
        tracksummary,
        'date',
        'mean') + 
      facet_wrap(~ year(date), ncol = 1) +   # <-- stack vertically
      xlab('') + 
      ylab('') + 
      scale_fill_distiller("Mean N. tracks/\nIDF Unit", palette = 'Spectral')
    
    
    ggsave(p, filename = file.path(folder_output, paste0('Calendar-Meantracks_per_IDF_',date(ini),"to",date(end),'.png')),
           width = 16, height = 6, units = "cm", dpi = 300)
    
    
    
    ### Table of total tracks per IDF per Date ----
    
    
    ##### HARD CODED #####
    df <- data.frame(
      period = rep(c("September 2024 (61 WTGs)",
                     "October 2024 (76 WTGs)",
                     "August 2025 (76 WTGs)"), each = 5),
      class = rep(c("100%", ">96%", ">88%", ">75%", ">60%"), times = 3),
      pct = c(
        19.7, 54.1, 16.4, 4.9, 4.9,   # Sep 2024
        31.6, 47.4, 15.8, 3.9, 1.3,   # Oct 2024
        0.0,  64.4, 28.8, 6.8, 6.8    # Aug 2025
      )
    )
    
    # Optional: keep periods ordered left-to-right
    df$period <- factor(df$period, levels = c("September 2024 (61 WTGs)",
                                              "October 2024 (76 WTGs)",
                                              "August 2025 (76 WTGs)"))    
    ##### FIM HARD CODED #####
    
    
    # Ensure stack order matches the figure (bottom -> top)
    df$class <- factor(df$class, levels = c("100%", ">96%", ">88%", ">75%", ">60%"))

    # Color palette approximating the image
    cols <- c(
      "100%" = "#3F6B4E",  # dark green
      ">96%" = "#7C915E",  # olive green
      ">88%" = "#EFE3A6",  # pale yellow
      ">75%" = "#B33A3A",  # red
      ">60%" = "#5B0000"   # dark maroon
    )
    
    p <- ggplot(df, aes(x = period, y = pct, fill = class)) +
      geom_col(width = 0.65,
               position = position_stack(reverse = TRUE)) +   
      scale_y_continuous(limits = c(0, 100),
                         breaks = seq(0, 100, 20)) +
      scale_fill_manual(values = cols) +
      labs(
        x = NULL,
        y = "Percentage of WTGs",
        fill = "Availability Class"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        panel.grid.minor = element_blank()
      )
    
    ggsave(p, filename = file.path(folder_output,"idf_availability.png"),
      width = 10, height = 6, dpi = 300
    )
    
    