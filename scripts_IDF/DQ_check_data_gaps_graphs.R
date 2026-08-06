##
## DQ check ----
##

  ### SCADA data ----

    scada_dt_unfilt1 <- scada_dt_unfilt %>%
      mutate(
        datetime = ymd_hms(datetime, tz = "UTC"),   # <-- use START column here
        date     = as.Date(datetime),
        time_num = hour(datetime) +
          minute(datetime) / 60 +
          second(datetime) / 3600
      ) %>%
      arrange(datetime)
    
    ##Temporal density map to check data gap
    
    #All turbines
    p <- ggplot(scada_dt_unfilt1, aes(x = date, y = time_num)) +
      geom_bin2d(bins = 200) +
      scale_fill_viridis_c(direction = -1) +   # reverses counts scale
      scale_y_continuous(
        name = "Time of day (hour)",
        breaks = seq(0, 24, by = 2),
        limits = c(0, 24)
      ) +
      scale_x_date(
        date_breaks = "1 month",     # first day of each month
        date_labels = "%Y-%m"        # format
      ) +
      labs(
        x = "Date",
        title = "SCADA data - Temporal density map (gaps appear as empty bins)"
      ) +
      theme_minimal()
    
    ggsave(
      filename = file.path(folder_output_DC,
                           paste0("data-SCADADB_temporal_map_", "ALL-T", ".png")),
      plot = p, width = 10, height = 6, dpi = 300, bg = "white"
    )
    
    #Per turbine
    dir.create(file.path(folder_output_DC, "data-SCADADB_perT"), recursive = TRUE)
    turbines <- unique(scada_dt_unfilt1$turbinelabel)
    for (t in turbines) {
      
      df_sub <- scada_dt_unfilt1 %>% filter(turbinelabel == t)
      
      p <- ggplot(df_sub, aes(x = date, y = time_num)) +
        geom_bin2d(bins = 200) +
        scale_fill_viridis_c(direction = -1) +   # reverses counts scale
        scale_y_reverse(
          name = "Time of day (hour)",
          breaks = seq(0, 24, by = 2),
          limits = c(24, 0)
        ) +
        scale_x_date(
          date_breaks = "1 month",
          date_labels = "%Y-%m"
        ) +
        labs(
          x = "Date",
          title = paste("Curtail data - Temporal density map - Turbine", t)
        ) +
        theme_bw() +
        theme(
          plot.background  = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA)
        )
      
      ggsave(
        filename = file.path(folder_output_DC,"data-SCADADB_perT",
                             paste0("data-SCADADB_temporal_map_", t, ".png")),
        plot = p, width = 10, height = 6, dpi = 300, bg = "white"
      )
    }



  ### Curtail data ----

    curtl_dt_unfilt1 <- curtl_dt_unfilt %>%
      mutate(
        datetime = ymd_hms(start, tz = "UTC"),   # <-- use START column here
        date     = as.Date(datetime),
        time_num = hour(datetime) +
          minute(datetime) / 60 +
          second(datetime) / 3600
      ) %>%
      arrange(datetime)
    
    ##Temporal density map to check data gap
    
    #All turbines
    p <- ggplot(curtl_dt_unfilt1, aes(x = date, y = time_num)) +
      geom_bin2d(bins = 200) +
      scale_fill_viridis_c(direction = -1) +   # reverses counts scale
      scale_y_continuous(
        name = "Time of day (hour)",
        breaks = seq(0, 24, by = 2),
        limits = c(0, 24)
      ) +
      scale_x_date(
        date_breaks = "1 month",     # first day of each month
        date_labels = "%Y-%m"        # format
      ) +
      labs(
        x = "Date",
        title = "Curtail data - Temporal density map (gaps appear as empty bins)"
      ) +
      theme_minimal()
    
    ggsave(
      filename = file.path(folder_output_DC,
                           paste0("data-CurtailDB_temporal_map_", "ALL-T", ".png")),
      plot = p, width = 10, height = 6, dpi = 300, bg = "white"
    )
    
    #Per turbine
    dir.create(file.path(folder_output_DC, "data-CurtailDB_perT"), recursive = TRUE)
    turbines <- unique(curtl_dt_unfilt1$turbine)
    for (t in turbines) {
      
      df_sub <- curtl_dt_unfilt1 %>% filter(turbine == t)
      
      p <- ggplot(df_sub, aes(x = date, y = time_num)) +
        geom_bin2d(bins = 200) +
        scale_fill_viridis_c(direction = -1) +   # reverses counts scale
        scale_y_reverse(
          name = "Time of day (hour)",
          breaks = seq(0, 24, by = 2),
          limits = c(24, 0)
        ) +
        scale_x_date(
          date_breaks = "1 month",
          date_labels = "%Y-%m"
        ) +
        labs(
          x = "Date",
          title = paste("Curtail data - Temporal density map - Turbine", t)
        ) +
        theme_bw() +
        theme(
          plot.background  = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA)
        )
      
      ggsave(
        filename = file.path(folder_output_DC,"data-CurtailDB_perT",
                             paste0("data-TracksDB_temporal_map_", t, ".png")),
        plot = p, width = 10, height = 6, dpi = 300, bg = "white"
      )
    }

    

### Tracks data ----

    track_dt_unfilt1 <- track_dt_unfilt %>%
      mutate(
        datetime = ymd_hms(timestamp, tz = "UTC"),     # change tz if needed
        date     = as.Date(datetime),
        time_num = hour(datetime) +
          minute(datetime) / 60 +
          second(datetime) / 3600
      ) %>%
      arrange(datetime)
    
    ##Temporal density map to check data gap
    
    #All turbines
    p <- ggplot(track_dt_unfilt1, aes(x = date, y = time_num)) +
      geom_bin2d(bins = 200) +
      scale_fill_viridis_c(direction = -1) +   # reverses counts scale
      scale_y_continuous(
        name = "Time of day (hour)",
        breaks = seq(0, 24, by = 2),
        limits = c(0, 24)
      ) +
      scale_x_date(
        date_breaks = "1 month",     # first day of each month
        date_labels = "%Y-%m"        # format
      ) +
      labs(
        x = "Date",
        title = "Tracks data - Temporal density map (gaps appear as empty bins)"
      ) +
      theme_minimal()
    
    ggsave(file.path(folder_output_DC,"data-TracksDB_temporal_map_ALL-T.png"), plot = p,
           width = 10, height = 6, dpi = 300,bg = "white")
    
    #Per turbine
    dir.create(file.path(folder_output_DC, "data-TracksDB_perT"), recursive = TRUE)
    turbines <- unique(track_dt_unfilt1$turbine)
    for (t in turbines) {
      
      df_sub <- track_dt_unfilt1 %>% filter(turbine == t)
      
      p <- ggplot(df_sub, aes(x = date, y = time_num)) +
        geom_bin2d(bins = 200) +
        scale_fill_viridis_c(direction = -1) +   # reverses counts scale
        scale_y_reverse(
          name = "Time of day (hour)",
          breaks = seq(0, 24, by = 2),
          limits = c(24, 0)
        ) +
        scale_x_date(
          date_breaks = "1 month",
          date_labels = "%Y-%m"
        ) +
        labs(
          x = "Date",
          title = paste("Tracks data - Temporal density map - Turbine", t)
        ) +
        theme_bw() +
        theme(
          plot.background  = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA)
        )
      
      ggsave(
        filename = file.path(folder_output_DC,"data-TracksDB_perT",
                             paste0("data-TracksDB_temporal_map_", t, ".png")),
        plot = p, width = 10, height = 6, dpi = 300, bg = "white"
      )
    }

