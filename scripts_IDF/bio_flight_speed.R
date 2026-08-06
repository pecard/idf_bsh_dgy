##
## Flight speed per species ----
##

    mspeed <- 
      track_dt %>%
      filter(
              spec %in% prioritysp & 
              count > 2 & 
              !is.na(dist_m) &
              speed_ms > 1 &
              speed_ms < 100 &
              spec != 'Turbine-Blade') %>%
      group_by(spec) %>%
      summarise(
        mean = mean(speed_ms, na.rm = TRUE),
        sd = sd(speed_ms, na.rm = TRUE),
        min = min(speed_ms, na.rm = TRUE),
        max = max(speed_ms, na.rm = TRUE)
      ) %>%
      arrange(desc(mean))
    

    writexl::write_xlsx(mspeed, path = file.path(folder_output,  
                                       paste0('Flight_speed_sp-priority.xlsx')) )
    
    