##
## Flight height per species ----
##
    
    fheight <- 
      track_dt %>%
      filter(
        count > 3 &
          !is.na(dist_m) &
          speed_ms > 1 &
          speed_ms < 100 &
          spec %in% prioritysp) %>%
      group_by(spec) %>%
      summarise(
        mean = mean(height, na.rm = TRUE),
        sd = sd(height, na.rm = TRUE),
        min = min(height, na.rm = TRUE),
        max = max(height, na.rm = TRUE)
      ) %>%
      arrange(desc(mean))
    
    writexl::write_xlsx(fheight, path = file.path(folder_output,  
                                         paste0('Flight_height_sp.xlsx')) )
    
    
   
   