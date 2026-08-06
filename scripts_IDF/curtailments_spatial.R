##
## Spatial curtailments ----
##

    ### Curtail: totals and per WTG ----
    
    # Date Range and duration
    curtl_dt %>% 
      summarise(tmin = min(start),
                tmax = max(end),
                msec = mean(duration),
                mdur = hms::as_hms(mean(duration)))
    
    
    ### Curtails: per WTG ----
    
    
    curtails_per_wtg <-
      data.table(curtl_dt)[ , .(count = .N), by = turbine
      ][, rel_freq := count / sum(count) * 100
      ][order(-rel_freq)][]
    
    
    
    # Spatial Visualization
    p <-
      wtg %>%
      left_join(
        curtl_dt %>%
          count(turbine)
        , by = c('ID' = 'turbine')) %>%
      mutate(n = ifelse(is.na(n),0,n)) %>%
      ggplot() +
      geom_sf(aes(size = n), colour = 'darkgrey') +
      geom_sf_text(aes(label = ID), colour = 'darkred', size = 2,
                   nudge_y = 200)
    
    ggsave(file.path(folder_output, paste0('curtails_all_spatial.png')), 
           plot=p, width = 8, height = 6, dpi = 300, bg='white')
 
    
    
    # Spatial Visualization - colored (heat map style)
    p <-
      wtg %>%
      left_join(
        curtl_dt %>% count(turbine),
        by = c("ID" = "turbine")
      ) %>%
      mutate(n = ifelse(is.na(n), 0, n),
             n_plot = ifelse(n == 0, NA, n)) %>%   # zeros become NA for colouring
      ggplot() +
      geom_sf(aes(size = n, colour = n_plot)) +
      geom_sf_text(
        aes(label = ID),
        colour = "darkred",
        size = 2,
        nudge_y = 200
      ) +
      scale_colour_gradientn(
        colours = c("yellow", "orange", "red"),
        na.value = "grey70",              # gery for zero curtailment
        name = "Total Curtailment"
      ) +
      scale_size(range = c(1, 8), name = "Total Curtailment") +
      theme_minimal()
    
    ggsave(file.path(folder_output, paste0('curtails_all_spatial(colored).png')), 
           plot=p, width = 8, height = 6, dpi = 300, bg='white')
    
   
  remove(p)
