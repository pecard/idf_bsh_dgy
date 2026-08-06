##
## Shutdown time (<1rpm) ----
##
    

    
    ### Prop of critical curtails ----
    time_to_curtl %>%
      count(status) %>%  
      mutate(prop = prop.table(n))
    
    # time_to_curtl |> 
    #   filter(species %in% prioritysp, !is.na(status)) |> 
    #   count(status) |> 
    #   mutate(prop = n / sum(n))
    
    writexl::write_xlsx(time_to_curtl, path = file.path(folder_output, 
                        paste0('shutdown_time_',date(scada_ini),'to',date(scada_end),'.xlsx')) )
    
    
    ### Summary statistics ----
    stat <- 
      time_to_curtl %>% 
      filter(time > 0) %>% 
      group_by(turb) %>% 
      summarise(m = round(mean(time, na.rm = T), 1),
                sd = round(sd(time, na.rm = T), 1),
                min = min(time, na.rm = T),
                max = max(time, na.rm = T))
    
    writexl::write_xlsx(stat, path = file.path(folder_output, 
                        paste0('shutdown_time_summary_',date(scada_ini),'to',date(scada_end),'.xlsx')) )
    
    
    p <- ggplot(time_to_curtl, aes(x = time)) +
      geom_histogram(
        binwidth = 5,
        boundary = 0,        # forces bins like 0–5, 5–10, ...
        fill = "steelblue",
        color = "black"
      ) +
      scale_x_continuous(
        limits = c(0, max(time_to_curtl$time)),  # adjust to your max range
        breaks = seq(0, max(time_to_curtl$time), 5)
      ) +
      labs(
        title = "Frequency distribution of Shutdown time",
        x = "Time (in seconds)",
        y = "Number of curtailments"
      ) +
      theme_minimal()
    
    ggsave(file.path(folder_output,paste0("shutdown_time_dist_",date(scada_ini),'to',date(scada_end),".png")), 
           plot = p, width = 8, height = 5, dpi = 300,bg = "white")
    
    
    
    ### Plot per classes ----
    time_to_curtl$time_cat <- cut(
      time_to_curtl$time,
      breaks = c(0, 5, 15, 30, 60, Inf),
      labels = c(
        "Very short",
        "Short",
        "Moderate",
        "Long",
        "Very long"
      ),
      include.lowest = TRUE
    )
    
    #force levels
    time_to_curtl$time_cat <- factor(
      time_to_curtl$time_cat,
      levels = c(
        "Very short",
        "Short",
        "Moderate",
        "Long",
        "Very long"
      )
    )
  
    p <- ggplot(time_to_curtl, aes(x = time_cat)) +
      geom_bar(fill = "steelblue", color = "black") +
      scale_x_discrete(drop = FALSE) +   # keeps empty categories
      labs(
        title = "Distribution of Shutdown time",
        x = "Time category",
        y = "Count"
      ) +
      theme_bw(base_size = 18)
    
    ggsave(file.path(folder_output,paste0("shutdown_time_categ_",date(scada_ini),'to',date(scada_end),".png")),
           plot = p, width = 8, height = 5, dpi = 300,bg = "white")
    
    
    #Summary table
    curtail_summary <- time_to_curtl %>%
      count(time_cat, .drop = FALSE) %>%
      mutate(
        percentage = round(100 * n / sum(n), 2)
      )
    
    writexl::write_xlsx(curtail_summary, path = file.path(folder_output, 
                        paste0('shutdown_time_class_summary_',date(scada_ini),'to',date(scada_end),'.xlsx')) )
    
    
    
    