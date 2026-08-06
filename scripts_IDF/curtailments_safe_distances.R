##
## Safe distances ----
##



    ###All species pooled ----
    
    p<-ggplot(
      time_to_curtl %>% filter(
        !is.na(minDist),
        species %in% prioritysp),
      aes(x = minDist)
    ) +
      geom_histogram(
        binwidth = 50,
        boundary = 0,
        colour = "grey"
      ) +
      labs(
        x = "Calculated theoretical safe distances (m)",
        y = "Count"
      ) +
      #geom_vline(xintercept = c(600, 750, 1000), linetype = "dashed") +
      geom_vline(xintercept = c(600), linetype = "dashed") +
      theme_bw()
    
    ggsave(filename = file.path(folder_output,paste0("curtail_SafeDist_allPriority",date(scada_ini),'to',date(scada_end),".png")),
           plot = p, width = 6, height = 4, dpi = 300)
    
    
    
    ###Per Species ----
    
    p<-ggplot(
      time_to_curtl %>% filter(
        !is.na(minDist),
        species %in% prioritysp),
      aes(x = minDist)
    ) +
      geom_histogram(
        binwidth = 50,
        boundary = 0,
        colour = "grey"
      ) +
      labs(
        x = "Calculated theoretical safe distances (m)",
        y = "Count"
      ) +
      #geom_vline(xintercept = c(600, 750, 1000), linetype = "dashed") +
      geom_vline(xintercept = c(600), linetype = "dashed") +
      facet_wrap(~ species, ncol = 3) +
      theme_bw()
    
    ggsave(filename = file.path(folder_output,paste0("curtail_SafeDist_perPriority",date(scada_ini),'to',date(scada_end),".png")),
           plot = p, width = 8, height = 8, dpi = 300)
    
    remove(p)