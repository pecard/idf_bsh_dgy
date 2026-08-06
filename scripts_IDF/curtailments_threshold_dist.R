##
## Trigger distance threshold ----
##


  
  ###All species ----
  # 50m bins
  
  p<-ggplot(
    time_to_curtl %>% filter(
      !is.na(x2d), 
      !is.na(status),
      species %in% prioritysp),
    aes(x = x2d, fill = status)
  ) +
    geom_histogram(
      binwidth = 50,
      boundary = 0,
      position = "dodge",
      colour = "white"
    ) +
    scale_fill_manual(
      values = c("OK" = "#17aeb0", "Crit" = "#ef6a5b"),
      labels = c(
        "OK" = "Safe",
        "Crit" = "Delayed"
      )
    ) +
    labs(
      x = "Distance to turbine at curtailment trigger (m)",
      y = "Count",
      fill = "Curtailment",
    ) +
    #geom_vline(xintercept = c(600, 750, 1000), linetype = "dashed") +
    geom_vline(xintercept = c(600), linetype = "dashed") +
    theme_bw()
    
  ggsave(filename = file.path(folder_output,paste0("curtail_Delayed&Safe_allPriority",date(scada_ini),'to',date(scada_end),".png")),
         plot = p, width = 6, height = 4, dpi = 300)
  
  
  
  ###Per species ----
  p<-ggplot(
    time_to_curtl %>% filter(
      !is.na(x2d), 
      !is.na(status),
      species %in% prioritysp),
    aes(x = x2d, fill = status)
  ) +
    geom_histogram(
      binwidth = 50,
      boundary = 0,
      position = "dodge",
      colour = "white"
    ) +
    scale_fill_manual(
      values = c("OK" = "#17aeb0", "Crit" = "#ef6a5b"),
      labels = c(
        "OK" = "Safe",
        "Crit" = "Delayed"
      )
    ) +
    labs(
      x = "Distance to turbine at curtailment trigger (m)",
      y = "Count",
      fill = "Curtailment",
    ) +
    #geom_vline(xintercept = c(600, 750, 1000), linetype = "dashed") +
    geom_vline(xintercept = c(600), linetype = "dashed") +
    facet_wrap(~ species, ncol = 3) +
    theme_bw()
  
  ggsave(filename = file.path(folder_output,paste0("curtail_Delayed&Safe_perPriority_",date(scada_ini),'to',date(scada_end),".png")),
         plot = p, width = 8, height = 8, dpi = 300)
  
  
 remove(p)
  
  