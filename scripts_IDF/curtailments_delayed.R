##
##  Delayed curtailments ----
##

    ### Proportion of curtailments exceeding and falling short safe distances ----

    ### All species----
    p <-
      time_to_curtl |> 
      filter(species %in% prioritysp, !is.na(status)) |> 
      ggplot() +
      geom_histogram(aes(diffDist, fill = col)) +
      scale_x_continuous(limits = c(-1000, 1000)) +
      labs(x = 'Distance from start order to turbine (m)',
           y = 'Frequency', 
           title = '') +
      theme_bw() +
      theme(legend.position = 'none')
    
    ggsave(file.path(folder_output,
                  paste0('curtail_Falling&Exceed_allPriority_',date(scada_ini),'to',date(scada_end),'.png')),
           plot = p, width = 6, height = 4, dpi = 300)
    

    ### Per species ----
    p <- 
      time_to_curtl |> 
      filter(species %in% prioritysp, !is.na(status)) |> 
      ggplot() +
      geom_histogram(aes(diffDist, fill = col)) +
      scale_x_continuous(limits = c(-1000, 1000)) +
      labs(x = 'Distance from start order to turbine (m)',
           y = 'Frequency',
           title = 'Proportion of curtailments exceeding or falling short in safe distance') +
      theme_bw()+
      theme(legend.position = 'none') +
      facet_wrap(~species, ncol=3)
    
    ggsave(file.path(folder_output, 
                  paste0('curtail_Falling&Exceed_perPriority_',date(scada_ini),'to',date(scada_end),'.png')),
        plot = p, width = 8, height = 8, dpi = 300)
    
    
    ### Prop critical dist ----
    Tab <- 
      time_to_curtl %>% 
      filter(species %in% prioritysp, !is.na(status)) |> 
      group_by(species) %>% 
      summarise(N = n(),
                crit = length(which(diffDist < 0))) %>% 
      mutate(Critical = round(crit / N * 100, 2)*(-1),
             OK = 100 - round(crit / N * 100, 2)) %>% 
      select(-c(crit)) %>% 
      gather(key = 'Status', value = 'Prop', -c(species, N))
    
    labels <- Tab %>% 
      filter(Status == 'OK') %>% 
      arrange(Prop) 
    
    Tab$species <- factor(Tab$species, levels = labels$species)
    
    Tab$Status <- ifelse(Tab$Status == 'Critical', 'Fall short', 'Exceeded')
    Tab$Status <- factor(Tab$Status, levels = c('Fall short', 'Exceeded'))
    
    
    ### Prop curtail exceed or falling ----    

    p<-ggplot(Tab, aes(x = species, y = Prop, fill = Status)) +
      geom_bar(stat = "identity") + 
      coord_flip()+ 
      scale_y_continuous(limits = c(-100, 100),
                         breaks = seq(-100, 100, 25),
                         labels = abs(seq(-100, 100, 25))) + 
      labs(title = 'Proportion of curtailments exceeding or falling short in safe distance',
           x = "", 
           y = "Frequency (%)") +
      theme_bw() +
      theme(axis.text.y = element_text(size = 12))
    
    ggsave(filename = file.path(folder_output, paste0('curtail_PropCrit_distance_allPriority_',date(scada_ini),'to',date(scada_end),'.png')),
           plot = p, width = 10, height = 6, dpi = 300)
    
    
    Tab2 <- Tab %>% spread(Status, Prop)
    
    writexl::write_xlsx(Tab2, path = file.path(folder_output,  
                         paste0('curtail_PropCrit_distance_allPriority_',date(scada_ini),'to',date(scada_end),'.xlsx')) )

    
    #remove 
    remove(Tab, Tab2, p, labels)
    
    
    