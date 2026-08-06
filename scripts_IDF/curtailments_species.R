##
## Species-specific curtailment ----
##


   ### Curtails: priority species ----

        
        curtails_priority <-
          curtl_dt %>%
          filter(species %in% prioritysp) %>%
          count(species) %>%
          mutate(per = n/sum(n) * 100) %>%
          arrange(species)
        
        
        writexl::write_xlsx(curtails_priority, 
                            file.path(folder_output, paste0('curtls_prioritysp.xlsx')) )
        

  ### Curtails: non-priority species ----
        
        
        curtails_nonpriority <-
          curtl_dt %>%
          filter(species %in% nonprioritysp) %>%
          count(species) %>%
          mutate(per = n/sum(n) * 100) %>%
          arrange(species)
        
        writexl::write_xlsx(curtails_nonpriority, 
                            file.path(folder_output, paste0('curtls_nonprioritysp.xlsx')) )


  ### Curtails: other species ----

        
        curtails_others <-
          curtl_dt %>%
          filter(species %in% othersp) %>%
          count(species) %>%
          mutate(per = n/sum(n) * 100) %>%
          arrange(species)
        
        writexl::write_xlsx(curtails_others, 
                            file.path(folder_output, paste0('curtls_others.xlsx')) )
