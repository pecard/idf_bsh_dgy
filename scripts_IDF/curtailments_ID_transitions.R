## 
## Curtailments due to ID transitions
##


    # Tracks with multiple IDs
    tracks_multi <- transitions_dt %>%
      filter(n > 1)
    nrow(tracks_multi)
    
    ##
    ## Unique tracks ----
    ##
    
    # Unique multi-ID track IDs
    multi_ids <- tracks_multi %>%
      distinct(track_id)
    
    # Unique tracks that triggered curtailments
    multi_tracks_with_curt <- multi_ids %>%
      inner_join(curtl_dt %>% distinct(track_id), by = "track_id")
    nrow(multi_tracks_with_curt)
    
    # Summary table
    summary_multi_tracks <- data.frame(
      total_multi_tracks = nrow(multi_ids),
      multi_tracks_with_curt = nrow(multi_tracks_with_curt),
      proportion = nrow(multi_tracks_with_curt) / nrow(multi_ids),
      percent = 100 * nrow(multi_tracks_with_curt) / nrow(multi_ids)
    )
    summary_multi_tracks
    
    #Species IDs for each tracks collapsed in one column separated by ,
    tracks_species <- track_dt %>%
      filter(track_id %in% multi_tracks_with_curt$track_id) %>%
      group_by(track_id) %>%
      summarise(
        species = paste(unique(na.omit(spec)), collapse = ", "),
        .groups = "drop"
      )
    
    nrow(tracks_species)
    
    #Extract last species ID
    tracks_nonpriority_last <- tracks_species %>%
      mutate(
        last_species = sapply(str_split(species, ",\\s*"), tail, 1)
      ) %>%
      filter(!(last_species %in% prioritysp))
    
    #which are non-priority and add - TRUE
    tracks_species <- tracks_species %>%
      mutate(
        last_species = sapply(str_split(species, ",\\s*"), tail, 1),
        last_nonpriority = !(last_species %in% prioritysp)
      )
    write_xlsx(tracks_species, file.path(folder_output,"ID_transition_curt_specIDs.xlsx"))
    
    #General totals for context
    
    uniqueN(track_dt$track_id) #total of unique tracks
    total_unique_tracks_curtail <- curtl_dt %>% summarise(total_tracks_with_curtailment = n_distinct(track_id)) #total tracks that triggered curtailments
    
    #summary statistics
    summary_transition_tracks <- data.frame(
      total_unique_tracks = uniqueN(track_dt$track_id),
      total_unique_tracks_with_curtail = total_unique_tracks_curtail$total_tracks_with_curtailment,
      total_tracks_multi_ID = nrow(tracks_multi),
      total_tracks_multi_ID_curtail = nrow(multi_tracks_with_curt),
      nonpriority_last = sum(tracks_species$last_nonpriority, na.rm = TRUE),
      percent_nonpriority_last = 100 * sum(tracks_species$last_nonpriority, na.rm = TRUE) / nrow(multi_tracks_with_curt)
    )
    write_xlsx(summary_transition_tracks, file.path(folder_output,"ID_transition_curt(tracks)_P-NP_summary.xlsx"))
    
    
    ##
    ## Curtailments (same track can trigger multiple curtailments) ----
    ##
    
    #total curtailments from ID transitions
    curtl_transition <- curtl_dt[
      curtl_dt$track_id %in% multi_tracks_with_curt$track_id,
    ]
    
    #total curtailments from P-->NP ID
    curtl_transition_nonpriority_last <- curtl_dt[
      curtl_dt$track_id %in% tracks_nonpriority_last$track_id,
    ]
    
    
    summary_transition_curt <- data.frame(
      total_curtailments = nrow(curtl_dt),
      curtailments_due_to_transitions = nrow(curtl_transition),
      curtailments_due_to_PNP_transition = nrow(curtl_transition_nonpriority_last),
      percent = 100 * nrow(curtl_transition_nonpriority_last) / nrow(curtl_dt)
    )
    write_xlsx(summary_transition_curt, file.path(folder_output,"ID_transition_curt_P-NP_summary.xlsx"))
    
    
    
    