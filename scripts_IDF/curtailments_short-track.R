##
## Short-track curtailment ----
##
    
   
    
    tracldt_unique_spec <- unique(track_dt, by = c("track_id", "spec"))
    
    curtl_short_track <-
      select(curtl_dt, turbine, species, start, track_id, x2d) %>%
      left_join(select(unique(track_dt, by = c("track_id", "spec")), 
                       track_id, timestamp, count, spec), 
                by = 'track_id', relationship = "many-to-many") %>%
      mutate(match = case_when(
        species == spec ~ 1,
        .default = 0))
    
    curtl_short_track %>% filter(is.na(timestamp))
    
    
    # Total short tracks (< 6 points)
    total_short_tracks <- nrow(track_dt[count < shorttrack_min_points])
    
    # Unique short-track curtailments at close range (< 300m)
    short_track_close <- unique(
      data.table(curtl_short_track),
      by = c("track_id", "turbine")
    ) %>%
      filter(
        count < shorttrack_min_points,
        x2d < shorttrack_eval_range
      )
    
    n_short_track_close <- nrow(short_track_close)
    
    # Percentage relative to total curtailments
    pct_short_track_close <- 
      n_short_track_close / nrow(curtl_dt) * 100
    
    
    results_table <- data.frame(
      Metric = c(
        "Total short tracks (<6 points)",
        "Short-track curtailments (<300m)",
        "Short-track curtailments (%) of total curtailments"
      ),
      Value = c(
        total_short_tracks,
        n_short_track_close,
        round(pct_short_track_close, 2)
      )
    )
    
    results_table
    writexl::write_xlsx(results_table, 
                        path = file.path(folder_output, 
                                         paste0('short-track_curtails_prop.xlsx')) )
    
    ### Priority species short-tracks that triggered curtailment ----
    curtl_short_track_priority <-
      unique(
        data.table(
          curtl_short_track)[count < shorttrack_min_points & x2d < shorttrack_eval_range], 
        by = c("track_id", "turbine"))[species %in% prioritysp
        ][ , .(count = .N), by = species
        ][, rel_freq := count / sum(count)  *100][] %>% adorn_totals()
    
    writexl::write_xlsx(curtl_short_track_priority, 
                        path = file.path(folder_output, 
                                         paste0('short-track_curtails_per_prioritysp.xlsx')) )
    
    