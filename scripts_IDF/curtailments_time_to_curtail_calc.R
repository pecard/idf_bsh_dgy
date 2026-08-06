##
## Calculate time to curtailment
##

    
    time_to_curtl <-
      na.omit(rbindlist(resl)) %>%
      left_join(curtl_dt %>% 
                  filter(!duplicated(track_id)) %>%
                  select(species, track_id), 
                by = 'track_id') %>% #turn one table with only one row per track_id
      mutate(diffDist = x2d - minDist, 
             # x2d = actual bird distance from turbine at the moment the curtailment was triggered;
             # minDist = distance travelled by the bird travelling at avg speed recorded from the bird track, during 
             #           the time between curtail trigger and rotor reaching rpm <2 = "SAFE DISTANCE"
             # diffDist = remaning marginal distance
            
             status = case_when(diffDist > 0 ~ 'OK', # OK if bird is at distance > safe distance 
                                .default = 'Crit'),  # Crit if safe dist > distance of the bird to the turbine
             col = case_when(diffDist > 0 ~ 'tomato', 
                             .default = 'lightblue')
      ) %>% filter(diffDist > -1000) 