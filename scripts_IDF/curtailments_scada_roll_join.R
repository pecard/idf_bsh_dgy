##
## Join SCADA data and Curtailment data with a roll join
##
    
    # Roll join because SCADA and curtailment data have differente temporal resolutions
    # SCADA has 10-second interval data


    resl <- list()
    scada_crt_roll_all <- list()
    for(j in turbinas_teste){
      
      #debug
      #j <- turbinas_teste[1]
      
      scada_dtWTG <- 
        scada_dt[turbinelabel %like% j & 
                   between(datetime, scada_ini, scada_end)
        ][
          readingname == 'RPM'
        ][, `:=` (day = as.Date(datetime)
                  # ,h = hour(datetime)
                  # , hms = format(datetime, format = "%H:%M:%S")
                  # , dec = as.numeric(lapply(str_split(str_sub(datetime, 12), ":"),  
                  #                         function(x) sum(as.numeric(unlist(x)) * 
                  #                                           c(1,1/60, 1/3600))))
        )
        ] 
      scada_dtWTG[, RollDate := datetime]
      
      curtl_dtWTG <- 
        curtl_dt %>% 
        filter(turbine %like% j & 
                 between(start, scada_ini, scada_end)) %>%
        arrange(start) %>%
        select(turbine, track_id, species, x2d, start, end) %>%
        pivot_longer(cols = start:end, names_to = 'status', 
                     values_to = 'datetimeC') %>%
        group_by(status) %>%
        mutate(id = row_number())
      
      setDT(curtl_dtWTG)
      
      curtl_dtWTG[, `:=` 
                  (RollDate = datetimeC
                    # , day = as.Date(datetimeC)
                    # , h = hour(datetimeC)
                    # , dec = as.numeric(lapply(str_split(str_sub(datetimeC, 12), ":"),  
                    #                         function(x) sum(as.numeric(unlist(x)) * 
                    #                                           c(1,1/60, 1/3600))))
                  )
      ]
      
      
      scada_crt_roll <-
        scada_dtWTG[                        # SCADA DT 
          curtl_dtWTG,                      # Curtail DT
          on = .(RollDate),                 # Join on Datetime 
          roll = "nearest",                 # using nearest time to sec.
          `:=`(species = i.species,         # retain these cols
               track_id = i.track_id,
               status = i.status,
               id = i.id)
        ][, .(turbinelabel, datetime, 
              value, species,
              track_id, status, id)]
      
      setkey(scada_crt_roll, 'datetime', 'id')
      
      #'### Time to curtail (rpm < 1) ---------------------------------------------
      res <- tibble(id = unique(curtl_dtWTG$id), time = NA, turb = NA,
                    track_id = NA, x2d = NA, avgspeed = NA, minDist = NA)
      for(i in res$id) {
        index <- scada_crt_roll[id == i, which = TRUE]
        if(length(index) == 1 || 
           identical(index, integer(0)) ||
           any(scada_crt_roll[min(index):max(index), value > 1]) == F) {
          next
        } else {
          start_value <- scada_crt_roll$value[min(index)] # rpm at curtail order
          
          # deal with speed below rpm < 1 (turbine was already off)
          if(start_value < 1) {
            res$time[i] <- 0
            res$avgspeed[i] <- NaN
            res$turb[i] <- j
            next
          } else {
            id <- scada_crt_roll$track_id[min(index)] #take the smalest value in index
            sub_tracks <- track_dt[track_id == id]
            res$track_id[i] <- id
            res$x2d[i] <- sub_tracks$dist[1] #stores distance
            
            q <- as.numeric(quantile(sub_tracks$speed_ms, probs = 0.95, na.rm = T))
            speeddata <- sub_tracks %>% 
              filter(speed_ms < q) %>% 
              filter(speed_ms > 0) %>% 
              summarise(avgSpeed = mean(speed_ms, na.rm = TRUE), #calculates mean flight speed
                        medianSpeed = median(speed_ms, na.rm = TRUE))
            res$avgspeed[i] <- speeddata$avgSpeed
            
            res$time[i] <- 
              scada_crt_roll[min(index):max(index)
              ][, gr := j
              ][status %like% 'start' | value < 2, .I[1], c('datetime', 'value', 'gr')
              ][1:2, .(diff = as.numeric(difftime(max(datetime), 
                                                  min(datetime), 
                                                  units = "secs"))), 
                by = gr]$diff
            
            res$minDist[i] <- res$time[i] * res$avgspeed[i] #distance traveled by the bird --> traveled.distance = time.difference * bird.speed
          }
        }
        res$turb <- j
        scada_crt_roll$turb <- j
      }
      resl[[j]] <- res
      scada_crt_roll_all[[j]] <- scada_crt_roll
    } #END
    
    #analysed periods
    if(exists("analysed_periods_index")){analysed_periods_index <- analysed_periods_index +1} else {analysed_periods_index <- 1}   

    