##
## Read heartbeat files ----
##
    
    heartb_files <- tibble(file = list.files(databases_dir, pattern = 'Heartbeats_.+csv', full.names = T))
    heartb_files$date <- ymd(str_extract(heartb_files$file, "(?<=Heartbeats_).+?(?=_)"))
    
    # Files to read from specific date
    #heartb_files_toread <- filter(heartb_files, date >= ini & date <= end)$file # using "ini"
    heartb_files_toread <- filter(heartb_files)$file 
    
    if (length(heartb_files_toread) > 0){
          lhb <- lapply(heartb_files_toread, 
                        fread, 
                        sep =",", 
                        header = T, 
                        na.strings = "NULL", 
                        stringsAsFactors = F, 
                        #skip = ini, 
                        blank.lines.skip = T)
          
          heartb_dt_unfilt <- rbindlist( lhb ) 
          
          # Unique IDF
          sort(unique(heartb_dt_unfilt$InstanceName))
          
          setnames(heartb_dt_unfilt, names(clean_names(heartb_dt_unfilt)))
          
          setnames(heartb_dt_unfilt, 
                   old = 
                     c(
                       "instance_name", 
                       'minutes_between_heartbeats'
                     ), 
                   new = 
                     c(
                       "idf",
                       'min_hb'
                     ))
          names(heartb_dt_unfilt)
          
          dropcols <- c("timestamp_utc", "previous_timestamp_utc")
          
          heartb_dt_unfilt[, (dropcols) := NULL]
          
          heartb_dt_unfilt[, timestamp := with_tz(timestamp, proj_timezone)]
          
      #    heartb_dt_unfilt <- heartb_dt_unfilt[as.IDate(timestamp, tz = proj_timezone) %between% 
      #                             c(start_date, end_date)][
      #                               idf != 'TIE-ZAR-119'][
      #                                 idf != '']
      
          
          #remove unwanted idf names
          heartb_dt_unfilt <- heartb_dt_unfilt[
                                     idf != 'TIE-ZAR-119'][
                                     idf != '']
          
          heartb_dt_unfilt[idf == ""]
          
    } ##end if (length(heartb_files_toread) > 0)
    
    
    #remove temporary objects
    remove(heartb_files_toread, heartb_files, dropcols, lhb)    
    