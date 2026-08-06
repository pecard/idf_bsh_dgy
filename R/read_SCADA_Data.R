##
## Read SCADA files ----
##
    
    scadafiles <- tibble(file = list.files(databases_dir , pattern = 'SCADA_.+csv', full.names = T))
    scadafiles$date <- ymd(str_extract(scadafiles$file, "(?<=SCADA_).+?(?=_)"))
    
    # Files to read from specific date using "scada_ini"
    # scadafiles_toread <- filter(scadafiles, date >= date(scada_ini) & date <= date(scada_end))$file # --> filtrar a BD depois
    scadafiles_toread <- filter(scadafiles)$file 
    
    if (length(scadafiles_toread) > 0) {
    
          #scadafiles <- list.files(databases_dir , pattern = 'SCADA_', full.names = T)
          
          l <- lapply(scadafiles_toread, 
                      fread, 
                      sep =",", 
                      header = T, 
                      na.strings = "NULL", 
                      stringsAsFactors = F, 
                      #skip = ini, 
                      blank.lines.skip = T)
          
          scada_dt_unfilt <- rbindlist( l )
          
          
          #Organizar info
          setnames(scada_dt_unfilt, tolower(names(scada_dt_unfilt)))
          setnames(scada_dt_unfilt, c('logtimestamp'), c('datetime'))
          setkey(scada_dt_unfilt, 'datetime')
          scada_dt_unfilt[ , `:=` (difft = as.numeric(difftime(datetime, lag(datetime), 
                                                        units = 'secs')),
                            monthy_y =  format(as.Date(datetime), "%Y-%m")
          )]
          scada_dt_unfilt[, date_min := cut(datetime, breaks = '10 min')]
          
    } #end if (length(scadafiles_toread) > 0)
          
    
    
    #remove temporary objects
    remove(l, scadafiles,scadafiles_toread)
