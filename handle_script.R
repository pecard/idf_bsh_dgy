##
## Project-specific data adjustments
##

  #WTG data
  wtg$ID <- wtg$Name
  wtg$ID <- sub("^([^0-9]*)(0*)([1-9][0-9]*)(.*)$", "\\1\\3\\4", wtg$ID)
  
  #correct labels
  #wtg$ID <- str_replace(wtg$ID, " - R", "") #remover caracteres para uniformizar nomes das turbinas
  #wtg$ID <- str_replace(wtg$ID, "A", "") #remover caracteres para uniformizar nomes das turbinas
  #wtg$ID <- str_replace(wtg$ID, "B", "") #remover caracteres para uniformizar nomes das turbinas  
  
  
  #Tier info
  tier$turbine <- sub("^([^0-9]*)(0*)([1-9][0-9]*)(.*)$", "\\1\\3\\4", tier$WTG)
  
  #Tier3
  tier3$turbine <- sub("^([^0-9]*)(0*)([1-9][0-9]*)(.*)$", "\\1\\3\\4", tier3$wtg)
  #tier3$turbine <- str_replace(tier3$turbine, "A", "") #remover caracteres para uniformizar nomes das turbinas
  
  scada_dt_unfilt$turbinelabel <- sub("^([^0-9]*)(0*)([1-9][0-9]*)(.*)$", "\\1\\3\\4", scada_dt_unfilt$turbinelabel)
  curtl_dt_unfilt$turbine <- sub("^([^0-9]*)(0*)([1-9][0-9]*)(.*)$", "\\1\\3\\4", curtl_dt_unfilt$turbine)
  track_dt_unfilt$turbine <- sub("^([^0-9]*)(0*)([1-9][0-9]*)(.*)$", "\\1\\3\\4", track_dt_unfilt$turbine)
  
  
