##
## Merge tier data
##

    #Tier scheme - Starting date
    tier <- read_xlsx(file.path(folder_input, tier_start_scheme_filename), sheet="tiers")
    tier$turbine <- sub("^([^0-9]*)(0*)([1-9][0-9]*)(.*)$", "\\1\\3\\4", tier$WTG)
    #correct labels
    tier$turbine <- str_replace(tier$turbine, " - R", "") #remover caracteres para uniformizar nomes das turbinas
    tier$turbine <- str_replace(tier$turbine, "R", "") #remover caracteres para uniformizar nomes das turbinas
    tier$turbine <- str_replace(tier$turbine, "A", "") #remover caracteres para uniformizar nomes das turbinas
    tier$turbine <- str_replace(tier$turbine, "B", "") #remover caracteres para uniformizar nomes das turbinas
    tier$turbine <- str_replace(tier$turbine, "New", "") #remover caracteres para uniformizar nomes das turbinas
    tier_dt <- setDT(tier)
    # convert mixed date columns
    excel_cols <- c("Tier 1 ini", "tier 1 end", "Tier 2A ini")
    for (cl in excel_cols) {
      tier[, (cl) := as.Date(as.numeric(get(cl)), origin = "1899-12-30")]
    }
    # convert POSIXct columns to Date
    date_cols <- c("First Gen Date", "WTG op Target Date",
                   "Tier 2A end", "Tier 2B ini", "Tier 2B end", "Tier 3 ini")
    
    for (cl in date_cols) {
      tier[, (cl) := as.Date(get(cl))]
    }
    
    
    #Tier 3 scheme - Starting date
    tier3 <- read_xlsx(file.path(folder_input, tier3_start_scheme_filename), 
                       sheet = 'tier3', col_types = c("date", "text", 'text', 'text')) %>% clean_names()
    tier3$turbine <- sub("^([^0-9]*)(0*)([1-9][0-9]*)(.*)$", "\\1\\3\\4", tier3$wtg)
    tier3$turbine <- str_replace(tier3$turbine, "A", "") #remover caracteres para uniformizar nomes das turbinas
    
    tier3_dt <- setDT(tier3)
    tier3_dt[, idate := as.IDate(timestamp)]
    set_tier3_dt <- tier3_dt[idate < date(end)]
    
    #update tier data with tier3
    tier_dt[tier3_dt, `Tier 3 ini` := i.idate, on = "turbine"]
    #remove `Tier 3 ini` dates from Tier3 for T106 & T108 --> it was set by site for tier1 as of june 2025
    tier_dt[turbine %in% c("T106", "T108"), `Tier 3 ini` := NA]
    
    
    #export data
    writexl::write_xlsx(tier_dt, 
                        path = file.path(folder_output,  
                                         paste0('tier_dates.xlsx')) )