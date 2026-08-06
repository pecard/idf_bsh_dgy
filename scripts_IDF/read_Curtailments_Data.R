##
## Read Curtailments files ----
##
    
    # Function to read Excel files ----
    read_xlsx_files <- function(x){
      df <- 
        read_xlsx(path = x, sheet = 1, skip = 1) %>%
        clean_names() %>%
        mutate(start = lubridate::as_datetime(start),
               end = lubridate::as_datetime(end) )
      return(df)
    }
    
    crtlfiles <- 
      tibble(
        file = list.files(
          databases_dir,
          pattern = "curtail_orders|Curtailments", #procurar por ficheiros com "curtail_orders" OU "curtailments"
          full.names = TRUE
        )
      ) %>%
      mutate(
        date = ymd(
          str_extract(file, "(?<=curtail_orders_|Curtailments_).+?(?=_)")
        )
      )
    
    # Files to read from specific date
    #crtlfiles_toread <- filter(crtlfiles, date >= ini & date <= end)$file # using "ini" #filtrar a BD depois
    crtlfiles_toread <- filter(crtlfiles)$file # using "ini"
    
    curtl_dt_unfilt <- 
      lapply(crtlfiles_toread, read_xlsx_files ) %>%
      bind_rows() %>%
      mutate(monthy_y =  format(as.Date(start), "%Y-%m"))
    
    
    #remove temporary objects
    remove(crtlfiles,crtlfiles_toread, read_xlsx_files )
