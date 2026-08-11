##
## Read Curtailments data (ordens de paragem/curtailment)
##
## Depende de: readxl, janitor, lubridate, dplyr, R/read_utils.R
##

read_curtailments_data <- function(databases_dirs, pattern) {

  files <- list_files_multi_dir(databases_dirs, pattern)

  read_one_file <- function(f) {
    read_xlsx(f, sheet = 1, skip = 1) %>%
      clean_names() %>%
      mutate(
        start = lubridate::as_datetime(start),
        end   = lubridate::as_datetime(end)
      )
  }

  dt <-
    lapply(files, read_one_file) %>%
    bind_rows() %>%
    distinct() %>% # remover linhas duplicadas -- ex: mesmo ficheiro/periodo repetido entre diretorios diferentes
    mutate(monthy_y = format(as.Date(start), "%Y-%m"))

  dt
}
