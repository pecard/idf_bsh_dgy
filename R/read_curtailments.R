##
## Read Curtailments data (ordens de paragem/curtailment)
##
## Depende de: readxl, janitor, lubridate, dplyr, R/read_utils.R
##

read_curtailments_data <- function(databases_dirs, pattern, tz = "UTC") {

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
    mutate(
      # source em UTC -- converte para tz local (ex: proj_timezone), so muda a
      # exibicao/agrupamento por dia, nao o instante (nao afeta roll joins)
      start = lubridate::with_tz(start, tz),
      end   = lubridate::with_tz(end, tz),
      monthy_y = format(as.Date(start), "%Y-%m")
    )

  dt
}
