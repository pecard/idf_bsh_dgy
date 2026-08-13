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
      # fonte (portal IdentiFlight) ja vem em hora LOCAL (confirmado), nao UTC --
      # force_tz() reinterpreta os mesmos numeros do relogio como sendo tz local,
      # SEM deslocar o instante (with_tz() deslocaria +/- o offset, dando horas
      # absolutas erradas -- foi o bug que motivou esta correcao)
      start = lubridate::force_tz(start, tz),
      end   = lubridate::force_tz(end, tz),
      monthy_y = format(as.Date(start), "%Y-%m")
    )

  dt
}
