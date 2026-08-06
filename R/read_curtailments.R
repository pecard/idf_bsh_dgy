##
## Read Curtailments data (ordens de paragem/curtailment)
##
## Depende de: readxl, janitor, lubridate, dplyr
##

read_curtailments_data <- function(databases_dir, pattern = "curtail_orders|Curtailments") {

  files <- list.files(databases_dir, pattern = pattern, full.names = TRUE)

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
    mutate(monthy_y = format(as.Date(start), "%Y-%m"))

  dt
}
