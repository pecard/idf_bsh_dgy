##
## Read Heartbeats data (sinal de vida das unidades IDF)
##
## Depende de: data.table, janitor, lubridate
##

read_heartbeats_data <- function(databases_dir, pattern = heartbeats_pattern,
                                  tz = "UTC", exclude_idf = c("TIE-ZAR-119", "")) {

  files <- list.files(databases_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) return(NULL)

  dt <- rbindlist(lapply(
    files,
    fread,
    sep = ",",
    header = TRUE,
    na.strings = "NULL",
    stringsAsFactors = FALSE,
    blank.lines.skip = TRUE
  ))

  setnames(dt, names(clean_names(dt)))

  setnames(
    dt,
    old = c("instance_name", "minutes_between_heartbeats"),
    new = c("idf", "min_hb")
  )

  dt[, c("timestamp_utc", "previous_timestamp_utc") := NULL]

  dt[, timestamp := with_tz(timestamp, tz)]

  # remover labels de IDF invalidos/nao relevantes para o projeto
  dt <- dt[!idf %in% exclude_idf]

  dt
}
