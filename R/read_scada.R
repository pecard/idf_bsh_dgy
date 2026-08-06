##
## Read SCADA data (RPM/estado das turbinas)
##
## Depende de: data.table
##

read_scada_data <- function(databases_dir, pattern = "SCADA_.+csv") {

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

  setnames(dt, tolower(names(dt)))
  setnames(dt, "logtimestamp", "datetime")
  setkey(dt, datetime)

  dt[, `:=`(
    difft    = as.numeric(difftime(datetime, shift(datetime), units = 'secs')),
    monthy_y = format(as.Date(datetime), "%Y-%m")
  )]

  dt[, date_min := cut(datetime, breaks = '10 min')]

  dt
}
