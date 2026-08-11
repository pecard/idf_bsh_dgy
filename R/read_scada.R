##
## Read SCADA data (RPM/estado das turbinas)
##
## Depende de: data.table, R/read_utils.R
##

read_scada_data <- function(databases_dirs, pattern) {

  files <- list_files_multi_dir(databases_dirs, pattern)

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

  # Remover linhas duplicadas -- ex: mesmo ficheiro/periodo repetido entre
  # diretorios diferentes -- antes de qualquer calculo dependente da ordem
  dt <- unique(dt)

  setnames(dt, tolower(names(dt)))
  setnames(dt, "logtimestamp", "datetime")
  setkey(dt, datetime)

  dt[, `:=`(
    difft    = as.numeric(difftime(datetime, data.table::shift(datetime), units = 'secs')),
    monthy_y = format(as.Date(datetime), "%Y-%m")
  )]

  dt[, date_min := cut(datetime, breaks = '10 min')]

  dt
}
