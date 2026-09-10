##
## Read SCADA data (RPM/estado das turbinas)
##
## Depende de: data.table, R/read_utils.R (list_files_multi_dir, read_csv_files_safe)
##

read_scada_data <- function(databases_dirs, pattern, tz = "UTC", farm_pattern = NULL) {

  files <- list_files_multi_dir(databases_dirs, pattern, farm_pattern)

  if (length(files) == 0) return(NULL)

  dt <- read_csv_files_safe(
    files,
    sep = ",",
    header = TRUE,
    na.strings = "NULL",
    stringsAsFactors = FALSE,
    blank.lines.skip = TRUE
  )

  # Remover linhas duplicadas -- ex: mesmo ficheiro/periodo repetido entre
  # diretorios diferentes -- antes de qualquer calculo dependente da ordem
  dt <- unique(dt)

  setnames(dt, tolower(names(dt)))
  setnames(dt, "logtimestamp", "datetime")

  # fonte (portal IdentiFlight) ja vem em hora LOCAL (confirmado), nao UTC --
  # force_tz() reinterpreta os mesmos numeros do relogio como sendo tz local,
  # SEM deslocar o instante (with_tz() deslocaria +/- o offset, dando horas
  # absolutas erradas -- foi o bug que motivou esta correcao)
  dt[, datetime := lubridate::force_tz(datetime, tz)]

  setkey(dt, datetime)

  dt[, `:=`(
    difft    = as.numeric(difftime(datetime, data.table::shift(datetime), units = 'secs')),
    monthy_y = format(as.Date(datetime), "%Y-%m")
  )]

  dt[, date_min := cut(datetime, breaks = '10 min')]

  dt
}
