##
## Read Heartbeats data (sinal de vida das unidades IDF)
##
## Depende de: data.table, janitor, lubridate, R/read_utils.R
##

read_heartbeats_data <- function(databases_dirs, pattern = "Bash_Heartbeats.+csv",
                                  tz = "UTC", exclude_idf = c("TIE-ZAR-119", "")) {

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

  setnames(dt, names(clean_names(dt)))

  setnames(
    dt,
    old = c("instance_name", "minutes_between_heartbeats"),
    new = c("idf", "min_hb")
  )

  dt[, c("timestamp_utc", "previous_timestamp_utc") := NULL]

  # fonte (portal IdentiFlight) ja vem em hora LOCAL (confirmado), nao UTC --
  # daí a coluna timestamp_utc/previous_timestamp_utc serem descartadas acima
  # e usarmos "timestamp" -- force_tz() reinterpreta os mesmos numeros do
  # relogio como tz local, SEM deslocar o instante (with_tz() deslocaria
  # +/- o offset, dando horas absolutas erradas -- foi o bug que motivou esta correcao)
  dt[, timestamp := force_tz(timestamp, tz)]

  # remover labels de IDF invalidos/nao relevantes para o projeto
  dt <- dt[!idf %in% exclude_idf]

  dt
}
