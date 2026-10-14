##
## Nomes de pastas/ficheiros de output de um incidente
##
## Cada CORRIDA grava numa pasta propria, outputs/<AAAAMMDD_HHMM de execucao>_<incident_tag>/,
## e todos os ficheiros levam o sufixo _<incident_tag>_<AAAAMMDD_HHMM de execucao> --
## assim 2 incidentes do mesmo parque, ou o MESMO incidente corrido com
## parametrizacoes diferentes (mesmo dia), nao se sobrepoem; uma corrida
## anterior fica inalterada na sua pasta. Um ficheiro copiado para fora da
## pasta (ex: anexo a um email) continua a identificar o incidente e a
## corrida. So' duas corridas no MESMO minuto partilham a pasta (a 2a
## sobrescreve a 1a).
##
## Cada pasta inclui tambem uma copia do settings file usado
## (settings_<incident_tag>_<stamp>.R) e um run_info_<incident_tag>_<stamp>.txt
## (data/hora, settings file, commit git) -- ver write_run_info() -- para
## saber, mais tarde, com que parametros foi produzida.
##
## incident_tag = <farm_code>_<turbina>_<data do incidente AAAAMMDD>
## (ex: ZRF_T94_20261001) -- nao usa incident_id, que nao tem formato
## uniforme entre incidentes (ZRF_Oct2026, ZRF_May032026, ...).
##
## Uso (ver run_incident_zrshan.R):
##   source("R/output_paths.R")
##   incident_tag <- make_incident_tag(farm_code, fatality_incidents$turbine[1], fatality_incidents$incident_date[1])
##   run_date     <- make_run_stamp()   # "AAAAMMDD_HHMM"
##   folder_output <- incident_output_folder("outputs", run_date, incident_tag)
##   out_name("coverage_3d_summary", "xlsx", incident_tag, run_date)
##   # (run_date = "20261007_1532")
##   # -> "coverage_3d_summary_ZRF_T94_20261001_20261007_1532.xlsx"
##
## Depende de: nada (R base)
##

make_incident_tag <- function(farm_code, turbine, incident_date) {
  paste(farm_code, turbine, format(as.Date(incident_date), "%Y%m%d"), sep = "_")
}

incident_output_folder <- function(root, run_date, incident_tag) {
  file.path(root, paste0(run_date, "_", incident_tag))
}

out_name <- function(base, ext, incident_tag, run_date) {
  sprintf("%s_%s_%s.%s", base, incident_tag, run_date, sub("^\\.", "", ext))
}

## Identificador da corrida: data + hora (minuto) locais, ex: "20261007_1532"
make_run_stamp <- function(time = Sys.time()) format(time, "%Y%m%d_%H%M")

## Copia o settings file usado para a pasta da corrida e escreve um
## run_info (texto) com data/hora, incidente, settings file e commit git
## (NA se git nao estiver disponivel). Devolve os 2 caminhos escritos.
write_run_info <- function(folder, incident_tag, run_stamp, settings_file, settings_path = settings_file,
                           incident_settings_file = NULL, incident_settings_path = incident_settings_file) {
  settings_copy <- file.path(folder, sprintf("settings_%s_%s.R", incident_tag, run_stamp))
  file.copy(settings_path, settings_copy, overwrite = TRUE)
  # settings do incidente (sobrepostos aos do parque), se existirem
  incident_copy <- NULL
  if (!is.null(incident_settings_file)) {
    incident_copy <- file.path(folder, sprintf("settings_incident_%s_%s.R", incident_tag, run_stamp))
    file.copy(incident_settings_path, incident_copy, overwrite = TRUE)
  }

  commit <- tryCatch(
    suppressWarnings(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)),
    error = function(e) NA_character_
  )
  commit <- if (length(commit) == 1L && !is.na(commit) && nzchar(commit)) commit else NA_character_

  info_file <- file.path(folder, sprintf("run_info_%s_%s.txt", incident_tag, run_stamp))
  writeLines(c(
    paste("Run stamp:      ", run_stamp),
    paste("Run time:       ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    paste("Incident tag:   ", incident_tag),
    paste("Settings file:  ", settings_file),
    paste("Settings copy:  ", basename(settings_copy)),
    if (!is.null(incident_copy)) paste("Incident settings:", incident_settings_file),
    if (!is.null(incident_copy)) paste("Incident copy:  ", basename(incident_copy)),
    paste("Git commit:     ", commit)
  ), info_file)

  c(settings_copy, incident_copy, info_file)
}
