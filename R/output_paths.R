##
## Nomes de pastas/ficheiros de output de um incidente
##
## Cada incidente grava numa pasta propria, outputs/<AAAAMMDD de execucao>_<incident_tag>/,
## e todos os ficheiros levam o sufixo _<incident_tag>_<AAAAMMDD de execucao> --
## assim 2 incidentes do mesmo parque corridos no mesmo dia nao se
## sobrepoem, e um ficheiro copiado para fora da pasta (ex: anexo a um
## email) continua a identificar o incidente e a corrida. Voltar a correr o
## mesmo incidente no mesmo dia sobrescreve a propria pasta (intencional).
##
## incident_tag = <farm_code>_<turbina>_<data do incidente AAAAMMDD>
## (ex: ZRF_T94_20261001) -- nao usa incident_id, que nao tem formato
## uniforme entre incidentes (ZRF_Oct2026, ZRF_May032026, ...).
##
## Uso (ver run_incident_zrshan.R):
##   source("R/output_paths.R")
##   incident_tag <- make_incident_tag(farm_code, fatality_incidents$turbine[1], fatality_incidents$incident_date[1])
##   run_date     <- format(Sys.time(), "%Y%m%d")
##   folder_output <- incident_output_folder("outputs", run_date, incident_tag)
##   out_name("coverage_3d_summary", "xlsx", incident_tag, run_date)
##   # -> "coverage_3d_summary_ZRF_T94_20261001_20261007.xlsx"
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
