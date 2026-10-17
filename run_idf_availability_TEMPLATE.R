##
## MODELO de lancador -- relatorio de disponibilidade das unidades IDF
## Copiar para run_idf_availability_<FARM>_<periodo>.R e ajustar. Source.
##
project_settings_file <- "userSettings_IDF_availability_ZRF.R"   # CHANGE !!!
force_reread_cache    <- T  # TRUE so' na 1a corrida a seguir a descarregar dados novos
generate_report       <- TRUE
# report_team <- "Paulo Cardoso; Sandra Rodrigues"
source("run_idf_availability.R")
