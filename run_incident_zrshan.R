##
## Atalho COMPATIVEL -- o script de incidente e' agora generico (qualquer
## parque): ver run_incident.R. Os lancadores existentes
## (run_incident_zrshan_T*.R) continuam a funcionar sem alteracoes: definem
## project_settings_file e dao source a ESTE ficheiro.
##
## Comportamento antigo preservado: sem project_settings_file definido,
## usa inputs/userSettings_ZRF.R (incidente T35 de 2026-05).
##
if (!exists("project_settings_file")) project_settings_file <- "userSettings_ZRF.R"
source("run_incident.R")
