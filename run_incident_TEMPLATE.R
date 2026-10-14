##
## MODELO de lancador de incidente -- qualquer parque
##
## Copiar para run_incident_<FARM>_<turbina>_<AAAAMMDD>.R e ajustar os 2
## ficheiros de settings. Dar Source a ESTE ficheiro (Ctrl+Shift+S).
##
## NUNCA copiar as linhas abaixo para dentro de run_incident.R.
##

## settings do PARQUE (datasets do portal IDF, shapefiles, DEM, ...)
project_settings_file  <- "userSettings_BSH.R"                       # CHANGE !!!
## settings do INCIDENTE (janela, turbina, unidades IDF, incidente) -- ver
## inputs/userSettings_incident_TEMPLATE.R
incident_settings_file <- "userSettings_incident_BSH_T00_20260131.R" # CHANGE !!!

## FALSE na maioria das corridas -- so' TRUE na 1a corrida a seguir a
## descarregar dados novos
force_reread_cache <- FALSE

## TRUE por omissao (gera o .docx final)
generate_report <- TRUE

## Equipa no cabecalho do relatorio (omissao: Paulo Cardoso; Sandra Rodrigues)
# report_team <- "Paulo Cardoso; Sandra Rodrigues"

source("run_incident.R")
