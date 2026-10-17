##
## Atalho para correr o relatorio de incidente de Zarafshan (ZRF) -- Egyptian
## Vulture, turbina T35, 11/05/2026 -- com project_settings_file apontado
## para inputs/userSettings_ZRF_T35_20260511.R.
##
## Uso: editar force_reread_cache/generate_report abaixo se necessario, e
## dar Source A ESTE FICHEIRO (Ctrl+Shift+S no RStudio, ou
## source("run_incident_zrshan_T35_20260511.R") na consola).
##
## Mesma logica de run_annual_analysis_BSH.R/run_annual_analysis_DGY.R --
## este e' um LANCADOR SEPARADO, nunca chamado a partir de dentro de
## run_incident_zrshan.R.
##
## NUNCA copiar as linhas abaixo para dentro de run_incident_zrshan.R.
##
## heartbeat_idf_units (IDF24/26/22/71/21) em inputs/userSettings_ZRF_T35_20260511.R.
##

project_settings_file <- "userSettings_ZRF_T35_20260511.R"

## Deixar FALSE na maioria das corridas -- so' TRUE na 1a corrida a seguir
## a descarregar dados novos (mesma cache/ZRSHAN/ partilhada com o
## incidente T35, ver farm_code em userSettings_ZRF_T35_20260511.R).
force_reread_cache <- T

## TRUE por omissao (gera o .docx final). FALSE so' para testar/depurar
## uma secção sem esperar pelo rmarkdown::render().
generate_report <- TRUE

source("run_incident_zrshan.R")
