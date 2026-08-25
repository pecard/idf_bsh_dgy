##
## Atalho para correr a analise anual (IDF_analysis.R) para o parque
## Dzhankeldy (DGY) -- mesmo IDF_analysis.R do parque Bash (BSH), so' com
## project_settings_file apontado para userSettings_DGY.R (ver comentario
## em IDF_analysis.R, "USER SETTINGS"). NAO ha um IDF_analysis_DGY.R
## separado -- os dois parques partilham o mesmo script de analise,
## parametrizado por qual ficheiro de settings e' sourced primeiro.
##
## Uso: editar force_reread_cache abaixo se necessario, e dar Source A
## ESTE FICHEIRO (Ctrl+Shift+S no RStudio, ou
## source("run_annual_analysis_DGY.R") na consola).
##
## Mesma razao de ser do run_annual_analysis.R (ver esse ficheiro) para
## nao colocar isto dentro de IDF_analysis.R -- este e' um LANCADOR
## SEPARADO, nunca chamado a partir de dentro do proprio IDF_analysis.R.
##
## NUNCA copiar as linhas abaixo para dentro de IDF_analysis.R.
##

project_settings_file <- "userSettings_DGY.R"

## Deixar FALSE na maioria das corridas -- so' mudar para TRUE na 1a corrida
## a seguir a descarregar dados novos, para forcar a releitura dos
## ficheiros brutos e regravar a cache com o conteudo atualizado (ver
## comentario em IDF_analysis.R, secção "0. Import data"). Voltar a FALSE
## depois dessa corrida -- deixar TRUE religa sempre os ficheiros brutos,
## mais lento sem necessidade. Cache do DGY fica em cache/DGY/ (nao colide
## com a cache/BSH/ do outro parque -- ver farm_code, userSettings_DGY.R).
force_reread_cache <- F

source("IDF_analysis.R")
