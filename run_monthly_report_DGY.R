##
## Atalho para correr o relatorio mensal (IDF_monthly_report.R) para o
## parque Dzhankeldy (DGY) -- mesmo IDF_monthly_report.R do parque Bash
## (BSH), so' com monthly_settings_file apontado para
## monthlyReportSettings_DGY.R (ver comentario em IDF_monthly_report.R,
## "SETTINGS"). NAO ha um IDF_monthly_report_DGY.R separado -- os dois
## parques partilham o mesmo script de relatorio, parametrizado por qual
## ficheiro de settings e' sourced primeiro (mesma ideia de
## run_annual_analysis_DGY.R para IDF_analysis.R).
##
## Uso: editar report_month/force_reread_cache_monthly abaixo e dar
## Source A ESTE FICHEIRO (Ctrl+Shift+S no RStudio, ou
## source("run_monthly_report_DGY.R") na consola).
##
## Mesma razao de ser do run_monthly_report_BSH.R (ver esse ficheiro) para nao
## colocar isto dentro de IDF_monthly_report.R -- este e' um LANCADOR
## SEPARADO, nunca chamado a partir de dentro do proprio IDF_monthly_report.R.
##
## NUNCA copiar as linhas abaixo para dentro de IDF_monthly_report.R.
##

monthly_settings_file <- "monthlyReportSettings_DGY.R"

report_month <- "2026-07"

## Deixar FALSE na maioria das corridas -- so' mudar para TRUE na 1a corrida
## a seguir a descarregar dados novos, para forcar a releitura dos
## ficheiros brutos e regravar a cache com o conteudo atualizado (ver
## comentario em IDF_monthly_report.R, secção "0. Import data"). Voltar a
## FALSE depois dessa corrida -- deixar TRUE religa sempre os ficheiros
## brutos, mais lento sem necessidade. Cache do DGY fica em cache/DGY/ (nao
## colide com a cache/BSH/ do outro parque -- ver farm_code,
## monthlyReportSettings_DGY.R).
force_reread_cache_monthly <- F

source("IDF_monthly_report.R")
