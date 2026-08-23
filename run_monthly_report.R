##
## Atalho alternativo para correr o relatorio mensal para 1 mes especifico
## sem editar IDF_monthly_report.R -- editar report_month abaixo e dar
## Source A ESTE FICHEIRO (Ctrl+Shift+S no RStudio, ou
## source("run_monthly_report.R") na consola).
##
## IDF_monthly_report.R ja tem um report_month por omissao, editavel
## diretamente nesse ficheiro (ver comentario la' proprio) -- este launcher
## so' e' preciso se preferir nao editar IDF_monthly_report.R a cada mes; o
## report_month definido aqui tem prioridade sobre esse valor por omissao.
##
## Porque este ficheiro existe (2026-08): a mesma ideia -- "report_month <-
## '...'; source('IDF_monthly_report.R')" -- ja foi colocada 2 vezes DENTRO
## de IDF_monthly_report.R, o que faz esse ficheiro chamar-se A SI PROPRIO a
## meio da sua propria execucao (recursao infinita, acaba num erro confuso
## tipo "node stack overflow"). Aqui e' seguro porque este ficheiro e' um
## LANCADOR SEPARADO -- so' faz a chamada uma vez, nunca a partir de dentro
## do proprio IDF_monthly_report.R.
##
## NUNCA copiar as 2 linhas abaixo para dentro de IDF_monthly_report.R.
##

report_month <- "2026-01"
source("IDF_monthly_report.R")
