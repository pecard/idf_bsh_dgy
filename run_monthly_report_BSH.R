##
## Atalho alternativo para correr o relatorio mensal para 1 mes especifico
## sem editar IDF_monthly_report.R -- editar report_month abaixo e dar
## Source A ESTE FICHEIRO (Ctrl+Shift+S no RStudio, ou
## source("run_monthly_report_BSH.R") na consola).
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
## NUNCA copiar as linhas abaixo para dentro de IDF_monthly_report.R.
##
## Este e' o launcher do Bash (BSH) -- nao define monthly_settings_file,
## por isso usa a omissao de IDF_monthly_report.R ("monthlyReportSettings_BSH.R").
## Ver run_monthly_report_DGY.R para o equivalente do Dzhankeldy.
##

report_month <- "2026-08"

## Deixar FALSE na maioria das corridas -- so' mudar para TRUE na 1a corrida
## a seguir a descarregar dados novos (ex: SCADA de turbinas recem-
## -adicionadas, como BSH01/BSH33 em 2026-08), para forcar a releitura dos
## ficheiros brutos e regravar a cache com o conteudo atualizado (ver
## comentario em IDF_monthly_report.R, secção "0. Import data"). Voltar a
## FALSE depois dessa corrida -- deixar TRUE religa sempre os ficheiros
## brutos, mais lento sem necessidade.
force_reread_cache_monthly <- FALSE

## Flag por dataset -- so' este dataset relê dos ficheiros brutos, os
## outros 3 (tracks/scada/heartbeats) ficam pela cache (ver
## force_reread_cache_monthly acima e a nota em IDF_monthly_report.R,
## secção "0. Import data"). Util quando so' uma fonte tem ficheiros
## novos/corrigidos -- 2026-09, curtailments corrigido depois do bug do
## curtailments_pattern. Comentar/apagar esta linha (ou por FALSE) quando
## ja nao for preciso.

force_reread_curtailments <- F
force_reread_tracks <- F
force_reread_scada <- F
force_reread_heartbeats <- F

## TRUE por omissao (gera o .docx final). Mudar para FALSE quando estas so'
## a testar/depurar uma secção e queres inspecionar as tabelas/objetos
## diretamente no ambiente ou nos xlsx de anexo, sem esperar pelo
## rmarkdown::render() (o passo mais lento).
generate_report <- TRUE

source("IDF_monthly_report.R")
