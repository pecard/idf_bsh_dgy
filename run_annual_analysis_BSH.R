##
## Atalho para correr a analise anual (IDF_analysis.R) sem editar esse
## ficheiro -- editar force_reread_cache abaixo e dar Source A ESTE
## FICHEIRO (Ctrl+Shift+S no RStudio, ou source("run_annual_analysis_BSH.R")
## na consola). Este e' o launcher do Bash (BSH) -- nao define
## project_settings_file, por isso usa a omissao de IDF_analysis.R
## ("userSettings_BSH.R"). Ver run_annual_analysis_DGY.R para o
## equivalente do Dzhankeldy.
##
## IDF_analysis.R ja tem force_reread_cache = FALSE por omissao (so' assume
## esse valor se nao estiver definido antes) -- este launcher so' e' preciso
## se preferires nao definir a variavel na consola a cada corrida; o valor
## definido aqui tem prioridade sobre esse valor por omissao.
##
## Mesma razao de ser do run_monthly_report_BSH.R (ver esse ficheiro): NUNCA
## colocar "force_reread_cache <- TRUE; source('IDF_analysis.R')" DENTRO de
## IDF_analysis.R -- isso faria esse ficheiro chamar-se a si proprio a meio
## da sua propria execucao (recursao infinita, "node stack overflow"). Aqui
## e' seguro porque este ficheiro e' um LANCADOR SEPARADO -- so' faz a
## chamada uma vez, nunca a partir de dentro do proprio IDF_analysis.R.
##
## NUNCA copiar as linhas abaixo para dentro de IDF_analysis.R.
##

## Deixar FALSE na maioria das corridas -- so' mudar para TRUE na 1a corrida
## a seguir a descarregar dados novos (ex: SCADA de turbinas recem-
## -adicionadas, como BSH01/BSH33 em 2026-08), para forcar a releitura dos
## ficheiros brutos e regravar a cache com o conteudo atualizado (ver
## comentario em IDF_analysis.R, secção "0. Import data"). Voltar a FALSE
## depois dessa corrida -- deixar TRUE religa sempre os ficheiros brutos,
## mais lento sem necessidade.
force_reread_cache <- F

source("IDF_analysis.R")
