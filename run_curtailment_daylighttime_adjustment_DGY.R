##
## Atalho para correr a analise de ajuste do horario diario de curtailment
## (R/curtailment_daylighttime_adjustment.R) para o parque Dzhankeldy (DGY)
## -- carrega SO' os curtailments (load_curtailments.R, NAO o
## IDF_analysis.R inteiro -- nao le tracks/SCADA/heartbeats nem calcula
## disponibilidade/resposta/terreno/clustering) e corre a analise/graficos
## (explore_curtailment_daylighttime_adjustment.R).
##
## Uso: editar as opcoes abaixo se necessario, e dar Source A ESTE
## FICHEIRO (Ctrl+Shift+S no RStudio, ou
## source("run_curtailment_daylighttime_adjustment_DGY.R") na consola).
##
## Mesma logica de run_annual_analysis_BSH.R/run_annual_analysis_DGY.R --
## este e' um LANCADOR SEPARADO, nunca chamado a partir de dentro de
## load_curtailments.R nem de explore_curtailment_daylighttime_adjustment.R.
##
## NUNCA copiar as linhas abaixo para dentro desses ficheiros.
##

project_settings_file <- "userSettings_DGY.R"

## TRUE por omissao -- curtailments e' o mais leve dos 4 datasets grandes,
## sem custo real em reler sempre os ficheiros brutos (ver nota em
## load_curtailments.R) -- garante que ficheiros novos (ex: um mes
## acabado de descarregar) sao sempre apanhados, sem teres de te lembrar
## de forcar. Muda para FALSE se quiseres reutilizar a cache em disco sem
## reler (ex: varias corridas seguidas so' a ajustar os parametros da
## analise abaixo, mesmos dados brutos).
force_reread_cache <- TRUE

## Opcoes da analise (todas opcionais -- ver
## explore_curtailment_daylighttime_adjustment.R para os valores por
## omissao de cada uma se nao definidas aqui):
# window_months <- 6              # janela de decisao -- ultimos N meses
# proposed_start_clock <- "07:00" # janela fixa proposta a testar
# proposed_end_clock   <- "18:00"
# plot_context_months <- 12       # so' para o contexto visual dos graficos
# edge_period   <- "month"        # "month" ou "week" -- bordos robustos (secção 2)
# edge_bin_mins <- 10
# edge_pct      <- 0.01
# edge_min_n    <- 20

source("load_curtailments.R")
source("explore_curtailment_daylighttime_adjustment.R")
