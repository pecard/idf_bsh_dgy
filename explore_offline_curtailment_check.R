##
## Script de consola para verificar se os intervalos "offline" (heartbeats)
## de uma unidade IDF tinham, apesar de tudo, curtailments a ser disparados
## pelas turbinas cobertas por essa unidade -- pedido do Paulo, 2026-09.
##
## Um intervalo offline (sem heartbeat) e' assumido, por omissao, como a
## protecao das aves desligada nesse periodo. Mas pode ser so' uma falha de
## comunicacao do PROPRIO heartbeat, com o sistema de deteção/curtailment a
## continuar operacional. Se encontrarmos curtailments disparados DENTRO de
## um intervalo offline, isso e' evidencia de que o sistema estava mesmo
## operacional -- esse intervalo deveria ser reclassificado como "Falha de
## comunicação da unidade IDF", nao indisponibilidade genuina.
##
## NAO faz parte do pipeline de producao (IDF_analysis.R/IDF_monthly_report.R
## nunca o chamam, nao escreve nada em outputs/ exceto o xlsx de revisao
## abaixo) -- e' so' para explorar/confirmar a hipotese antes de decidir se
## vale a pena formalizar isto numa secção do relatorio.
##
## Pre-requisitos (correr isto DEPOIS de uma corrida normal de
## IDF_analysis.R OU IDF_monthly_report.R, na mesma sessao) -- objetos ja
## tem de existir: heartb_dt, curtl_dt, turbine_idf_manual_dt,
## heartbeat_offline_gap_min, heartbeat_interval_min (todos ja calculados/
## lidos pelo script principal -- ver esses ficheiros se algum faltar).
##
## Correr: source("explore_offline_curtailment_check.R")
##

source("R/availability_daylight.R") # compute_offline_intervals()
source("R/offline_curtailment_check.R")

if (is.null(turbine_idf_manual_dt)) {
  stop("turbine_idf_manual_dt e' NULL -- confirma se a matriz manual (turbine_idf_matrix_filename) existe em inputs/ nesta corrida.")
}

## 1. Intervalos offline por unidade IDF, mesmos parametros do relatorio ----

offline_dt <- compute_offline_intervals(
  heartb_dt,
  offline_gap_min  = heartbeat_offline_gap_min,
  online_grace_min = heartbeat_interval_min
)

cat(sprintf("\n===== %d intervalo(s) offline encontrado(s), %d unidade(s) IDF distintas =====\n",
  nrow(offline_dt), data.table::uniqueN(offline_dt$idf)
))

## 2. Cruza com curtailments -- ha algum disparado DENTRO do intervalo? ----

checked_dt <- check_offline_curtailment_overlap(offline_dt, curtl_dt, turbine_idf_manual_dt)

cat("\n===== Resumo: indisponibilidade genuina vs falha de comunicação =====\n")
summary_offline <- summarise_offline_curtailment_overlap(checked_dt)
cat("-- Farm-wide --\n")
print(summary_offline$overall)
cat("\n-- Por unidade IDF --\n")
print(summary_offline$by_idf)

## 3. Detalhe dos intervalos reclassificados -- para revisao manual do Paulo
## (confirmar caso a caso antes de assumir que e' mesmo so' falha de
## comunicação, nao um problema real intermitente) --------------------------

reclass_dt <- checked_dt[has_curtailment == TRUE]
cat(sprintf(
  "\n%d de %d intervalo(s) offline tinham pelo menos 1 curtailment dentro do periodo -- candidatos a 'Falha de comunicação da unidade IDF':\n",
  nrow(reclass_dt), nrow(checked_dt)
))
print(reclass_dt[order(-n_curtailments_during_offline)])

if (exists("write_xlsx_local") && exists("folder_output")) {
  write_xlsx_local(
    list(All_offline_intervals = checked_dt, Reclassified_comm_failure = reclass_dt),
    file.path(folder_output, "offline_curtailment_overlap_check.xlsx")
  )
  cat(sprintf("\nGravado: '%s'\n", file.path(folder_output, "offline_curtailment_overlap_check.xlsx")))
}
