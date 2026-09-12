##
## Script de consola para cruzar intervalos "offline" (heartbeats) de uma
## unidade IDF com evidencia de que o sistema continuava operacional --
## pedido do Paulo, 2026-09.
##
## Um intervalo offline (sem heartbeat) e' assumido, por omissao, como a
## protecao das aves desligada nesse periodo. Mas pode ser so' uma falha de
## comunicacao do PROPRIO heartbeat. 2 fontes de evidencia cruzadas:
##   - curtailment disparado pela turbina DENTRO do intervalo (evidencia
##     forte -- a unidade IDF estava mesmo a decidir/atuar)
##   - leitura SCADA de RPM da turbina DENTRO do intervalo (evidencia de
##     que a turbina/SCADA estava a reportar normalmente)
## Um caso real (DGY, 2026-07, revisto manualmente pelo Paulo) mostrou
## heartbeat E SCADA ausentes num sub-periodo, mas com curtailments a
## disparar na mesma -- por isso os 2 sinais sao mantidos distintos em vez
## de fundidos num so' boolean (ver classify_offline_evidence(),
## R/offline_curtailment_check.R, para os 3 rotulos possiveis e a ordem de
## prioridade entre os 2 sinais).
##
## NAO faz parte do pipeline de producao -- e' so' para explorar/confirmar
## a hipotese antes de decidir se vale a pena formalizar isto numa secção
## do relatorio.
##
## Pre-requisitos (correr isto DEPOIS de uma corrida normal de
## IDF_analysis.R OU IDF_monthly_report.R, na mesma sessao) -- objetos ja
## tem de existir: heartb_dt, curtl_dt, scada_dt, heartbeat_offline_gap_min,
## heartbeat_interval_min. Turbina(s) por unidade IDF -- 2 fontes possiveis
## (ver secção 1 abaixo), a preferida (cobertura geometrica 2D) so' fica
## disponivel se turbine_idf_coverage_dt ja tiver sido calculada (so'
## acontece em IDF_analysis.R, secção 0 -- NAO em IDF_monthly_report.R, que
## nao le o shapefile das unidades IDF nem idf_op_detection_range). Se so'
## correste o relatorio mensal nesta sessao, o script usa a matriz manual
## como alternativa, com um aviso.
##
## Correr: source("explore_offline_curtailment_check.R")
##

source("R/availability_daylight.R") # compute_offline_intervals()
source("R/turbine_idf_coverage.R")  # top_turbines_by_idf() (cobertura geometrica)
source("R/offline_curtailment_check.R")

## 1. Turbina(s) a verificar por unidade IDF -- preferencia: cobertura
## geometrica 2D (top 2 turbinas por unidade), pedido do Paulo, 2026-09.
## 3 niveis de fallback, do mais ao menos preferido -- so' cai para a
## matriz manual se nao houver mesmo forma de calcular a geometria ---------

n_top_turbines <- 2L
idf_turbines_dt <- NULL

if (exists("turbine_idf_coverage_dt")) {
  cat(sprintf("\nA usar cobertura geometrica 2D ja calculada nesta sessao (top %d turbina(s) por unidade IDF).\n", n_top_turbines))
  idf_turbines_dt <- idf_turbines_from_coverage(turbine_idf_coverage_dt, n = n_top_turbines)

} else if (exists("wtg") && exists("idf_op_detection_range") && exists("idf_filename") &&
           exists("folder_input") && exists("crs_projection_plannar")) {

  cat(sprintf(
    "\nturbine_idf_coverage_dt nao existia -- a ler '%s' e calcular agora (top %d turbina(s) por unidade IDF).\n",
    idf_filename, n_top_turbines
  ))

  # mesma normalizacao de coluna de ID que IDF_analysis.R aplica (secção
  # "0. Import data") antes de compute_turbine_idf_coverage() -- BSH ja'
  # tem "imaging_he" nativamente, DGY so' tem "Name" (idf_source_id_col,
  # ver monthlyReportSettings_DGY.R). NAO reproduz o filtro de
  # idf_installed_units do IDF_analysis.R (exclui 2 registos "DZH-23"
  # fantasma, sem geometria de turbina distinta) -- inofensivo aqui, esses
  # registos nunca aparecem em heartb_dt$idf, por isso nunca fazem match
  # em offline_dt.
  if (!exists("idf")) {
    idf <- sf::read_sf(file.path(folder_input, idf_filename))
    idf <- sf::st_transform(idf, crs_projection_plannar)
    idf_source_id_col <- if (exists("idf_source_id_col")) idf_source_id_col else "imaging_he"
    idf$imaging_he <- idf[[idf_source_id_col]]
  }

  turbine_idf_coverage_dt <- compute_turbine_idf_coverage(
    wtg, idf, buffer_m = idf_op_detection_range,
    wtg_id_col = "InternalNa", idf_id_col = "imaging_he"
  )
  idf_turbines_dt <- idf_turbines_from_coverage(turbine_idf_coverage_dt, n = n_top_turbines)
}

if (is.null(idf_turbines_dt)) {
  if (exists("turbine_idf_manual_dt") && !is.null(turbine_idf_manual_dt)) {
    message(
      "turbine_idf_coverage_dt indisponivel nesta sessao, e faltam wtg/idf_filename/idf_op_detection_range/",
      "folder_input/crs_projection_plannar para a calcular na hora. A usar a matriz manual (Primary IDF) como ",
      "alternativa -- para a cobertura geometrica (o metodo preferido), confirma que correste o relatorio ",
      "mensal ate' a secção que le wtg, e que idf_filename/idf_op_detection_range estao definidos em ",
      "monthlyReportSettings_BSH.R/_DGY.R (adicionados 2026-09)."
    )
    idf_turbines_dt <- idf_turbines_from_manual_matrix(turbine_idf_manual_dt)
  } else {
    stop("Nem cobertura geometrica (turbine_idf_coverage_dt, ou wtg+idf_filename+idf_op_detection_range para a calcular) nem turbine_idf_manual_dt disponiveis -- impossivel saber que turbina(s) verificar por unidade IDF.")
  }
}

cat(sprintf("Turbina(s) por unidade IDF a verificar (%d unidade(s)):\n", data.table::uniqueN(idf_turbines_dt$idf)))
print(idf_turbines_dt[order(idf)])


## 2. Intervalos offline por unidade IDF, mesmos parametros do relatorio ----

offline_dt <- compute_offline_intervals(
  heartb_dt,
  offline_gap_min  = heartbeat_offline_gap_min,
  online_grace_min = heartbeat_interval_min
)

cat(sprintf("\n===== %d intervalo(s) offline encontrado(s), %d unidade(s) IDF distintas =====\n",
  nrow(offline_dt), data.table::uniqueN(offline_dt$idf)
))


## 3. Cruza com curtailments E leituras SCADA de RPM, so' nas turbinas de
## idf_turbines_dt -----------------------------------------------------

curtl_checked_dt <- check_offline_curtailment_overlap(offline_dt, curtl_dt, idf_turbines_dt)
scada_checked_dt <- check_offline_scada_presence(offline_dt, scada_dt, idf_turbines_dt)
combined_dt <- classify_offline_evidence(curtl_checked_dt, scada_checked_dt)

cat("\n===== Resumo: classificação por evidência =====\n")
summary_offline <- summarise_offline_evidence(combined_dt)
cat("-- Farm-wide --\n")
print(summary_offline$overall)
cat("\n-- Por unidade IDF --\n")
print(summary_offline$by_idf)


## 4. Detalhe -- para revisao manual do Paulo (confirmar caso a caso,
## sobretudo "Sem evidência", antes de decidir a classificacao final) ------

cat("\n===== Detalhe (ordenado por classificação, depois por unidade/data) =====\n")
detail_dt <- combined_dt[order(classification, idf, off_start)]
print(detail_dt[, .(idf, off_start, off_end, n_curtailments_during_offline, has_scada_rpm, classification)])

if (exists("write_xlsx_local") && exists("folder_output")) {
  write_xlsx_local(
    list(
      All_offline_intervals    = combined_dt,
      Comm_failure_confirmed   = combined_dt[classification == "Falha de comunicação da unidade IDF"],
      Operational_no_detection = combined_dt[classification == "Turbina operacional sem deteção"],
      No_evidence_review       = combined_dt[classification == "Sem evidência (heartbeat e SCADA em falta)"],
      Summary_by_idf           = summary_offline$by_idf,
      Summary_overall          = summary_offline$overall
    ),
    file.path(folder_output, "offline_curtailment_overlap_check.xlsx")
  )
  cat(sprintf("\nGravado: '%s'\n", file.path(folder_output, "offline_curtailment_overlap_check.xlsx")))
}
