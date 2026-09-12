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
## heartbeat_interval_min. Turbina(s) por unidade IDF -- ver secção 1
## abaixo: a matriz manual (Primary IDF) e' SEMPRE preferida quando
## identificada para o parque (turbine_idf_manual_dt); so' cai para a
## cobertura geometrica (limiar de %, numero de turbinas variavel por
## unidade) quando nao ha ficheiro de matriz manual.
##
## Correr: source("explore_offline_curtailment_check.R")
##

source("R/availability_daylight.R") # compute_offline_intervals()
source("R/turbine_idf_coverage.R")  # turbines_by_idf_threshold() (fallback geometrico)
source("R/offline_curtailment_check.R")

## 1. Turbina(s) a verificar por unidade IDF -- decisao do Paulo, 2026-09,
## apos o caso real da DZH62-04 (primaria genuina de 2 turbinas, DZH62 E
## DZH63): a matriz manual e' SEMPRE preferida quando existir um ficheiro
## operacional identificado para o parque, porque reflete conhecimento
## real que nenhum metodo geometrico reproduz sozinho. So' cai para a
## cobertura geometrica (limiar de % de sobreposicao, NAO um numero fixo
## de turbinas -- ver turbines_by_idf_threshold(), R/turbine_idf_coverage.R)
## quando NAO ha matriz manual identificada -- ver
## OFFLINE_EVIDENCE_SCOPE_NOTE, R/offline_curtailment_check.R, para a
## salvaguarda que acompanha esse fallback -----------------------------

min_pct_coverage <- 20
idf_turbines_dt <- NULL
used_geometric_fallback <- FALSE

if (exists("turbine_idf_manual_dt") && !is.null(turbine_idf_manual_dt)) {

  cat("\nA usar a matriz manual (Primary IDF) -- fonte preferida (ficheiro operacional identificado para este parque).\n")
  idf_turbines_dt <- idf_turbines_from_manual_matrix(turbine_idf_manual_dt)

} else {

  message(sprintf(
    "Nenhuma matriz manual (turbine_idf_manual_dt) identificada para este parque -- a usar cobertura geometrica 2D como fallback (turbinas com >= %d%% de sobreposicao de buffer).",
    min_pct_coverage
  ))
  used_geometric_fallback <- TRUE

  if (exists("turbine_idf_coverage_dt")) {
    cat("Cobertura geometrica ja calculada nesta sessao.\n")

  } else if (exists("wtg") && exists("idf_op_detection_range") && exists("idf_filename") &&
             exists("folder_input") && exists("crs_projection_plannar")) {

    cat(sprintf("turbine_idf_coverage_dt nao existia -- a ler '%s' e calcular agora.\n", idf_filename))

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

  } else {
    stop("Nem turbine_idf_manual_dt nem cobertura geometrica (turbine_idf_coverage_dt, ou wtg+idf_filename+idf_op_detection_range para a calcular) disponiveis -- impossivel saber que turbina(s) verificar por unidade IDF.")
  }

  idf_turbines_dt <- idf_turbines_from_coverage(turbine_idf_coverage_dt, min_pct_coverage = min_pct_coverage)

  n_no_turbine <- data.table::uniqueN(turbine_idf_coverage_dt$idf) - data.table::uniqueN(idf_turbines_dt$idf)
  if (n_no_turbine > 0) {
    message(sprintf(
      "Aviso: %d unidade(s) IDF sem nenhuma turbina >= %d%% de cobertura -- ficam sem verificacao possivel (has_curtailment/has_scada_rpm = FALSE sempre).",
      n_no_turbine, min_pct_coverage
    ))
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

  # xlsx partilhado com a equipa IDF e o cliente (pedido do Paulo, 2026-09)
  # -- conteudo integralmente em ingles (nomes de sheet, colunas e valores
  # de classificacao). Sheet de metodologia (OFFLINE_EVIDENCE_SCOPE_NOTE,
  # R/offline_curtailment_check.R) so' incluida quando o fallback
  # geometrico foi mesmo usado -- a matriz manual (fonte preferida) nao
  # tem esta limitacao, nao precisa da salvaguarda.
  sheets <- list(
    All_offline_intervals    = combined_dt,
    Comm_failure_confirmed   = combined_dt[classification == "IDF unit communication failure"],
    Operational_no_detection = combined_dt[classification == "Turbine operational, no detection"],
    No_evidence_review       = combined_dt[classification == "No evidence (heartbeat and SCADA both missing)"],
    Summary_by_idf           = summary_offline$by_idf,
    Summary_overall          = summary_offline$overall
  )

  if (used_geometric_fallback) {
    sheets <- c(
      list(Methodology_Note = data.table::data.table(Methodology_Note = OFFLINE_EVIDENCE_SCOPE_NOTE)),
      sheets
    )
  }

  write_xlsx_local(sheets, file.path(folder_output, "offline_curtailment_overlap_check.xlsx"))
  cat(sprintf("\nGravado: '%s'\n", file.path(folder_output, "offline_curtailment_overlap_check.xlsx")))
}
