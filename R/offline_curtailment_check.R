##
## Cruza intervalos "offline" de uma unidade IDF (heartbeats) com evidencia
## de que o sistema continuava operacional -- curtailments disparados e/ou
## leituras SCADA de RPM -- pedido do Paulo, 2026-09.
##
## Um intervalo offline (sem heartbeat, ver compute_offline_intervals(),
## R/availability_daylight.R) e' interpretado, por omissao, como a
## protecao das aves desligada nesse periodo. Mas pode ser so' uma falha de
## comunicacao do PROPRIO heartbeat -- com o sistema de deteção/curtailment
## a continuar operacional. 2 fontes de evidencia independentes:
##   - curtailment disparado pela turbina DENTRO do intervalo -- evidencia
##     forte de que a unidade IDF estava mesmo a decidir/atuar (nao so' o
##     heartbeat que falhou)
##   - leitura SCADA de RPM da turbina DENTRO do intervalo -- evidencia de
##     que a turbina/SCADA estava a reportar normalmente (mecanicamente
##     operacional), independente da unidade IDF em si
## Um caso real (DGY, 2026-07, ver historial) mostrou as 2 fontes a
## divergir: heartbeat E scada ausentes num sub-periodo, mas com
## curtailments a disparar na mesma -- por isso classify_offline_evidence()
## abaixo distingue os 2 sinais em vez de os fundir num so' boolean.
##
## Depende de: data.table
##
## Uso:
##   source("R/availability_daylight.R")   # compute_offline_intervals()
##   source("R/turbine_idf_coverage.R")    # top_turbines_by_idf() (opcional, ver abaixo)
##   source("R/offline_curtailment_check.R")
##
##   offline_dt <- compute_offline_intervals(heartb_dt, offline_gap_min, online_grace_min)
##
##   ## turbina(s) a verificar por unidade IDF -- 2 fontes possiveis:
##   idf_turbines_dt <- idf_turbines_from_coverage(turbine_idf_coverage_dt, n = 2) # preferida (pedido do Paulo)
##   # OU, se turbine_idf_coverage_dt nao existir nesta sessao (so' calculada
##   # em IDF_analysis.R, secção 0 -- ver nota no explore_offline_curtailment_check.R):
##   idf_turbines_dt <- idf_turbines_from_manual_matrix(turbine_idf_manual_dt)
##
##   curtl_checked_dt <- check_offline_curtailment_overlap(offline_dt, curtl_dt, idf_turbines_dt)
##   scada_checked_dt <- check_offline_scada_presence(offline_dt, scada_dt, idf_turbines_dt)
##   combined_dt <- combine_offline_evidence(curtl_checked_dt, scada_checked_dt)
##   summarise_offline_evidence(combined_dt)
##


## 1. Turbina(s) a verificar por unidade IDF -- 2 formas de as obter --------

## 1a. A partir da cobertura geometrica 2D (top_turbines_by_idf(),
## R/turbine_idf_coverage.R) -- preferida (pedido do Paulo, 2026-09): as N
## turbinas com maior % de sobreposicao de buffer com essa unidade IDF, em
## vez so' da atribuicao manual 1-turbina-1-unidade.
idf_turbines_from_coverage <- function(coverage_dt, n = 2) {
  top_turbines_by_idf(coverage_dt, n = n)[, .(idf, turbine)]
}

## 1b. A partir da matriz manual (Turbine ID -> Primary IDF) -- fallback
## quando a cobertura geometrica (turbine_idf_coverage_dt) nao esta
## disponivel nesta sessao (so' calculada em IDF_analysis.R, nao em
## IDF_monthly_report.R -- ver nota no explore_offline_curtailment_check.R).
## So' a atribuicao Primary conta (colunas "Turbine ID"/"Primary IDF", ou
## ja' renomeadas "turbine"/"primary_idf") -- mesma fonte de
## join_availability_to_turbine(), R/availability_daylight.R.
idf_turbines_from_manual_matrix <- function(turbine_idf_manual_dt) {
  manual_dt <- data.table::as.data.table(turbine_idf_manual_dt)
  data.table::setnames(
    manual_dt,
    old = c("Turbine ID", "Primary IDF"),
    new = c("turbine", "primary_idf"),
    skip_absent = TRUE
  )
  unique(manual_dt[!is.na(primary_idf), .(idf = primary_idf, turbine)])
}


## 2. Curtailments disparados DENTRO do intervalo offline, pelas turbinas
## de idf_turbines_dt -------------------------------------------------------
##
## Unidades IDF ausentes de idf_turbines_dt (sem nenhuma turbina atribuida)
## ficam com n_curtailments_during_offline = 0/has_curtailment = FALSE --
## sem turbina para verificar, nao ha' como confirmar operacionalidade por
## esta via.

check_offline_curtailment_overlap <- function(offline_dt, curtl_dt, idf_turbines_dt) {

  off <- data.table::copy(offline_dt)
  off[, offline_id := .I]

  off_turbines <- merge(off, idf_turbines_dt, by = "idf", allow.cartesian = TRUE)

  if (nrow(off_turbines) == 0L || nrow(curtl_dt) == 0L) {
    off[, `:=`(n_curtailments_during_offline = 0L, has_curtailment = FALSE)]
    off[, offline_id := NULL]
    return(off[])
  }

  # mesmo padrao de interval join ja usado em time_to_first_decline()/
  # time_to_rpm_thresholds() (R/curtailment_response_latency.R,
  # R/curtailment_shutdown_time.R) -- curtl_dt e' "x", off_turbines e' "i".
  # Um curtailment conta como "dentro do intervalo offline" pelo seu
  # instante de start (>= off_start e <= off_end, fronteiras inclusive --
  # mesma logica >=/<= ja usada nesses joins).
  hits <- curtl_dt[
    off_turbines,
    on = .(turbine, start >= off_start, start <= off_end),
    allow.cartesian = TRUE,
    .(offline_id = i.offline_id, curtl_start = x.start)
  ]
  hits <- hits[!is.na(curtl_start)] # sem match -- o join deixa NA em vez de remover a linha

  curtl_counts <- hits[, .(n_curtailments_during_offline = .N), by = offline_id]

  out <- merge(off, curtl_counts, by = "offline_id", all.x = TRUE)
  out[is.na(n_curtailments_during_offline), n_curtailments_during_offline := 0L]
  out[, has_curtailment := n_curtailments_during_offline > 0L]
  out[, offline_id := NULL]

  out[]
}


## 3. Leituras SCADA de RPM DENTRO do intervalo offline, pelas turbinas de
## idf_turbines_dt -----------------------------------------------------------
##
## has_scada_rpm = TRUE so' confirma que HOUVE alguma leitura SCADA (a
## turbina estava a reportar, mecanicamente) -- nao diz nada sobre o valor
## do RPM em si (podia estar parada com rpm=0 e mesmo assim ter leitura).
## Mesma logica de idf_turbines_dt de check_offline_curtailment_overlap()
## acima -- unidades sem turbina atribuida ficam has_scada_rpm = FALSE.

check_offline_scada_presence <- function(offline_dt, scada_dt, idf_turbines_dt) {

  off <- data.table::copy(offline_dt)
  off[, offline_id := .I]

  off_turbines <- merge(off, idf_turbines_dt, by = "idf", allow.cartesian = TRUE)

  rpm_dt <- scada_dt[readingname == "RPM", .(turbine = turbinelabel, datetime)]

  if (nrow(off_turbines) == 0L || nrow(rpm_dt) == 0L) {
    off[, has_scada_rpm := FALSE]
    off[, offline_id := NULL]
    return(off[])
  }

  hits <- rpm_dt[
    off_turbines,
    on = .(turbine, datetime >= off_start, datetime <= off_end),
    allow.cartesian = TRUE,
    .(offline_id = i.offline_id, rpm_time = x.datetime)
  ]
  hits <- hits[!is.na(rpm_time)]

  scada_ids <- unique(hits$offline_id)

  out <- off
  out[, has_scada_rpm := offline_id %in% scada_ids]
  out[, offline_id := NULL]

  out[]
}


## 4. Junta os 2 sinais e classifica cada intervalo offline -----------------
##
## curtl_checked_dt/scada_checked_dt: devolvidos por
## check_offline_curtailment_overlap()/check_offline_scada_presence() sobre
## o MESMO offline_dt (por isso o merge por idf/off_start/off_end e'
## exato, sem folga de tempo a resolver).
##
## classification, por ordem de prioridade (caso real que motivou esta
## distincao, DGY 2026-07: um sub-periodo sem heartbeat NEM SCADA, mas
## com curtailments a disparar -- a evidencia mais forte, o curtailment,
## decide, mesmo quando o SCADA tambem esta em falha):
##   "Falha de comunicação da unidade IDF" -- has_curtailment = TRUE
##     (a unidade estava mesmo a decidir/atuar; SCADA em falha ao mesmo
##     tempo so' mostra que o SCADA tem a sua propria falha de comunicação
##     independente, nao que o sistema de protecao estivesse parado)
##   "Turbina operacional sem deteção" -- has_curtailment = FALSE E
##     has_scada_rpm = TRUE (SCADA confirma a turbina a reportar
##     normalmente; sem curtailment so' porque nao passou nenhuma ave)
##   "Sem evidência (heartbeat e SCADA em falta)" -- nenhum dos 2 sinais --
##     o caso mais ambiguo: pode ser indisponibilidade genuina da protecao,
##     ou so' um gap de dados que afeta heartbeat E SCADA em simultaneo
##     (ex: ficheiros brutos em falta para esse periodo) -- requer
##     confirmacao manual, nao assumir nenhuma das duas leituras sozinha

classify_offline_evidence <- function(curtl_checked_dt, scada_checked_dt) {

  combined <- merge(
    curtl_checked_dt, scada_checked_dt,
    by = c("idf", "off_start", "off_end"),
    all = TRUE
  )

  combined[, classification := data.table::fcase(
    has_curtailment,                     "Falha de comunicação da unidade IDF",
    !has_curtailment & has_scada_rpm,     "Turbina operacional sem deteção",
    default = "Sem evidência (heartbeat e SCADA em falta)"
  )]

  combined[]
}


## 5. Resumo -- minutos offline por classificacao, por unidade IDF e
## farm-wide ------------------------------------------------------------

summarise_offline_evidence <- function(combined_dt) {

  dt <- data.table::copy(combined_dt)
  dt[, duration_mins := as.numeric(difftime(off_end, off_start, units = "mins"))]

  by_idf <- dt[, .(
    n_intervals = .N,
    total_mins  = round(sum(duration_mins), 1)
  ), by = .(idf, classification)]
  data.table::setorder(by_idf, idf, classification)

  overall <- dt[, .(
    n_intervals = .N,
    total_mins  = round(sum(duration_mins), 1)
  ), by = classification]
  overall[, pct_of_total := round(100 * total_mins / sum(total_mins), 1)]
  data.table::setorder(overall, classification)

  list(by_idf = by_idf[], overall = overall[])
}
