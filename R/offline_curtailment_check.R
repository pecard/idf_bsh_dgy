##
## Verifica curtailments disparados DENTRO de intervalos "offline" de uma
## unidade IDF (heartbeats) -- pedido do Paulo, 2026-09.
##
## Um intervalo offline (sem heartbeat, ver compute_offline_intervals(),
## R/availability_daylight.R) e' interpretado, por omissao, como a
## protecao das aves desligada nesse periodo. Mas pode ser so' uma falha de
## comunicacao do PROPRIO heartbeat -- com o sistema de deteção/curtailment
## a continuar operacional. Se a(s) turbina(s) cobertas por essa unidade
## IDF dispararam pelo menos 1 curtailment DENTRO do intervalo offline,
## isso e' evidencia de que o sistema estava mesmo operacional -- esse
## intervalo deve ser reclassificado como "Falha de comunicação da unidade
## IDF", nao indisponibilidade genuina da protecao.
##
## Depende de: data.table
##
## Uso:
##   source("R/availability_daylight.R") # compute_offline_intervals()
##   source("R/offline_curtailment_check.R")
##
##   offline_dt <- compute_offline_intervals(heartb_dt, offline_gap_min, online_grace_min)
##   checked_dt <- check_offline_curtailment_overlap(offline_dt, curtl_dt, turbine_idf_manual_dt)
##   summarise_offline_curtailment_overlap(checked_dt)
##


## 1. Junta cada intervalo offline as turbinas cobertas por essa unidade
## IDF (Primary IDF, matriz manual -- mesma fonte de
## join_availability_to_turbine(), R/availability_daylight.R) e verifica se
## alguma delas disparou um curtailment DENTRO do intervalo ----------------
##
## turbine_idf_manual_dt: mesmo formato usado em join_availability_to_turbine()
## (colunas "Turbine ID"/"Primary IDF", ou ja' renomeadas "turbine"/
## "primary_idf") -- so' a atribuicao Primary conta aqui (uma unidade
## Secondary normalmente nao e' quem dispara o curtailment dessa turbina).
## Unidades IDF sem nenhuma turbina Primary atribuida na matriz ficam com
## n_curtailments_during_offline = 0/has_curtailment = FALSE (sem turbina
## para verificar, nao ha' como confirmar operacionalidade por esta via).

check_offline_curtailment_overlap <- function(offline_dt, curtl_dt, turbine_idf_manual_dt) {

  manual_dt <- data.table::as.data.table(turbine_idf_manual_dt)
  data.table::setnames(
    manual_dt,
    old = c("Turbine ID", "Primary IDF"),
    new = c("turbine", "primary_idf"),
    skip_absent = TRUE
  )
  idf_turbines_dt <- unique(manual_dt[!is.na(primary_idf), .(idf = primary_idf, turbine)])

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


## 2. Resumo -- minutos offline reclassificados ("Falha de comunicação da
## unidade IDF", quando ha pelo menos 1 curtailment dentro do intervalo) vs
## indisponibilidade genuina, por unidade IDF e farm-wide -------------------

summarise_offline_curtailment_overlap <- function(checked_dt) {

  dt <- data.table::copy(checked_dt)
  dt[, duration_mins := as.numeric(difftime(off_end, off_start, units = "mins"))]
  dt[, classification := data.table::fifelse(
    has_curtailment, "Falha de comunicação da unidade IDF", "Indisponibilidade genuína"
  )]

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
