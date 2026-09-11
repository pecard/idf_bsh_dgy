##
## Agregados farm-wide para a secção "Technical Summary" (sem numeração,
## topo do relatorio mensal -- ver report/monthly_report_template.rmd) --
## pedido do Paulo, 2026-09: 6 frases de contexto antes do relatorio
## detalhado (disponibilidade geral do IDF, curtailments por prioridade,
## latencia de resposta, tempo de paragem, no-response/missed curtailments,
## curtailments desnecessarios P->NP).
##
## Cada funcao aqui reutiliza um objeto JA calculado por outra secção do
## relatorio mensal (system availability, curtailments by species, shutdown
## time, ID transitions) -- nao faz nenhum calculo novo sobre os dados
## brutos, so' agrega esse objeto a 1 unico valor farm-wide (em vez de por
## unidade IDF/turbina/especie). Latencia de resposta e no-response nao
## precisam de funcao propria aqui -- summarise_latency(), ja em
## R/curtailment_response_latency.R, ja devolve exatamente 1 linha
## farm-wide com mean_latency_sec/n_no_response/pct_no_response.
##
## Depende de: data.table
##
## Uso:
##   source("R/monthly_technical_summary.R")
##   summarise_availability_overall(idf_availability_summary$by_idf)
##   summarise_curtailment_priority_split(monthly_species_curt_by_group_dt)
##   summarise_shutdown_overall(tt_dt, safe_shutdown_rpm)
##   summarise_unnecessary_curtailments(monthly_id_risk_summary$pnp_curtailments)
##


## 1. Disponibilidade geral do IDF -- soma farm-wide (nao por unidade) ----
##
## by_idf_dt: summarise_availability(idf_availability_dt)$by_idf,
## R/availability_daylight.R -- daylight_mins_total e' o MESMO valor em
## todas as linhas (mesmo calendario para todas as unidades), por isso
## somar por unidade da' o total de minutos-oportunidade de monitorizacao
## de toda a frota (n_unidades x calendario), nao um numero inflacionado
## por engano -- e o denominador correto para o offline_pct farm-wide
## (equivalente a uma media ponderada por unidade, ja que o peso e' igual
## em todas).

summarise_availability_overall <- function(by_idf_dt) {

  daylight_mins_total <- sum(by_idf_dt$daylight_mins_total)
  offline_mins_total  <- sum(by_idf_dt$offline_mins_total)

  data.table::data.table(
    daylight_mins_total = daylight_mins_total,
    offline_mins_total  = offline_mins_total,
    offline_pct         = if (daylight_mins_total == 0) NA_real_ else round(100 * offline_mins_total / daylight_mins_total, 1)
  )
}


## 2. Curtailments por grupo de prioridade -- extrai priority/nonpriority ----
##
## by_group_dt: summarise_curtailment_species_group(), R/curtailment_species.R
## -- NAO e' 0-preenchida na origem (um grupo sem nenhum curtailment esse
## mes simplesmente nao aparece como linha), por isso 0-preenchemos aqui.

summarise_curtailment_priority_split <- function(by_group_dt) {

  get_group <- function(g) {
    row <- by_group_dt[species_group == g]
    if (nrow(row) == 0L) return(list(n = 0L, pct_of_total = 0))
    list(n = row$n[1], pct_of_total = row$pct_of_total[1])
  }

  p  <- get_group("priority")
  np <- get_group("nonpriority")

  data.table::data.table(
    priority_n = p$n, priority_pct = p$pct_of_total,
    nonpriority_n = np$n, nonpriority_pct = np$pct_of_total
  )
}


## 3. Tempo de paragem farm-wide, so' para o limiar "parado" ----
##
## tt_dt: time_to_rpm_thresholds(), R/curtailment_shutdown_time.R.
## stopped_threshold: o rpm que define "parado" no resto do relatorio
## (safe_shutdown_rpm, monthlyReportSettings_BSH.R/_DGY.R) -- precisa de
## ser um dos valores testados em shutdown_time_thresholds, senao dt fica
## sempre vazio (0 linhas) e devolve NA sem aviso.

summarise_shutdown_overall <- function(tt_dt, stopped_threshold) {

  dt <- tt_dt[threshold == stopped_threshold & !is.na(time_to_threshold_sec)]

  if (nrow(dt) == 0L) {
    return(data.table::data.table(mean_time_sec = NA_real_, max_time_sec = NA_real_))
  }

  data.table::data.table(
    mean_time_sec = round(mean(dt$time_to_threshold_sec), 1),
    max_time_sec  = round(max(dt$time_to_threshold_sec), 1)
  )
}


## 4. Curtailments desnecessarios (P->NP) -- numero e % de TODOS os
## curtailments do mes (nao % de tracks multi-ID, ao contrario da tabela
## "Risk by Direction") ----
##
## pnp_curtailments_dt: summarise_id_transition_risk(...)$pnp_curtailments,
## R/id_transitions.R -- fica com 0 linhas (schema vazio) quando risk_dt
## (tracks com mais de 1 classificacao de especie) esta vazio esse mes.

summarise_unnecessary_curtailments <- function(pnp_curtailments_dt) {

  if (nrow(pnp_curtailments_dt) == 0L) {
    return(data.table::data.table(n = 0L, pct_of_total = NA_real_))
  }

  data.table::data.table(
    n = pnp_curtailments_dt$curtailments_due_to_p_to_np[1],
    pct_of_total = pnp_curtailments_dt$pct_of_total[1]
  )
}
