##
## Latencia de resposta ao curtailment -- tempo desde o "start" ate a
## turbina COMECAR a reagir de forma significativa (nao ate parar)
##
## Pedido do Paulo (2026-08): a equipa de desenvolvimento do IDF sugere
## uma latencia esperada de ~20s entre o sinal de curtailment e o inicio
## da desaceleracao; o Paulo suspeita que pode ser maior, ~30s. Isto e'
## uma pergunta DIFERENTE da dos ficheiros R/curtailment_response_buffer.R
## e R/curtailment_shutdown_time.R -- aqueles medem quanto tempo ate a
## turbina ATINGIR um RPM baixo (parar); este mede quanto tempo ate a
## turbina COMECAR a descer (o arranque da resposta).
##
## Porque um limiar RELATIVO (fracao do RPM de baseline), nao absoluto:
## turbinas podem iniciar o curtailment a RPMs de baseline muito
## diferentes (ex: 10 rpm num evento, 6.5 rpm noutro -- ver os proprios
## exemplos "missed"/"delayed" ja explorados). Um limiar absoluto (ex:
## "latencia = tempo ate < 3 rpm") nao mede a mesma coisa nos dois casos:
## a turbina a 6.5 rpm ja esta muito mais perto de 3 rpm a partida, sem
## ter respondido mais depressa -- so' teria "menos caminho a percorrer".
## decline_pct_threshold evita essa distorcao: mede sempre "caiu X% face
## ao proprio ponto de partida", seja qual for esse ponto de partida.
##
## Adotado em producao 2026-08 (IDF_analysis.R secção 3.5b/IDF_monthly_report.R
## secção 5b) com decline_pct_threshold=0.10 (mesmo valor de
## curtailment_drop_pct_threshold, mas mecanismo diferente -- ver nota em
## time_to_first_decline() abaixo) e buffer_after_end_sec=shutdown_time_buffer_sec
## (partilhado com time_to_rpm_thresholds(), R/curtailment_shutdown_time.R).
## Um "no-response event" (secção "No-Response Events" do relatorio) e' um
## curtailment sem decline_pct_threshold detetado dentro dessa janela --
## nem sequer uma resposta parcial, nao so' uma resposta lenta.
##
## Depende de: data.table, R/curtailment_response.R (usa match_nearest_rpm())
##
## Uso:
##   source("R/curtailment_response.R")
##   source("R/curtailment_response_latency.R")
##   latency_dt <- time_to_first_decline(curtl_dt, scada_dt, decline_pct_threshold = 0.10, buffer_after_end_sec = 60)
##   summarise_latency(latency_dt)
##   summarise_latency_bands(latency_dt)
##   summarise_latency_by_turbine(latency_dt)
##


## 1. Tempo ate a 1a queda relativa significativa de RPM, por curtailment ----
##
## buffer_after_end_sec (por omissao 0, mesmo comportamento de
## time_to_rpm_thresholds()): a janela de procura e' [start, end +
## buffer_after_end_sec] -- ligada a duracao REAL de cada curtailment (nao
## um offset fixo desde o start), o mesmo padrao ja usado em
## time_to_rpm_thresholds(). Procuramos sempre a PRIMEIRA leitura que
## cumpre o limiar (min(datetime)), por isso alargar a janela nunca muda
## um resultado ja encontrado, so' pode encontrar um mais tarde nos casos
## que antes ficavam sem deteção.

time_to_first_decline <- function(curtl_dt, scada_dt, decline_pct_threshold = 0.10,
                                  start_end_gap_sec = 2, buffer_after_end_sec = 0) {

  dt <- as.data.table(curtl_dt)
  dt[, curtailment_id := .I]

  empty <- data.table::data.table(
    curtailment_id = integer(), turbine = character(), track_id = character(),
    species = character(), start = as.POSIXct(character()), end = as.POSIXct(character()),
    start_rpm = numeric(), decline_time = as.POSIXct(character()), latency_sec = numeric()
  )

  ## baseline no start (mesma tolerancia apertada usada em toda a analise
  ## de resposta) -- so seguimos curtailments com match fiavel
  start_events <- dt[, .(id = curtailment_id, turbine, event_time = start)]
  start_match  <- match_nearest_rpm(start_events, scada_dt, max_gap_sec = start_end_gap_sec)

  valid_ids <- start_match[valid_match == TRUE, id]
  dt_valid  <- dt[curtailment_id %in% valid_ids]
  if (nrow(dt_valid) == 0L) return(empty)

  start_rpm_dt <- start_match[valid_match == TRUE, .(curtailment_id = id, start_rpm = rpm)]

  windows <- merge(
    dt_valid[, .(curtailment_id, turbine, window_start = start,
                window_end = end + buffer_after_end_sec)],
    start_rpm_dt, by = "curtailment_id"
  )

  rpm_dt <- scada_dt[readingname == "RPM", .(turbine = turbinelabel, datetime, rpm = value)]

  rpm_window <- rpm_dt[
    windows,
    on = .(turbine, datetime >= window_start, datetime <= window_end),
    allow.cartesian = TRUE,
    .(curtailment_id = i.curtailment_id, window_start = i.window_start,
      start_rpm = i.start_rpm, datetime = x.datetime, rpm = x.rpm)
  ]

  rpm_window[, decline_pct := data.table::fifelse(
    !is.na(start_rpm) & start_rpm > 0 & !is.na(rpm), (start_rpm - rpm) / start_rpm, NA_real_
  )]

  hits <- rpm_window[
    !is.na(decline_pct) & decline_pct >= decline_pct_threshold,
    .(decline_time = min(datetime), window_start = first(window_start)),
    by = curtailment_id
  ]
  if (nrow(hits) > 0L) hits[, latency_sec := as.numeric(difftime(decline_time, window_start, units = "secs"))]

  out <- merge(
    dt_valid[, .(curtailment_id, turbine, track_id, species, start, end)],
    start_rpm_dt, by = "curtailment_id"
  )
  out <- merge(out, hits[, .(curtailment_id, decline_time, latency_sec)], by = "curtailment_id", all.x = TRUE)
  data.table::setorder(out, curtailment_id)
  out[]
}


## 2. Resumo farm-wide -- 1 linha, com n reached/no-response e media/mediana ----
##
## n_no_response/pct_no_response = complemento de n_reached/pct_reached --
## e' a definicao de "no-response event" usada na secção "No-Response
## Events" do relatorio: nenhuma queda >= decline_pct_threshold detetada
## dentro da janela de procura (nem sequer uma resposta parcial).

summarise_latency <- function(latency_dt) {

  n_total   <- nrow(latency_dt)
  n_reached <- sum(!is.na(latency_dt$latency_sec))

  data.table::data.table(
    n_curtailments     = n_total,
    n_reached          = n_reached,
    pct_reached        = round(100 * n_reached / n_total, 1),
    n_no_response      = n_total - n_reached,
    pct_no_response    = round(100 * (n_total - n_reached) / n_total, 1),
    mean_latency_sec   = round(mean(latency_dt$latency_sec, na.rm = TRUE), 1),
    median_latency_sec = round(median(latency_dt$latency_sec, na.rm = TRUE), 1)
  )
}


## 3. Bandas -- % de curtailments (dos que tiveram deteção) com latencia
##    dentro de varios cortes (ex: <=20s vs <=30s, para comparar a sugestao
##    da equipa do IDF com a hipotese do Paulo) ----

summarise_latency_bands <- function(latency_dt, cutoffs_sec = c(20, 30, 40, 60)) {

  n_reached <- sum(!is.na(latency_dt$latency_sec))

  data.table::rbindlist(lapply(cutoffs_sec, function(cutoff) {
    data.table::data.table(
      cutoff_sec        = cutoff,
      pct_within_cutoff = if (n_reached == 0L) NA_real_ else
        round(100 * sum(latency_dt$latency_sec <= cutoff, na.rm = TRUE) / n_reached, 1)
    )
  }))
}


## 4. Resumo por turbina -- mesmas colunas de summarise_latency(), 1 linha
##    por turbina, para identificar turbinas com latencia/no-response
##    sistematicamente mais alta ----

summarise_latency_by_turbine <- function(latency_dt) {

  by_turbine <- latency_dt[, {
    n_reached <- sum(!is.na(latency_sec))
    .(
      n_curtailments     = .N,
      n_reached          = n_reached,
      pct_reached        = round(100 * n_reached / .N, 1),
      n_no_response      = .N - n_reached,
      pct_no_response    = round(100 * (.N - n_reached) / .N, 1),
      mean_latency_sec   = round(mean(latency_sec, na.rm = TRUE), 1),
      median_latency_sec = round(median(latency_sec, na.rm = TRUE), 1)
    )
  }, by = turbine]

  data.table::setorder(by_turbine, -pct_no_response)
  by_turbine[]
}


## 5. Sweep de decline_pct_threshold -- exploratorio, ver se a latencia
##    estimada e' sensivel a escolha do limiar relativo (ex: 5% vs 10% vs
##    20%). Nao usado em producao (IDF_analysis.R usa sempre um so' valor,
##    curtailment_latency_decline_pct). ----

compare_latency_thresholds <- function(curtl_dt, scada_dt,
                                       decline_pct_candidates = c(0.05, 0.10, 0.20),
                                       start_end_gap_sec = 2, buffer_after_end_sec = 0) {

  data.table::rbindlist(lapply(decline_pct_candidates, function(p) {
    lat_dt <- time_to_first_decline(
      curtl_dt, scada_dt, decline_pct_threshold = p,
      start_end_gap_sec = start_end_gap_sec, buffer_after_end_sec = buffer_after_end_sec
    )
    summ  <- summarise_latency(lat_dt)
    bands <- summarise_latency_bands(lat_dt)
    summ[, decline_pct_threshold := p]
    summ[, pct_within_20s := bands[cutoff_sec == 20, pct_within_cutoff]]
    summ[, pct_within_30s := bands[cutoff_sec == 30, pct_within_cutoff]]
    data.table::setcolorder(summ, c("decline_pct_threshold", setdiff(names(summ), "decline_pct_threshold")))
    summ[]
  }))
}


## 6. Exemplos para plot -- "no_response": sem deteção, os mais recentes
##    primeiro; "slowest": maior latencia registada, entre os que tiveram
##    deteção. Mesmo padrao de select_curtailment_examples()
##    (R/curtailment_forensic_trace.R), adaptado a latency_dt (que nao tem
##    coluna response_flag). ----

select_latency_examples <- function(latency_dt, type = c("no_response", "slowest"), n = 3) {

  type <- match.arg(type)
  dt <- if (type == "no_response") latency_dt[is.na(latency_sec)] else latency_dt[!is.na(latency_sec)]
  if (nrow(dt) == 0L) return(dt)

  if (type == "slowest") data.table::setorder(dt, -latency_sec) else data.table::setorder(dt, -start)
  dt[seq_len(min(n, .N))]
}


## 7. Histograma da distribuicao de latencia, com marcadores nos 20s/30s
##    em discussao ----

plot_latency_histogram <- function(latency_dt, cutoffs_sec = c(20, 30)) {

  dt <- latency_dt[!is.na(latency_sec)]

  ggplot(dt, aes(x = latency_sec)) +
    geom_histogram(binwidth = 5, boundary = 0, fill = "steelblue", colour = "black") +
    geom_vline(xintercept = cutoffs_sec, colour = "firebrick", linetype = "dashed") +
    scale_x_continuous(breaks = scales::breaks_width(20), expand = c(0, 0)) +
    labs(
      x = "Latency -- time from curtailment start to first significant RPM decline (s)",
      y = "Number of curtailments",
      title = "Distribution of response latency"
    ) +
    theme_minimal()
}
