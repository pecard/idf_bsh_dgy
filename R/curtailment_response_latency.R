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
## cutin_rpm (Paulo, 2026-08, a partir de 3 exemplos "no-response" no
## relatorio anual cujo RPM de baseline ja estava perto de 0 -- turbina
## claramente ja parada/abaixo da velocidade de cut-in tipica de producao
## de energia, ~3 rpm, NAO um curtailment falhado): curtailments cujo RPM
## no "start" (mesmo baseline de start_end_gap_sec) esta abaixo de
## cutin_rpm sao excluidos da contagem de reached/no-response (nao ha
## "resposta" significativa para medir numa turbina que ja nao estava a
## produzir), mas ficam marcados (`below_cutin = TRUE`) e contados a parte
## -- filtrados, mas visiveis, nao apagados do universo.
##
## no_data (Paulo, 2026-08, a partir de um caso concreto -- BSH54,
## 2025-11-11 13:47:38, RPM claramente achatado ~6.5 durante toda a ordem,
## que "desapareceu" completamente de latency_dt): curtailments sem
## nenhuma leitura SCADA a <= start_end_gap_sec do "start" nao tinham
## nenhum bucket proprio -- ficavam simplesmente FORA de latency_dt, sem
## contar em lado nenhum (nem reached, nem no-response, nem below_cutin).
## O esquema antigo (classify_response_flag(), R/curtailment_response_classify.R)
## tinha esse caso coberto (final_status == "no_data", visivel em toda a
## tabela via pct_of_total vs pct_of_known) -- este esquema replica essa
## visibilidade: no_data fica marcado (nao apagado), e n_no_data/pct_no_data
## sao sempre % do universo TOTAL (n_curtailments), tal como pct_of_total
## em summarise_response_by_flag(). Sem isto, um curtailment com resposta
## genuinamente nula podia "desaparecer" so' por a leitura SCADA mais
## proxima do sinal exato de start ter caido a 3-9s (tick tipico do SCADA
## ~10-15s) em vez de <=2s -- nao e' falta real de dados, e' so' a
## tolerancia apertada a nao alinhar com a amostragem.
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
                                  start_end_gap_sec = 2, buffer_after_end_sec = 0, cutin_rpm = 0) {

  dt <- as.data.table(curtl_dt)
  dt[, curtailment_id := .I]

  empty <- data.table::data.table(
    curtailment_id = integer(), turbine = character(), track_id = character(),
    species = character(), start = as.POSIXct(character()), end = as.POSIXct(character()),
    start_rpm = numeric(), no_data = logical(), below_cutin = logical(),
    decline_time = as.POSIXct(character()), latency_sec = numeric()
  )
  if (nrow(dt) == 0L) return(empty)

  ## baseline no start -- roll = start_end_gap_sec (nao "nearest"): so'
  ## aceita a leitura SCADA mais recente a <= start_end_gap_sec ANTES do
  ## sinal de start, nunca depois. Decisao 2026-08 (ver nota do Paulo sobre
  ## o risco de "mascarar o estado real" ao relaxar start_end_gap_sec): uma
  ## leitura POSTERIOR ao start pode ja refletir alguma desaceleracao real
  ## (a propria resposta que estamos a medir), o que contaminaria o
  ## baseline e, por construcao do calculo (decline_pct relativo a
  ## start_rpm), tende a SOBRESTIMAR a latencia (o limiar de queda passa a
  ## ser medido a partir de um numero ja mais baixo, exigindo MAIS queda
  ## adicional para disparar) -- podendo classificar erradamente respostas
  ## rapidas como lentas ou mesmo como no-response. Uma leitura ANTERIOR ao
  ## start nunca pode conter essa contaminacao (o curtailment ainda nao
  ## tinha sido emitido), por isso e' seguro relaxar start_end_gap_sec
  ## (menos no_data) sem este risco, desde que o match continue restrito ao
  ## passado -- start_rpm_dt so' tem os curtailments com match fiavel; os
  ## restantes (no_data) ficam sem entrada aqui, recuperados como NA no
  ## merge all.x mais abaixo
  start_events <- dt[, .(id = curtailment_id, turbine, event_time = start)]
  start_match  <- match_nearest_rpm(start_events, scada_dt, max_gap_sec = start_end_gap_sec, roll = start_end_gap_sec)

  start_rpm_dt <- start_match[valid_match == TRUE, .(curtailment_id = id, start_rpm = rpm)]
  start_rpm_dt[, below_cutin := start_rpm < cutin_rpm]

  ## curtailments abaixo do cut-in ficam de fora da procura de queda (nao
  ## ha "resposta" significativa para medir numa turbina ja parada) -- so'
  ## os elegiveis (>= cutin_rpm) entram no join caro abaixo
  eligible_ids <- start_rpm_dt[below_cutin == FALSE, curtailment_id]
  dt_eligible  <- dt[curtailment_id %in% eligible_ids]

  windows <- merge(
    dt_eligible[, .(curtailment_id, turbine, window_start = start,
                    window_end = end + buffer_after_end_sec)],
    start_rpm_dt, by = "curtailment_id"
  )

  rpm_dt <- scada_dt[readingname == "RPM", .(turbine = turbinelabel, datetime, rpm = value)]

  hits <- if (nrow(windows) == 0L) {
    data.table::data.table(curtailment_id = integer(), decline_time = as.POSIXct(character()), latency_sec = numeric())
  } else {
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

    h <- rpm_window[
      !is.na(decline_pct) & decline_pct >= decline_pct_threshold,
      .(decline_time = min(datetime), window_start = first(window_start)),
      by = curtailment_id
    ]
    ## difftime() vetoriza corretamente mesmo com 0 linhas (nenhuma queda
    ## detetada em nenhum curtailment) -- sem guarda nrow()>0 necessaria,
    ## ao contrario da 1a versao deste ficheiro (que deixava a coluna
    ## latency_sec por criar nesse caso, e o merge() mais abaixo falhava)
    h[, latency_sec := as.numeric(difftime(decline_time, window_start, units = "secs"))]
    h
  }

  ## merge all.x sobre TODOS os curtailments (nao so' dt_valid) -- quem nao
  ## tem entrada em start_rpm_dt fica com start_rpm/below_cutin = NA,
  ## marcado no_data abaixo em vez de desaparecer
  out <- merge(
    dt[, .(curtailment_id, turbine, track_id, species, start, end)],
    start_rpm_dt, by = "curtailment_id", all.x = TRUE
  )
  out[, no_data := is.na(start_rpm)]
  out <- merge(out, hits[, .(curtailment_id, decline_time, latency_sec)], by = "curtailment_id", all.x = TRUE)
  data.table::setcolorder(out, c(
    "curtailment_id", "turbine", "track_id", "species", "start", "end",
    "start_rpm", "no_data", "below_cutin", "decline_time", "latency_sec"
  ))
  data.table::setorder(out, curtailment_id)
  out[]
}


## 2. Resumo farm-wide -- 1 linha, com n reached/no-response e media/mediana ----
##
## n_curtailments = universo TOTAL (todos os curtailments passados a
## time_to_first_decline(), incluindo os sem baseline). n_no_data/pct_no_data
## (% de n_curtailments, como pct_of_total em summarise_response_by_flag())
## sao os sem leitura SCADA fiavel no start -- nao sabemos o que aconteceu,
## por isso ficam de fora de TODAS as restantes contagens (nem reached, nem
## no-response, nem below_cutin), visiveis mas nao assumidos. n_known =
## n_curtailments - n_no_data e' o universo com baseline conhecido;
## n_below_cutin/pct_below_cutin sao % de n_known. n_eligible = n_known -
## n_below_cutin e' o universo realmente avaliado; n_reached/pct_reached e
## n_no_response/pct_no_response sao sempre % de n_eligible -- e' a
## definicao de "no-response event" usada na secção "No-Response Events" do
## relatorio: nenhuma queda >= decline_pct_threshold detetada dentro da
## janela de procura, numa turbina com baseline conhecido e acima do cut-in
## no "start" (nem sequer uma resposta parcial, e nao um artefacto de
## turbina ja parada ou sem dado).

summarise_latency <- function(latency_dt) {

  n_total       <- nrow(latency_dt)
  n_no_data     <- sum(latency_dt$no_data)
  n_known       <- n_total - n_no_data
  n_below_cutin <- sum(latency_dt$below_cutin, na.rm = TRUE)
  n_eligible    <- n_known - n_below_cutin
  n_reached     <- sum(!is.na(latency_dt$latency_sec))

  data.table::data.table(
    n_curtailments     = n_total,
    n_no_data          = n_no_data,
    pct_no_data        = if (n_total == 0L) NA_real_ else round(100 * n_no_data / n_total, 1),
    n_known            = n_known,
    n_below_cutin      = n_below_cutin,
    pct_below_cutin    = if (n_known == 0L) NA_real_ else round(100 * n_below_cutin / n_known, 1),
    n_eligible         = n_eligible,
    n_reached          = n_reached,
    pct_reached        = if (n_eligible == 0L) NA_real_ else round(100 * n_reached / n_eligible, 1),
    ## NA (nao 0) quando n_eligible==0 -- "0" seria lido como "0 curtailments
    ## sem resposta, de X avaliados", mas aqui X tambem e' 0 (nada para
    ## avaliar, nao uma resposta confirmada). Relevante sobretudo com
    ## amostras pequenas (ex: por incidente, secção "Curtailment Response
    ## Around Each Incident") -- ver a coluna Curtailments (n_eligible) ao
    ## lado para distinguir os dois casos.
    n_no_response      = if (n_eligible == 0L) NA_integer_ else n_eligible - n_reached,
    pct_no_response    = if (n_eligible == 0L) NA_real_ else round(100 * (n_eligible - n_reached) / n_eligible, 1),
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
    n_no_data     <- sum(no_data)
    n_known       <- .N - n_no_data
    n_below_cutin <- sum(below_cutin, na.rm = TRUE)
    n_eligible    <- n_known - n_below_cutin
    n_reached     <- sum(!is.na(latency_sec))
    .(
      n_curtailments     = .N,
      n_no_data          = n_no_data,
      pct_no_data        = round(100 * n_no_data / .N, 1),
      n_known            = n_known,
      n_below_cutin      = n_below_cutin,
      pct_below_cutin    = if (n_known == 0L) NA_real_ else round(100 * n_below_cutin / n_known, 1),
      n_eligible         = n_eligible,
      n_reached          = n_reached,
      pct_reached        = if (n_eligible == 0L) NA_real_ else round(100 * n_reached / n_eligible, 1),
      ## ver nota em summarise_latency() -- NA (nao 0) quando n_eligible==0
      n_no_response      = if (n_eligible == 0L) NA_integer_ else n_eligible - n_reached,
      pct_no_response    = if (n_eligible == 0L) NA_real_ else round(100 * (n_eligible - n_reached) / n_eligible, 1),
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
                                       start_end_gap_sec = 2, buffer_after_end_sec = 0, cutin_rpm = 0) {

  data.table::rbindlist(lapply(decline_pct_candidates, function(p) {
    lat_dt <- time_to_first_decline(
      curtl_dt, scada_dt, decline_pct_threshold = p,
      start_end_gap_sec = start_end_gap_sec, buffer_after_end_sec = buffer_after_end_sec, cutin_rpm = cutin_rpm
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


## 6. Exemplos para plot -- "no_response": sem deteção, com baseline
##    conhecido (no_data == FALSE) E acima do cut-in (exclui curtailments
##    sem dado suficiente para avaliar e os ja abaixo da velocidade de
##    cut-in -- nenhum dos dois e' uma falha de resposta confirmada), os
##    mais recentes primeiro; "slowest": maior latencia registada, entre os
##    que tiveram deteção. Mesmo padrao de select_curtailment_examples()
##    (R/curtailment_forensic_trace.R), adaptado a latency_dt (que nao tem
##    coluna response_flag). ----

select_latency_examples <- function(latency_dt, type = c("no_response", "slowest"), n = 3) {

  type <- match.arg(type)
  dt <- if (type == "no_response") {
    latency_dt[is.na(latency_sec) & no_data == FALSE & below_cutin == FALSE]
  } else {
    latency_dt[!is.na(latency_sec)]
  }
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
