##
## Tempo de resposta / falha de resposta ao curtailment, via SCADA (RPM)
##
## Para cada sinal de curtailment (start/end), encontra a leitura SCADA (RPM)
## mais proxima no tempo, da mesma turbina, via roll join -- mas com um limite
## de tolerancia EXPLICITO (`max_gap_sec`), ao contrario do update-join
## "nearest" sem limite usado no script original
## (scripts_IDF/curtailments_scada_roll_join.R), que nao tinha tolerancia e
## podia perder eventos silenciosamente quando 2+ eventos "casavam" com a
## mesma leitura SCADA (o update-join so guarda o ultimo a ser processado).
##
## Aqui, o join devolve sempre uma linha nova por evento (nunca apaga/
## sobrescreve), por isso nao ha esse risco de perda.
##
## Depois, para cada curtailment, monitoriza o RPM numa JANELA FIXA apos o
## sinal de start (`monitor_window_sec`) -- nao a duracao toda do curtailment
## -- e classifica a resposta da turbina em 4 categorias:
##   - already_stopped : ja estava abaixo do limiar no sinal de start
##   - responded        : desceu abaixo do limiar dentro da janela -> regista o tempo
##   - no_response       : NUNCA desceu abaixo do limiar dentro da janela (falha grave)
##   - no_data           : nao ha leituras SCADA suficientes na janela para decidir
## "no_response" e "no_data" ficam sempre distintos -- nunca se confundem no
## mesmo NA, ao contrario do script original.
##
## Depende de: data.table
##
## Uso (janela completa, tolerancia mais larga):
##   source("R/curtailment_response.R")
##   response_dt <- classify_curtailment_response(
##     curtl_dt, scada_dt,
##     monitor_window_sec = 90,
##     rpm_threshold = safe_shutdown_rpm,  # definido em userSettings_BSH.R
##     max_gap_sec = 15
##   )
##   summary <- summarise_curtailment_response(response_dt)
##   summary$by_status
##   summary$by_turbine
##
## Uso (resposta imediata + delta start->end, tolerancia apertada -- ver
## assess_curtailment_response() abaixo para o racional):
##   assess_dt <- assess_curtailment_response(
##     curtl_dt, scada_dt,
##     start_end_gap_sec = 2,
##     max_next_gap_sec = 20,
##     drop_pct_threshold = 0.10,
##     rpm_threshold = safe_shutdown_rpm
##   )
##   summary2 <- summarise_curtailment_assessment(assess_dt)
##


## 1. Match "nearest" com tolerancia explicita, para um conjunto de eventos ----
##    events_dt precisa de: id, turbine, event_time
##    scada_dt precisa de: turbinelabel, datetime, value, readingname

## roll (novo, 2026-08, ver discussao start_end_gap_sec em
## R/curtailment_response_latency.R): por omissao "nearest" (leitura mais
## proxima no tempo, antes OU depois do evento -- comportamento original,
## mantido para todos os usos existentes). Quando usado para o BASELINE de
## um curtailment (o "start"), um numero positivo (ex: roll = max_gap_sec)
## restringe a LOCF -- so' aceita leitura a <= esse numero de segundos ANTES
## do evento, nunca depois. Isto elimina, por construcao, o risco de o
## baseline ja refletir uma desaceleracao real causada pelo proprio
## curtailment (que so' pode acontecer numa leitura POSTERIOR ao sinal) --
## permite relaxar a tolerancia (menos no_data) sem contaminar o baseline.
match_nearest_rpm <- function(events_dt, scada_dt, max_gap_sec = 15, roll = "nearest") {

  rpm_dt <- scada_dt[readingname == "RPM", .(turbine = turbinelabel, datetime, rpm = value)]
  setkey(rpm_dt, turbine, datetime)

  matched <- rpm_dt[
    events_dt,
    on = .(turbine, datetime = event_time),
    roll = roll,
    .(
      id             = i.id,
      turbine        = i.turbine,
      event_time     = i.event_time,
      scada_datetime = x.datetime,
      rpm            = x.rpm
    )
  ]

  matched[, gap_sec := as.numeric(abs(difftime(scada_datetime, event_time, units = "secs")))]

  # marcar (nao apagar) os matches fora da tolerancia -- fica visivel para log/QA
  matched[, valid_match := !is.na(gap_sec) & gap_sec <= max_gap_sec]
  matched[valid_match == FALSE, `:=`(scada_datetime = as.POSIXct(NA), rpm = NA_real_)]

  matched[]
}


## 2. Eventos start/end de cada curtailment, para usar com match_nearest_rpm() ----
##    Usa um curtailment_id explicito para ligar start<->end -- mais robusto
##    que o pivot_longer + group_by(status) + row_number() do script original,
##    que dependia da ordem das linhas se manter alinhada entre os 2 grupos.

build_curtailment_events <- function(curtl_dt) {
  dt <- as.data.table(curtl_dt)
  dt[, curtailment_id := .I]

  rbindlist(list(
    dt[, .(id = curtailment_id, turbine, event_type = "start", event_time = start)],
    dt[, .(id = curtailment_id, turbine, event_type = "end",   event_time = end)]
  ))
}


## 3. Classificar a resposta de cada curtailment ----

classify_curtailment_response <- function(curtl_dt, scada_dt,
                                          monitor_window_sec = 90,
                                          rpm_threshold = 1,
                                          max_gap_sec = 15) {

  dt <- as.data.table(curtl_dt)
  dt[, curtailment_id := .I]

  ## RPM mais proximo do sinal de start (ja estava parada?)
  start_events <- dt[, .(id = curtailment_id, turbine, event_time = start)]
  start_match  <- match_nearest_rpm(start_events, scada_dt, max_gap_sec)
  setnames(
    start_match,
    c("event_time", "scada_datetime", "rpm", "gap_sec", "valid_match"),
    c("start_time", "start_scada_datetime", "start_rpm", "start_gap_sec", "start_match_valid")
  )

  ## Todas as leituras RPM na janela [start, start + monitor_window_sec]
  rpm_dt <- scada_dt[readingname == "RPM", .(turbine = turbinelabel, datetime, rpm = value)]

  windows <- dt[, .(curtailment_id, turbine,
                    window_start = start,
                    window_end   = start + monitor_window_sec)]

  rpm_window <- rpm_dt[
    windows,
    on = .(turbine, datetime >= window_start, datetime <= window_end),
    allow.cartesian = TRUE,
    .(curtailment_id = i.curtailment_id, window_start = i.window_start,
      datetime = x.datetime, rpm = x.rpm)
  ]

  # numero de leituras por curtailment (inclui os que tem 0, para distinguir no_data)
  n_readings_dt <- rpm_window[, .(n_readings_in_window = sum(!is.na(rpm))), by = curtailment_id]

  # 1o instante, dentro da janela, com rpm abaixo do limiar -- window_start
  # vem do MESMO grupo (first(), nao de um merge() separado depois -- e o
  # padrao que confirmamos funcionar bem em assess_curtailment_response())
  drop_times_dt <- rpm_window[
    !is.na(rpm) & rpm < rpm_threshold,
    .(first_drop_time = min(datetime), window_start = first(window_start)),
    by = curtailment_id
  ]
  drop_times_dt[, time_to_drop_sec := as.numeric(difftime(first_drop_time, window_start, units = "secs"))]

  drop_summary <- merge(
    n_readings_dt,
    drop_times_dt[, .(curtailment_id, first_drop_time, time_to_drop_sec)],
    by = "curtailment_id", all.x = TRUE
  )

  ## Combinar tudo ----
  result <- merge(
    dt[, .(curtailment_id, turbine, track_id, species, start, end)],
    start_match[, .(curtailment_id = id, start_scada_datetime, start_rpm, start_gap_sec, start_match_valid)],
    by = "curtailment_id", all.x = TRUE
  )
  result <- merge(result, drop_summary[, .(curtailment_id, n_readings_in_window, first_drop_time, time_to_drop_sec)],
                  by = "curtailment_id", all.x = TRUE)

  ## Classificacao -- "already_stopped" tem prioridade (independente da janela);
  ## depois "no_data" (sem leituras); so depois se decide responded vs no_response
  result[, response_status := fcase(
    !is.na(start_rpm) & start_match_valid & start_rpm < rpm_threshold, "already_stopped",
    is.na(n_readings_in_window) | n_readings_in_window == 0,           "no_data",
    !is.na(first_drop_time),                                           "responded",
    default = "no_response"
  )]

  result[]
}


## 4. Resumo -- global e por turbina (para detetar turbinas problematicas) ----

summarise_curtailment_response <- function(response_dt) {

  by_status <- response_dt[, .(n = .N), by = response_status]
  by_status[, pct := round(100 * n / sum(n), 1)]

  by_turbine <- response_dt[, .(
    n_curtailments        = .N,
    n_responded           = sum(response_status == "responded"),
    n_no_response          = sum(response_status == "no_response"),
    n_already_stopped      = sum(response_status == "already_stopped"),
    n_no_data              = sum(response_status == "no_data"),
    mean_time_to_drop_sec  = mean(time_to_drop_sec[response_status == "responded"], na.rm = TRUE)
  ), by = turbine]

  by_turbine[, no_response_pct := round(100 * n_no_response / n_curtailments, 1)]
  setorder(by_turbine, -no_response_pct)

  list(by_status = by_status[], by_turbine = by_turbine[])
}


## 5. Avaliacao de resposta -- baseline apertado + verificacao imediata + delta ----
##
## Diferenca face a classify_curtailment_response(): em vez de associar o RPM
## do "start" a uma leitura ate 15s de distancia (o que, com uma turbina a
## demorar 50-110s a parar, pode ja refletir alguma desaceleracao real, nao o
## estado no instante exato do sinal), esta versao:
##
##   1. So aceita o RPM no start/end se a leitura SCADA estiver a <= 2s
##      (start_end_gap_sec) -- caso contrario fica NA, nao arriscamos.
##   2. Vai buscar especificamente a PROXIMA leitura SCADA a seguir ao start
##      (~10s depois, o proximo "tick" do SCADA) e verifica se ja caiu pelo
##      menos `drop_pct_threshold` (10% por omissao) face ao baseline --
##      se nao caiu, `no_immediate_response = TRUE` (flag critica: a turbina
##      nao chegou sequer a começar a reagir no primeiro tick).
##   3. Calcula tambem o RPM no `end` do curtailment (mesma tolerancia
##      apertada) e o delta face ao start, para avaliar a queda global,
##      independentemente da resposta imediata.
##
## `no_immediate_response` e `final_status` sao sinais complementares --
## uma turbina pode reagir devagar (no_immediate_response=TRUE) mas ainda
## assim parar a tempo (final_status="full_stop_by_end").

assess_curtailment_response <- function(curtl_dt, scada_dt,
                                        start_end_gap_sec = 2,
                                        max_next_gap_sec = 20,
                                        drop_pct_threshold = 0.10,
                                        rpm_threshold = 1) {

  dt <- as.data.table(curtl_dt)
  dt[, curtailment_id := .I]

  ## 1) RPM no start e no end, com tolerancia apertada ----
  start_events <- dt[, .(id = curtailment_id, turbine, event_time = start)]
  end_events   <- dt[, .(id = curtailment_id, turbine, event_time = end)]

  start_match <- match_nearest_rpm(start_events, scada_dt, max_gap_sec = start_end_gap_sec)
  end_match   <- match_nearest_rpm(end_events,   scada_dt, max_gap_sec = start_end_gap_sec)

  setnames(
    start_match,
    c("event_time", "scada_datetime", "rpm", "gap_sec", "valid_match"),
    c("start_time", "start_scada_datetime", "start_rpm", "start_gap_sec", "start_match_valid")
  )
  setnames(
    end_match,
    c("event_time", "scada_datetime", "rpm", "gap_sec", "valid_match"),
    c("end_time", "end_scada_datetime", "end_rpm", "end_gap_sec", "end_match_valid")
  )

  ## 2) Primeira leitura SCADA a seguir ao start (o "proximo tick", ~10s depois) ----
  rpm_dt <- scada_dt[readingname == "RPM", .(turbine = turbinelabel, datetime, rpm = value)]
  setkey(rpm_dt, turbine, datetime)

  starts <- dt[, .(curtailment_id, turbine, start)]

  next_reading <- rpm_dt[
    starts,
    on = .(turbine, datetime > start),
    mult = "first",
    .(curtailment_id = i.curtailment_id, start = i.start, next_datetime = x.datetime, next_rpm = x.rpm)
  ]
  next_reading[, next_gap_sec := as.numeric(difftime(next_datetime, start, units = "secs"))]

  # leitura "seguinte" demasiado longe para ser fiavel (buraco de dados logo a
  # seguir ao sinal) -- nao arriscamos usa-la para a verificacao imediata
  next_reading[
    !is.na(next_gap_sec) & next_gap_sec > max_next_gap_sec,
    `:=`(next_datetime = as.POSIXct(NA), next_rpm = NA_real_, next_gap_sec = NA_real_)
  ]

  ## 3) Combinar tudo ----
  result <- merge(
    dt[, .(curtailment_id, turbine, track_id, species, start, end)],
    start_match[, .(curtailment_id = id, start_scada_datetime, start_rpm, start_gap_sec, start_match_valid)],
    by = "curtailment_id", all.x = TRUE
  )
  result <- merge(
    result,
    end_match[, .(curtailment_id = id, end_scada_datetime, end_rpm, end_gap_sec, end_match_valid)],
    by = "curtailment_id", all.x = TRUE
  )
  result <- merge(
    result,
    next_reading[, .(curtailment_id, next_datetime, next_rpm, next_gap_sec)],
    by = "curtailment_id", all.x = TRUE
  )

  # duracao da propria ordem de curtailment (end - start) -- diagnostico para
  # investigar se "partial_or_no_stop" esta a confundir "nao respondeu" com
  # "a ordem de curtailment durou menos tempo do que o necessario para parar"
  # (ver shutdown_time_high_cut, secção 7 -- se muitos "partial_or_no_stop"
  # tiverem duracao curta, o problema pode ser o denominador/definicao, nao
  # uma falha real de resposta). Ver summarise_curtailment_assessment().
  result[, duration_sec := as.numeric(difftime(end, start, units = "secs"))]

  ## 4) Sinal 1: resposta imediata (queda >= drop_pct_threshold na leitura seguinte) ----
  result[, immediate_drop_pct := fifelse(
    !is.na(start_rpm) & start_rpm > 0 & !is.na(next_rpm),
    (start_rpm - next_rpm) / start_rpm,
    NA_real_
  )]

  result[, no_immediate_response := fcase(
    is.na(immediate_drop_pct), NA,
    immediate_drop_pct < drop_pct_threshold, TRUE,
    default = FALSE
  )]

  ## 5) Sinal 2: delta start->end e estado final ----
  result[, rpm_delta := start_rpm - end_rpm]
  result[, rpm_delta_pct := fifelse(
    !is.na(start_rpm) & start_rpm > 0 & !is.na(end_rpm),
    rpm_delta / start_rpm,
    NA_real_
  )]

  result[, final_status := fcase(
    !is.na(start_rpm) & start_match_valid & start_rpm < rpm_threshold, "already_stopped",
    !is.na(end_rpm) & end_match_valid & end_rpm < rpm_threshold,       "full_stop_by_end",
    !is.na(end_rpm) & end_match_valid,                                 "partial_or_no_stop",
    default = "no_data"
  )]

  result[]
}


## 6. Resumo -- global e por turbina, para assess_curtailment_response() ----

summarise_curtailment_assessment <- function(assess_dt) {

  by_status <- assess_dt[, .(n = .N), by = final_status]
  by_status[, pct := round(100 * n / sum(n), 1)]
  # % sobre os curtailments com match valido (exclui no_data) -- da o peso
  # real do partial_or_no_stop ("missed curtailments") sem ser diluido pelo
  # no_data, que pode ser a maioria dos casos e esconder a taxa de falha
  n_valid_status <- sum(by_status[final_status != "no_data", n])
  by_status[, pct_of_valid := fifelse(
    final_status == "no_data", NA_real_, round(100 * n / n_valid_status, 1)
  )]

  # diagnostico -- duracao media/mediana da ordem de curtailment (end-start),
  # por status final. Se "partial_or_no_stop" tiver duracao sistematicamente
  # mais curta que "full_stop_by_end", a leitura de "missed curtailment" pode
  # estar a confundir ordens de curtailment curtas (a ave afastou-se depressa,
  # a ordem terminou antes de haver tempo para parar) com falhas reais de
  # resposta -- comparar com shutdown_time_high_cut (secção 7) antes de tirar
  # conclusoes so' a partir desta tabela.
  by_status_duration <- assess_dt[, .(
    mean_duration_sec   = round(mean(duration_sec, na.rm = TRUE), 1),
    median_duration_sec = round(median(duration_sec, na.rm = TRUE), 1)
  ), by = final_status]
  by_status <- merge(by_status, by_status_duration, by = "final_status")

  # no_data por ultimo (nao e' um "pior caso" na mesma escala dos outros 3,
  # e' ausencia de informacao) -- os outros 3 mantem a ordem de gravidade
  # crescente definida em assess_curtailment_response()
  status_order <- c("already_stopped", "full_stop_by_end", "partial_or_no_stop", "no_data")
  by_status[, status_rank := match(final_status, status_order)]
  data.table::setorder(by_status, status_rank)
  by_status[, status_rank := NULL]

  by_immediate <- assess_dt[, .(n = .N), by = no_immediate_response]
  by_immediate[, pct := round(100 * n / sum(n), 1)]
  # % sobre os casos em que foi possivel determinar (exclui NA)
  n_determined <- sum(by_immediate[!is.na(no_immediate_response), n])
  by_immediate[, pct_of_determined := fifelse(
    is.na(no_immediate_response), NA_real_, round(100 * n / n_determined, 1)
  )]

  by_turbine <- assess_dt[, .(
    n_curtailments          = .N,
    n_no_immediate_response = sum(no_immediate_response, na.rm = TRUE),
    n_already_stopped       = sum(final_status == "already_stopped"),
    n_full_stop_by_end      = sum(final_status == "full_stop_by_end"),
    n_partial_or_no_stop    = sum(final_status == "partial_or_no_stop"),
    n_no_data               = sum(final_status == "no_data"),
    mean_rpm_delta_pct      = mean(rpm_delta_pct, na.rm = TRUE)
  ), by = turbine]

  by_turbine[, n_valid_matches := n_curtailments - n_no_data]
  by_turbine[, no_immediate_response_pct := round(100 * n_no_immediate_response / n_curtailments, 1)]
  # "missed curtailments" -- partial_or_no_stop sobre o universo com match
  # valido (exclui no_data), por turbina
  by_turbine[, missed_pct := fifelse(
    n_valid_matches > 0, round(100 * n_partial_or_no_stop / n_valid_matches, 1), NA_real_
  )]
  setorder(by_turbine, -missed_pct)

  list(by_status = by_status[], by_immediate = by_immediate[], by_turbine = by_turbine[])
}
