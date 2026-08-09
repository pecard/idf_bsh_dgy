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
## Uso:
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


## 1. Match "nearest" com tolerancia explicita, para um conjunto de eventos ----
##    events_dt precisa de: id, turbine, event_time
##    scada_dt precisa de: turbinelabel, datetime, value, readingname

match_nearest_rpm <- function(events_dt, scada_dt, max_gap_sec = 15) {

  rpm_dt <- scada_dt[readingname == "RPM", .(turbine = turbinelabel, datetime, rpm = value)]
  setkey(rpm_dt, turbine, datetime)

  matched <- rpm_dt[
    events_dt,
    on = .(turbine, datetime = event_time),
    roll = "nearest",
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
    .(curtailment_id = i.curtailment_id, window_start = i.window_start, datetime, rpm)
  ]

  # numero de leituras por curtailment (inclui os que tem 0, para distinguir no_data)
  n_readings_dt <- rpm_window[, .(n_readings_in_window = sum(!is.na(rpm))), by = curtailment_id]

  # 1o instante, dentro da janela, com rpm abaixo do limiar -- calculado a
  # parte (filtrar e so depois agregar), sem bloco condicional dentro do
  # agregado, para nao repetir o bug de time_to_drop_sec sair sempre 0
  drop_times_dt <- rpm_window[
    !is.na(rpm) & rpm < rpm_threshold,
    .(first_drop_time = min(datetime)),
    by = curtailment_id
  ]

  drop_summary <- merge(n_readings_dt, drop_times_dt, by = "curtailment_id", all.x = TRUE)
  drop_summary <- merge(drop_summary, windows[, .(curtailment_id, window_start)], by = "curtailment_id")
  drop_summary[, time_to_drop_sec := as.numeric(difftime(first_drop_time, window_start, units = "secs"))]

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
