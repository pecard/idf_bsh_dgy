##
## Trace forense de curtailments, por turbina e dia
##
## Reconstroi, para uma turbina e um dia especificos, a sequencia de eventos
## de cada curtailment: quando o track (a ave) comecou a ser seguido, quando
## o curtailment disparou/terminou, o tempo ate 2rpm/1rpm (shutdown time), e
## as metricas de distancia de seguranca (curtailment_safe_distance.R).
##
## Uso pensado: sob-demanda, para investigar um incidente especifico (nao
## corre automaticamente no IDF_analysis.R para todo o periodo) -- ex: apos
## a colisao de uma especie prioritaria, para comparar visualmente com o
## portal do IdentiFlight (perfil de RPM com curtailments sobrepostos).
##
## Depende de: data.table, plotly, R/curtailment_response.R,
## R/curtailment_shutdown_time.R (usa time_to_rpm_thresholds()),
## R/curtailment_safe_distance.R (usa compute_safe_distance())
##
## Timezone: tz e' derivado automaticamente dos proprios dados (tzone da
## coluna start/datetime), nao ha default "UTC" a martelo -- os 4 datasets
## principais ja vem convertidos para proj_timezone (Asia/Samarkand) desde a
## leitura (ver read_*_data()), por isso basta nao interferir. Um default
## "UTC" aqui desalinhava a fronteira do dia em +5h (Asia/Samarkand = UTC+5),
## fazendo um curtailment das 6:50 local aparecer a cair no dia/janela errados.
##
## Uso:
##   source("R/curtailment_response.R")
##   source("R/curtailment_shutdown_time.R")
##   source("R/curtailment_safe_distance.R")
##   source("R/curtailment_forensic_trace.R")
##
##   find_high_curtailment_days(curtl_dt, "BSH54", min_curtailments = 5)
##
##   tt_dt       <- time_to_rpm_thresholds(curtl_scada_dt, scada_dt, thresholds = c(2, 1, 0))
##   safe_dist_dt <- compute_safe_distance(curtl_scada_dt, scada_dt, track_dt)
##   forensic_dt <- build_forensic_trace("BSH54", "2026-04-12", curtl_scada_dt, tt_dt, safe_dist_dt, track_dt)
##
##   plot_forensic_rpm(forensic_dt, scada_dt, "BSH54", "2026-04-12")
##


## 1. Dias candidatos -- turbinas/dias com muitos curtailments, para escolher o que investigar ----

find_high_curtailment_days <- function(curtl_dt, turbine_id, min_curtailments = 5, tz = NULL) {

  if (is.null(tz)) tz <- attr(curtl_dt$start, "tzone")

  dt <- curtl_dt[turbine == turbine_id]
  dt[, day := as.Date(start, tz = tz)]

  out <- dt[, .(n_curtailments = .N), by = day]
  out <- out[n_curtailments >= min_curtailments]
  data.table::setorder(out, -n_curtailments)
  out[]
}


## 2. Tabela forense -- 1 linha por curtailment, turbina+dia ----
##
## Base/backbone = curtl_dt (TODOS os curtailments desse turbina+dia, sem
## filtro de fiabilidade) -- tt_dt/safe_dist_dt sao feitos LEFT JOIN por cima,
## por (track_id, start). tt_dt (time_to_rpm_thresholds()) exclui de proposito
## curtailments sem leitura de SCADA fiavel perto do start (start_end_gap_sec,
## por omissao 2s -- ver R/curtailment_shutdown_time.R); isso e' correto para
## as metricas agregadas (3.6/3.7), mas para uma tabela forense TODOS os
## curtailments desse dia tem de aparecer -- os sem match fiavel ficam com
## start_rpm/time_to_Xrpm_sec (e por arrasto as colunas de safe_dist_dt) a NA,
## em vez de a linha inteira desaparecer.
##
## Chave composta (track_id, start), nao so track_id: o mesmo track_id pode,
## em teoria, disparar mais do que um curtailment nesse turbina+dia.

build_forensic_trace <- function(turbine_id, date, curtl_dt, tt_dt, safe_dist_dt, track_dt, tz = NULL) {

  if (is.null(tz)) tz <- attr(curtl_dt$start, "tzone")
  date <- as.Date(date)

  base_dt <- curtl_dt[
    turbine == turbine_id & as.Date(start, tz = tz) == date,
    .(turbine, track_id, species, start, end)
  ]

  if (nrow(base_dt) == 0L) return(base_dt)

  tt_sub <- tt_dt[turbine == turbine_id & track_id %in% base_dt$track_id]

  if (nrow(tt_sub) > 0L) {
    tt_wide <- data.table::dcast(
      tt_sub, track_id + start + start_rpm ~ threshold, value.var = "time_to_threshold_sec"
    )
    id_cols  <- c("track_id", "start", "start_rpm")
    val_cols <- setdiff(names(tt_wide), id_cols)
    data.table::setnames(tt_wide, val_cols, paste0("time_to_", val_cols, "rpm_sec"))
  } else {
    tt_wide <- data.table::data.table(track_id = character(), start = base_dt$start[0])
  }

  out <- merge(base_dt, tt_wide, by = c("track_id", "start"), all.x = TRUE)

  track_started_dt <- track_dt[, .(track_started = min(timestamp)), by = track_id]
  out <- merge(out, track_started_dt, by = "track_id", all.x = TRUE)

  sd_cols <- safe_dist_dt[
    , .(track_id, start, x2d, avg_speed_ms, min_safe_dist_m, dist_margin_m, status, turbine_state)
  ]
  out <- merge(out, sd_cols, by = c("track_id", "start"), all.x = TRUE)

  data.table::setorder(out, start)
  out[]
}


## 3. Plot RPM (estilo portal IdentiFlight, interativo) -- curva de RPM com
##    curtailments sobrepostos, em plotly para permitir consultar com o rato
##    o timestamp/RPM da curva e o track_id/especie/start/end/status de cada
##    curtailment ----
##    forensic_dt: resultado de build_forensic_trace() (pode ser de varios dias/turbinas,
##    a funcao filtra por turbine_id + [t_ini, t_end])

plot_forensic_rpm <- function(forensic_dt, scada_dt, turbine_id, date,
                              t_ini = NULL, t_end = NULL, tz = NULL) {

  if (is.null(tz)) tz <- attr(scada_dt$datetime, "tzone")
  date <- as.Date(date)
  if (is.null(t_ini)) t_ini <- as.POSIXct(paste(date, "00:00:00"), tz = tz)
  if (is.null(t_end)) t_end <- as.POSIXct(paste(date, "23:59:59"), tz = tz)

  rpm_dt <- scada_dt[
    turbinelabel == turbine_id & readingname == "RPM" & datetime >= t_ini & datetime <= t_end
  ]
  events <- forensic_dt[turbine == turbine_id & start >= t_ini & start <= t_end]

  y_max <- max(c(rpm_dt$value, 2, 1, 0), na.rm = TRUE) * 1.1
  y_min <- -0.5

  p <- plotly::plot_ly()

  # barra translucida (duracao do curtailment) + linha vertical no start, cada
  # uma com hover mostrando track_id/especie/start/end/status -- um par de
  # traces por curtailment (poucos por dia, sem problema de desempenho)
  if (nrow(events) > 0L) {
    for (i in seq_len(nrow(events))) {
      ev <- events[i]
      hover_label <- sprintf(
        "%s (%s)<br>start: %s<br>end: %s<br>status: %s",
        ev$track_id, ev$species, format(ev$start, "%H:%M:%S"), format(ev$end, "%H:%M:%S"),
        if (is.na(ev$status)) "sem match RPM fiavel" else ev$status
      )
      p <- p %>% plotly::add_trace(
        x = c(ev$start, ev$end, ev$end, ev$start), y = c(y_min, y_min, y_max, y_max),
        type = "scatter", mode = "none", fill = "toself",
        fillcolor = "rgba(90,90,90,0.22)", hoverinfo = "text", text = hover_label,
        showlegend = FALSE
      )
      p <- p %>% plotly::add_trace(
        x = c(ev$start, ev$start), y = c(y_min, y_max),
        type = "scatter", mode = "lines", line = list(color = "grey30", width = 1.5),
        hoverinfo = "text", text = hover_label, showlegend = FALSE
      )
    }
  }

  # curva de RPM -- hover com timestamp exato e valor de RPM
  p <- p %>% plotly::add_trace(
    data = rpm_dt, x = ~datetime, y = ~value, type = "scatter", mode = "lines",
    line = list(color = "#e8792f", width = 1.5), name = "RPM",
    hovertemplate = "%{x|%Y-%m-%d %H:%M:%S}<br>RPM: %{y:.2f}<extra></extra>"
  )

  # marcadores dos momentos em que a turbina atingiu 2rpm/1rpm (cruzamento
  # visual direto com os numeros de shutdown_time que estamos a validar contra o portal)
  if ("time_to_2rpm_sec" %in% names(events)) {
    ev2 <- events[!is.na(time_to_2rpm_sec)]
    if (nrow(ev2) > 0L) {
      p <- p %>% plotly::add_trace(
        data = ev2, x = ~(start + time_to_2rpm_sec), y = 2, type = "scatter", mode = "markers",
        marker = list(symbol = "x", size = 9, color = "steelblue"), name = "2 rpm",
        text = ~sprintf("%s<br>2rpm em +%.1fs", track_id, time_to_2rpm_sec), hoverinfo = "text"
      )
    }
  }
  if ("time_to_1rpm_sec" %in% names(events)) {
    ev1 <- events[!is.na(time_to_1rpm_sec)]
    if (nrow(ev1) > 0L) {
      p <- p %>% plotly::add_trace(
        data = ev1, x = ~(start + time_to_1rpm_sec), y = 1, type = "scatter", mode = "markers",
        marker = list(symbol = "x", size = 9, color = "darkred"), name = "1 rpm",
        text = ~sprintf("%s<br>1rpm em +%.1fs", track_id, time_to_1rpm_sec), hoverinfo = "text"
      )
    }
  }

  p %>% plotly::layout(
    title = sprintf("%s -- RPM (%s)", turbine_id, format(date)),
    xaxis = list(title = ""),
    yaxis = list(title = "RPM", range = c(y_min, y_max)),
    showlegend = TRUE
  )
}
