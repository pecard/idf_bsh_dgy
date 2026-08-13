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
## Depende de: data.table, ggplot2, R/curtailment_response.R,
## R/curtailment_shutdown_time.R (usa time_to_rpm_thresholds()),
## R/curtailment_safe_distance.R (usa compute_safe_distance())
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
##   forensic_dt <- build_forensic_trace("BSH54", "2026-04-12", tt_dt, safe_dist_dt, track_dt)
##
##   plot_forensic_rpm(forensic_dt, scada_dt, "BSH54", "2026-04-12")
##


## 1. Dias candidatos -- turbinas/dias com muitos curtailments, para escolher o que investigar ----

find_high_curtailment_days <- function(curtl_dt, turbine_id, min_curtailments = 5, tz = "UTC") {

  dt <- curtl_dt[turbine == turbine_id]
  dt[, day := as.Date(start, tz = tz)]

  out <- dt[, .(n_curtailments = .N), by = day]
  out <- out[n_curtailments >= min_curtailments]
  data.table::setorder(out, -n_curtailments)
  out[]
}


## 2. Tabela forense -- 1 linha por curtailment, turbina+dia ----

build_forensic_trace <- function(turbine_id, date, tt_dt, safe_dist_dt, track_dt, tz = "UTC") {

  date <- as.Date(date)

  tt_wide <- data.table::dcast(
    tt_dt[turbine == turbine_id],
    curtailment_id + turbine + track_id + species + start + end + start_rpm ~ threshold,
    value.var = "time_to_threshold_sec"
  )
  id_cols  <- c("curtailment_id", "turbine", "track_id", "species", "start", "end", "start_rpm")
  val_cols <- setdiff(names(tt_wide), id_cols)
  data.table::setnames(tt_wide, val_cols, paste0("time_to_", val_cols, "rpm_sec"))

  tt_wide <- tt_wide[as.Date(start, tz = tz) == date]

  if (nrow(tt_wide) == 0L) return(tt_wide)

  track_started_dt <- track_dt[, .(track_started = min(timestamp)), by = track_id]
  out <- merge(tt_wide, track_started_dt, by = "track_id", all.x = TRUE)

  sd_cols <- safe_dist_dt[, .(track_id, x2d, avg_speed_ms, min_safe_dist_m, dist_margin_m, status, turbine_state)]
  out <- merge(out, sd_cols, by = "track_id", all.x = TRUE)

  data.table::setorder(out, start)
  out[]
}


## 3. Plot RPM (estilo portal IdentiFlight) -- curva de RPM com curtailments sobrepostos ----
##    forensic_dt: resultado de build_forensic_trace() (pode ser de varios dias/turbinas,
##    a funcao filtra por turbine_id + [t_ini, t_end])

plot_forensic_rpm <- function(forensic_dt, scada_dt, turbine_id, date,
                              t_ini = NULL, t_end = NULL, tz = "UTC") {

  date <- as.Date(date)
  if (is.null(t_ini)) t_ini <- as.POSIXct(paste(date, "00:00:00"), tz = tz)
  if (is.null(t_end)) t_end <- as.POSIXct(paste(date, "23:59:59"), tz = tz)

  rpm_dt <- scada_dt[
    turbinelabel == turbine_id & readingname == "RPM" & datetime >= t_ini & datetime <= t_end
  ]
  events <- forensic_dt[turbine == turbine_id & start >= t_ini & start <= t_end]

  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = events, ggplot2::aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
      fill = "grey50", alpha = 0.25
    ) +
    ggplot2::geom_vline(
      data = events, ggplot2::aes(xintercept = start), colour = "grey30", linewidth = 0.6
    ) +
    ggplot2::geom_line(
      data = rpm_dt, ggplot2::aes(x = datetime, y = value), colour = "#e8792f", linewidth = 0.4
    )

  # marcadores dos momentos em que a turbina atingiu 2rpm/1rpm (cruzamento
  # visual direto com os numeros de shutdown_time que estamos a validar contra o portal)
  if ("time_to_2rpm_sec" %in% names(events)) {
    ev2 <- events[!is.na(time_to_2rpm_sec)]
    if (nrow(ev2) > 0L) {
      p <- p + ggplot2::geom_point(
        data = ev2, ggplot2::aes(x = start + time_to_2rpm_sec, y = 2),
        shape = 4, size = 2.2, stroke = 1, colour = "steelblue"
      )
    }
  }
  if ("time_to_1rpm_sec" %in% names(events)) {
    ev1 <- events[!is.na(time_to_1rpm_sec)]
    if (nrow(ev1) > 0L) {
      p <- p + ggplot2::geom_point(
        data = ev1, ggplot2::aes(x = start + time_to_1rpm_sec, y = 1),
        shape = 4, size = 2.2, stroke = 1, colour = "darkred"
      )
    }
  }

  p +
    ggplot2::labs(x = NULL, y = "RPM", title = sprintf("%s -- %s", turbine_id, format(date))) +
    ggplot2::theme_minimal()
}
