##
## Disponibilidade (availability) das unidades IDF durante as horas de luz
##
## Cada unidade IDF envia um heartbeat a cada ~30 min quando esta
## operacional. Consideramos uma unidade "offline" quando o intervalo
## entre dois heartbeats consecutivos excede `offline_gap_min` (ou seja,
## falhou pelo menos 2 heartbeats seguidos). Os primeiros
## `online_grace_min` minutos desse intervalo sao contados como online
## (a unidade so se considera offline depois de "faltar" ao heartbeat
## seguinte seria esperado).
##
## A disponibilidade e avaliada apenas durante o "daylight time" (entre
## o nascer e o por do sol) de cada dia, calculado a partir da posicao
## (lat/lon) do projeto com o package suncalc.
##
## Depende de: data.table, lubridate, suncalc, ggplot2
##


## 1. Calendario de luz do dia (sunrise/sunset) para um intervalo de datas ----

build_daylight_calendar <- function(start_date, end_date, lat, lon, tz) {

  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")

  sun <- as.data.table(
    suncalc::getSunlightTimes(
      date = dates,
      lat  = lat,
      lon  = lon,
      keep = c("sunrise", "sunset"),
      tz   = tz
    )
  )

  sun[, daylight_mins := as.numeric(difftime(sunset, sunrise, units = "mins"))]

  sun[, .(date, sunrise, sunset, daylight_mins)]
}


## 2. Intervalos "offline" por unidade IDF, a partir dos heartbeats ----

compute_offline_intervals <- function(heartb_dt, offline_gap_min = 60, online_grace_min = 30) {

  hb <- copy(heartb_dt)
  setkeyv(hb, c("idf", "timestamp"))

  hb[, ts_prev := shift(timestamp, type = "lag"), by = idf]

  # 1º heartbeat de cada unidade: assume-se online nos `online_grace_min` min anteriores
  hb[is.na(ts_prev), ts_prev := timestamp - minutes(online_grace_min)]

  hb[, gap_mins := as.numeric(difftime(timestamp, ts_prev, units = "mins"))]

  hb <- unique(hb, by = key(hb))

  hb[gap_mins >= offline_gap_min, .(
    idf,
    off_start = ts_prev + minutes(online_grace_min),
    off_end   = timestamp
  )]
}


## 3. Sobrepor intervalos offline com as horas de luz de cada dia ----
##    -> minutos offline durante o dia, por unidade IDF e por dia

compute_daylight_offline <- function(offline_intervals, daylight_cal, tz) {

  if (nrow(offline_intervals) == 0L) {
    return(daylight_cal[, .(date, idf = NA_character_, offline_mins = 0,
                            daylight_mins, offline_pct = 0)][0])
  }

  off <- copy(offline_intervals)
  off[, `:=`(
    date_start = as.IDate(off_start, tz = tz),
    date_end   = as.IDate(off_end,   tz = tz)
  )]

  # expandir cada intervalo para uma linha por (intervalo x dia) que atravessa
  off_exp <- off[, .(date = seq(date_start, date_end, by = "day")),
                by = .(idf, off_start, off_end)]

  off_exp <- daylight_cal[off_exp, on = "date", nomatch = 0]

  off_exp[, day_start := as.POSIXct(date, tz = tz)]
  off_exp[, day_end   := day_start + days(1)]

  # sobreposicao entre [off_start, off_end] e [sunrise, sunset], dentro do dia
  off_exp[, start_num := pmax(as.numeric(off_start), as.numeric(day_start), as.numeric(sunrise))]
  off_exp[, end_num   := pmin(as.numeric(off_end),   as.numeric(day_end),   as.numeric(sunset))]
  off_exp[, offline_mins := pmax(0, (end_num - start_num) / 60)]

  daily_off <- off_exp[, .(offline_mins = sum(offline_mins)), by = .(idf, date)]

  res <- daylight_cal[daily_off, on = "date"]
  res[, offline_pct := fifelse(daylight_mins > 0, offline_mins / daylight_mins, NA_real_)]

  res
}


## 4. Grelha completa IDF x dia (dias sem registo offline = 0%) ----

daylight_availability <- function(heartb_dt, daylight_cal, tz,
                                  offline_gap_min = 60, online_grace_min = 30) {

  offline_intervals <- compute_offline_intervals(heartb_dt, offline_gap_min, online_grace_min)
  daily_offline <- compute_daylight_offline(offline_intervals, daylight_cal, tz)

  all_idf <- sort(unique(heartb_dt$idf))
  grid <- CJ(idf = all_idf, date = daylight_cal$date)

  res_full <- daily_offline[grid, on = .(idf, date)]
  res_full <- daylight_cal[res_full, on = "date"]

  # dias com luz do dia mas sem periodo offline registado -> 0 min offline
  res_full[is.na(offline_mins) & daylight_mins > 0, offline_mins := 0]
  res_full[, offline_pct := fifelse(daylight_mins > 0, offline_mins / daylight_mins, NA_real_)]
  res_full[, ym := format(date, "%Y-%m")]

  res_full[]
}


## 5. Resumos de disponibilidade por unidade IDF (total e mensal) ----

summarise_availability <- function(daylight_availability_dt) {

  by_idf <- daylight_availability_dt[, .(
    offline_mins_total  = sum(offline_mins, na.rm = TRUE),
    daylight_mins_total = sum(daylight_mins, na.rm = TRUE),
    days_covered        = uniqueN(date)
  ), by = idf]

  by_idf[, monitoring_period_pct := round(100 * offline_mins_total / daylight_mins_total, 1)]
  by_idf[, availability_pct := round(100 - monitoring_period_pct, 1)]
  setorder(by_idf, -monitoring_period_pct)

  by_month <- daylight_availability_dt[, .(
    offline_mins_total  = sum(offline_mins, na.rm = TRUE),
    daylight_mins_total = sum(daylight_mins, na.rm = TRUE),
    days_covered        = uniqueN(date)
  ), by = .(idf, ym)]

  by_month[, offline_pct_weighted := round(100 * offline_mins_total / daylight_mins_total, 1)]
  setorder(by_month, ym, -offline_mins_total)

  list(by_idf = by_idf[], by_month = by_month[])
}


## 6. Calendario com % de tempo diurno disponivel, por unidade IDF ----

add_calendar_coords <- function(dt) {
  dt <- copy(dt)
  dt[, `:=`(
    weekday_lab = factor(
      c("M", "T", "W", "T.", "F", "S", "S.")[lubridate::wday(date, week_start = 1)],
      levels = c("M", "T", "W", "T.", "F", "S", "S.")
    ),
    week_of_month = {
      ms  <- lubridate::floor_date(date, "month")
      wms <- lubridate::wday(ms, week_start = 1)
      as.integer(1 + ((lubridate::mday(date) + wms - 2) %/% 7))
    }
  )]
  dt[]
}

plot_availability_calendar <- function(daylight_availability_dt, idf_sel = NULL) {

  dt <- add_calendar_coords(daylight_availability_dt)
  dt[, availability_pct := 100 * (1 - offline_pct)]

  if (!is.null(idf_sel)) dt <- dt[idf %in% idf_sel]

  ggplot(dt, aes(x = weekday_lab, y = week_of_month, fill = availability_pct)) +
    geom_tile(colour = "grey85", linewidth = 0.2) +
    facet_grid(idf ~ ym, scales = "free_y") +
    scale_y_reverse(breaks = seq(1, 6, 1)) +
    scale_fill_gradient(
      name = "Disponibilidade\n(% tempo diurno)",
      low = "firebrick", high = "forestgreen",
      limits = c(0, 100),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      x = NULL, y = "Semana do mes",
      title = "Disponibilidade diurna por unidade IDF"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(hjust = 0.5)
    )
}
