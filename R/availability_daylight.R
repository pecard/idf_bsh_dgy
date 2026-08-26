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
## Depende de: data.table, lubridate, suncalc, ggplot2, scales, sf (so' as
## funcoes 10-11, join_availability_to_turbine()/plot_availability_spatial())
##


## 1. Calendario de luz do dia (sunrise/sunset) para um intervalo de datas ----

build_daylight_calendar <- function(start_date, end_date, lat, lon, tz) {

  # as.Date() sem tz= usa UTC por omissao -- se start_date/end_date forem
  # POSIXct (ex: ini/end em Asia/Samarkand, UTC+5), a meia-noite local cai
  # as 19h do dia anterior em UTC, empurrando o calendario 1 dia para tras.
  # tz=tz aqui reutiliza o mesmo fuso ja recebido para o suncalc abaixo.
  dates <- seq(as.Date(start_date, tz = tz), as.Date(end_date, tz = tz), by = "day")

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

  hb[, ts_prev := data.table::shift(timestamp, type = "lag"), by = idf]

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
  by_idf[, availability_cat := cut(
    availability_pct,
    breaks = c(-Inf, 60, 80, 90, 96, Inf),
    labels = c("<60%", "60-80%", "80-90%", "90-96%", ">96%"),
    right = TRUE
  )]
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

## `by_idf_summary` (de summarise_availability()$by_idf) e usado so para
## selecionar as `top_n` unidades com mais tempo offline quando `idf_sel`
## nao e indicado -- mantem o grafico legivel quando ha muitas unidades IDF.

plot_availability_calendar <- function(daylight_availability_dt, by_idf_summary,
                                       idf_sel = NULL, top_n = 12L) {

  if (is.null(idf_sel)) {
    idf_sel <- by_idf_summary[order(-offline_mins_total)][seq_len(min(top_n, .N)), idf]
  }

  # subtitulo correto tambem quando idf_sel cobre TODAS as unidades (o
  # calendario completo do anexo, nao so' um top-N) -- "Top N" seria
  # enganador nesse caso
  subtitle_txt <- if (length(idf_sel) >= data.table::uniqueN(by_idf_summary$idf)) {
    sprintf("All %d IDF units", length(idf_sel))
  } else {
    sprintf("Top %d IDF units by offline time", length(idf_sel))
  }

  dt <- add_calendar_coords(daylight_availability_dt)
  dt <- dt[idf %in% idf_sel]

  # celulas sem qualquer offline (cinza) separadas das celulas com offline > 0 (gradiente)
  dat_zero <- dt[!is.na(offline_pct) & offline_pct <= 0]
  dat_pos  <- dt[!is.na(offline_pct) & offline_pct > 0]

  ggplot() +
    geom_tile(
      data = dat_zero,
      aes(x = weekday_lab, y = week_of_month),
      fill = "grey90", colour = "grey85", linewidth = 0.2
    ) +
    geom_tile(
      data = dat_pos,
      aes(x = weekday_lab, y = week_of_month, fill = offline_pct),
      colour = "grey85", linewidth = 0.2
    ) +
    facet_grid(idf ~ ym, scales = "free_y") +
    scale_fill_gradient(
      name = "Offline (%)",
      limits = c(0, 1),
      labels = scales::percent,
      breaks = scales::pretty_breaks(5),
      low = "#e0f3db", high = "#0868ac"
    ) +
    scale_y_reverse(breaks = seq(1, 6, 1)) +
    labs(
      x = NULL, y = "Week of month",
      title = "Daylight offline (%) calendar by IDF unit",
      subtitle = subtitle_txt
    ) +
    theme_minimal(base_size = 9) +
    theme(
      panel.grid = element_blank(),
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text = element_text(face = "bold"),
      # rotacao vertical (nao encolher o texto) -- com muitos meses lado a
      # lado (facet_grid ~ ym), cada painel fica demasiado estreito para 7
      # rotulos horizontais ("M T W T. F S S."); na vertical cada rotulo so'
      # precisa da largura de 1 caracter, nao do texto todo (mesma correcao
      # ja aplicada ao eixo X das leituras SCADA, R/curtailment_forensic_trace.R)
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 6),
      axis.ticks = element_blank()
    )
}


## 7. Frequencia de unidades IDF por categoria de disponibilidade ----

plot_availability_frequency <- function(by_idf_summary) {

  ggplot(by_idf_summary, aes(x = availability_cat)) +
    geom_bar(fill = "steelblue") +
    labs(
      x = "Availability category",
      y = "Number of IDF units",
      title = "Frequency of IDF units by daylight availability"
    ) +
    theme_minimal()
}


## 8. Grelha de heartbeats por slot (ex: 30 min), classificada dia/noite ----
##    Reutiliza o `daylight_cal` (funcao 1) em vez de recalcular o suncalc.

heartbeat_slot_grid <- function(heartb_dt, daylight_cal, tz,
                                start_date, end_date,
                                idf_sel = NULL, slot_mins = 30) {

  if (!is.null(idf_sel)) heartb_dt <- heartb_dt[idf %in% idf_sel]
  all_idf <- if (is.null(idf_sel)) sort(unique(heartb_dt$idf)) else idf_sel

  start_dt <- as.POSIXct(paste(as.Date(start_date), "00:00:00"), tz = tz)
  end_dt   <- as.POSIXct(paste(as.Date(end_date) + 1, "00:00:00"), tz = tz)

  slots <- seq(start_dt, end_dt - minutes(slot_mins), by = paste(slot_mins, "min"))

  grid <- CJ(idf = all_idf, slot = slots)
  grid[, date := as.Date(slot, tz = tz)]
  grid[, time_decimal := lubridate::hour(slot) + lubridate::minute(slot) / 60]

  # heartbeats observados, arredondados para o slot correspondente
  hb_slots <- heartb_dt[
    idf %in% all_idf & timestamp >= start_dt & timestamp < end_dt,
    .(idf, slot = lubridate::floor_date(timestamp, unit = paste(slot_mins, "minutes")))
  ]
  hb_slots <- unique(hb_slots, by = c("idf", "slot"))
  hb_slots[, heartbeat_present := TRUE]

  grid <- hb_slots[grid, on = .(idf, slot)]
  grid[is.na(heartbeat_present), heartbeat_present := FALSE]

  # dia/noite: comparar o ponto medio do slot com o sunrise/sunset desse dia
  grid <- daylight_cal[, .(date, sunrise, sunset)][grid, on = "date"]
  grid[, slot_midpoint := slot + minutes(slot_mins / 2)]
  grid[, daylight := slot_midpoint >= sunrise & slot_midpoint < sunset]

  grid[, slot_status := fcase(
    daylight  & heartbeat_present,  "Heartbeat - daylight",
    daylight  & !heartbeat_present, "Missing - daylight",
    !daylight & heartbeat_present,  "Heartbeat - night",
    !daylight & !heartbeat_present, "Missing - night"
  )]
  grid[, slot_status := factor(
    slot_status,
    levels = c("Heartbeat - daylight", "Missing - daylight",
              "Heartbeat - night", "Missing - night")
  )]

  grid[]
}


## 9. Plot da grelha de heartbeats (dia/noite, presente/em falta), por unidade IDF ----

plot_heartbeat_slots <- function(slot_grid_dt, date_breaks = "2 days", title = NULL) {

  if (is.null(title)) {
    title <- sprintf(
      "IdentiFlight heartbeat availability (%s to %s)",
      format(min(slot_grid_dt$date), "%d %b %Y"),
      format(max(slot_grid_dt$date), "%d %b %Y")
    )
  }

  ggplot(slot_grid_dt, aes(x = date, y = time_decimal, fill = slot_status)) +
    geom_tile(width = 0.95, height = 0.48) +
    facet_wrap(~idf, ncol = 1) +
    scale_fill_manual(
      name = "IDF status",
      values = c(
        "Heartbeat - daylight" = "#2C7FB8",
        "Missing - daylight"   = "#D7301F",
        "Heartbeat - night"    = "#BDBDBD",
        "Missing - night"      = "#252525"
      ),
      drop = FALSE
    ) +
    scale_x_date(date_breaks = date_breaks, date_labels = "%d %b %Y", expand = c(0, 0)) +
    scale_y_continuous(
      limits = c(0, 24),
      breaks = seq(0, 24, 3),
      labels = function(x) sprintf("%02d:00", x),
      expand = c(0, 0)
    ) +
    labs(
      x = "Date", y = "Local time",
      title = title,
      subtitle = "Daylight vs. night, heartbeat present vs. missing"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(size = 8),
      legend.position = "bottom"
    )
}


## 10. Cruzar disponibilidade por unidade IDF com a localizacao das turbinas
##     (matriz manual turbina<->IDF, Primary IDF) ----
##
## Devolve 1 linha por turbina do shapefile (wtg_sf), com as coordenadas
## (x, y, no CRS em que wtg_sf ja estiver -- projetar ANTES de chamar esta
## funcao) e a disponibilidade da SUA unidade IDF primaria (coluna "Primary
## IDF" da matriz manual) neste periodo. NAO agrega Primary+Secondary --
## e' a mesma logica "1 unidade por turbina" da secção de disponibilidade
## acima (by_idf), so' reprojetada no espaco; nao substitui a analise de
## fallback usada na investigacao de fatalidades (R/fatality_window_analysis.R).
## Turbinas sem unidade IDF primaria na matriz manual, ou cuja unidade
## primaria nao tem heartbeats neste periodo (nao aparece em by_idf_summary),
## ficam com as colunas de disponibilidade a NA -- mostradas no plot como
## "no data", nao descartadas em silencio.
##
## by_idf_summary: summarise_availability(...)$by_idf
## turbine_idf_manual_dt: lido diretamente do xlsx (ACWA_IDF_Coverage_Matrix.xlsx
## via turbine_idf_matrix_filename) -- colunas "Turbine ID"/"Primary IDF"

join_availability_to_turbine <- function(by_idf_summary, wtg_sf, turbine_idf_manual_dt,
                                         wtg_id_col = "InternalNa") {

  manual_dt <- data.table::as.data.table(turbine_idf_manual_dt)
  data.table::setnames(
    manual_dt,
    old = c("Turbine ID", "Primary IDF"),
    new = c("turbine", "primary_idf"),
    skip_absent = TRUE
  )
  manual_dt <- unique(manual_dt[, .(turbine, primary_idf)])

  coords <- sf::st_coordinates(wtg_sf)
  turbine_xy <- data.table::data.table(
    turbine = wtg_sf[[wtg_id_col]],
    x = coords[, "X"],
    y = coords[, "Y"]
  )

  out <- merge(turbine_xy, manual_dt, by = "turbine", all.x = TRUE)
  out <- merge(out, by_idf_summary, by.x = "primary_idf", by.y = "idf", all.x = TRUE)

  n_no_data <- out[is.na(monitoring_period_pct), .N]
  if (n_no_data > 0) {
    message(sprintf(
      "Aviso: %d turbina(s) sem disponibilidade calculavel (sem unidade IDF primaria na matriz manual, ou sem heartbeats dessa unidade neste periodo) -- ficam \"no data\" no plot espacial.",
      n_no_data
    ))
  }

  out[]
}


## 11. Plot espacial de (in)disponibilidade -- 1 ponto por turbina, tamanho
##     e cor proporcionais a % offline (monitoring_period_pct); turbinas
##     sem dados (NA) ficam com um marcador "x" cinzento, categoria
##     separada na legenda, nao descartadas do plot ----

plot_availability_spatial <- function(turbine_availability_dt, value_col = "monitoring_period_pct") {

  dt <- data.table::copy(turbine_availability_dt)
  dt[, value := get(value_col)]
  dt[, has_data := !is.na(value)]

  ggplot() +
    geom_point(
      data = dt[has_data == TRUE],
      aes(x = x, y = y, size = value),
      colour = "steelblue", alpha = 0.8 # so' tamanho varia -- sem escala de cor, 20% transparencia (ajuda a ver turbinas sobrepostas/proximas)
    ) +
    geom_point(
      data = dt[has_data == FALSE],
      aes(x = x, y = y),
      size = 2, colour = "grey60", shape = 4, alpha = 0.8
    ) +
    scale_size_continuous(name = "Offline (%)", range = c(2, 10)) +
    coord_equal() +
    labs(
      x = NULL, y = NULL,
      title = "Spatial distribution of IDF unavailability by turbine",
      subtitle = "Point size = % of daylight monitoring period offline (primary IDF unit); x = no data"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}
