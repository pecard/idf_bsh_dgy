##
## Cobertura temporal e lacunas de dados nas 4 bases de dados
## (tracks, curtailments, SCADA, heartbeats)
##
## Depende de: data.table, ggplot2
##


## 1. Intervalo de datas com dados, para UM dataset ----
##    Devolve: primeiro/ultimo dia com dados, duracao do periodo,
##    dias com dados e dias em falta nesse periodo.

data_date_range <- function(dt, datetime_col, label) {

  if (is.null(dt) || nrow(dt) == 0L) {
    return(data.table(
      dataset = label,
      start = as.Date(NA), end = as.Date(NA),
      total_days = NA_integer_, days_with_data = NA_integer_,
      days_missing = NA_integer_, pct_missing = NA_real_
    ))
  }

  dates <- as.Date(dt[[datetime_col]])
  start <- min(dates, na.rm = TRUE)
  end   <- max(dates, na.rm = TRUE)

  total_days     <- as.integer(end - start) + 1L
  days_with_data <- uniqueN(dates)
  days_missing   <- total_days - days_with_data

  data.table(
    dataset        = label,
    start          = start,
    end            = end,
    total_days     = total_days,
    days_with_data = days_with_data,
    days_missing   = days_missing,
    pct_missing    = round(100 * days_missing / total_days, 1)
  )
}


## 2. Tabela resumo com os 4 datasets ----

data_coverage_summary <- function(track_dt, curtl_dt, scada_dt, heartb_dt) {
  rbindlist(list(
    data_date_range(track_dt,  "timestamp", "Tracks"),
    data_date_range(curtl_dt,  "start",     "Curtailments"),
    data_date_range(scada_dt,  "datetime",  "SCADA"),
    data_date_range(heartb_dt, "timestamp", "Heartbeats")
  ))
}


## 3. Dias com dados (e respetiva contagem de registos), para UM dataset ----

daily_presence <- function(dt, datetime_col, label) {

  if (is.null(dt) || nrow(dt) == 0L) {
    return(data.table(dataset = character(), date = as.Date(character()), n = integer()))
  }

  dt <- as.data.table(dt)
  daily <- dt[, .(n = .N), by = .(date = as.Date(get(datetime_col)))]
  daily[, dataset := label]
  daily[]
}


## 4. Grelha completa dataset x dia, para o periodo global ----
##    (do 1º ao ultimo dia com dados, em qualquer um dos 4 datasets)

data_coverage_calendar <- function(track_dt, curtl_dt, scada_dt, heartb_dt) {

  daily <- rbindlist(list(
    daily_presence(track_dt,  "timestamp", "Tracks"),
    daily_presence(curtl_dt,  "start",     "Curtailments"),
    daily_presence(scada_dt,  "datetime",  "SCADA"),
    daily_presence(heartb_dt, "timestamp", "Heartbeats")
  ))

  if (nrow(daily) == 0L) stop("Nenhuma das 4 bases de dados tem dados.")

  full_range <- seq(min(daily$date), max(daily$date), by = "day")
  datasets   <- c("Tracks", "Curtailments", "SCADA", "Heartbeats")

  grid <- CJ(dataset = datasets, date = full_range)
  grid[daily, n := i.n, on = .(dataset, date)]
  grid[is.na(n), n := 0L]
  grid[, has_data := n > 0]
  grid[, dataset := factor(dataset, levels = rev(datasets))] # ordem no eixo y do plot

  grid[]
}


## 5. Plot "calendario" de cobertura (lacunas visiveis como faixas em branco) ----

plot_data_coverage <- function(coverage_dt) {

  ggplot(coverage_dt, aes(x = date, y = dataset, fill = has_data)) +
    geom_tile(colour = "white", linewidth = 0.1) +
    scale_fill_manual(
      values = c(`TRUE` = "steelblue", `FALSE` = "grey85"),
      labels = c(`TRUE` = "Com dados", `FALSE` = "Sem dados"),
      name = NULL
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
    labs(
      title = "Cobertura temporal das bases de dados",
      x = NULL, y = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}
