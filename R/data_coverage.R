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

  # tz= explicito -- as.Date() sem tz usa UTC por omissao, o que desloca a
  # data (ate 5h, Asia/Samarkand = UTC+5) perto da meia-noite local
  dates <- as.Date(dt[[datetime_col]], tz = attr(dt[[datetime_col]], "tzone"))
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
  dc_tz <- attr(dt[[datetime_col]], "tzone")
  daily <- dt[, .(n = .N), by = .(date = as.Date(get(datetime_col), tz = dc_tz))]
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
##
## date_breaks/date_labels por omissao servem para o periodo TIPICO desta
## funcao (historico completo do projeto, IDF_analysis.R) -- para uma
## janela curta (ex: 1 mes, relatorio mensal), passar algo mais fino (ex:
## date_breaks = "2 days", date_labels = "%d %b"), senao o eixo x fica so'
## com 1 marca (mesma logica de slot_date_breaks em IDF_analysis.R, secção
## 3.1 -- heartbeat_slots).

plot_data_coverage <- function(coverage_dt, date_breaks = "1 month", date_labels = "%Y-%m") {

  ggplot(coverage_dt, aes(x = date, y = dataset, fill = has_data)) +
    geom_tile(colour = "white", linewidth = 0.1) +
    scale_fill_manual(
      values = c(`TRUE` = "steelblue", `FALSE` = "grey85"),
      labels = c(`TRUE` = "Data available", `FALSE` = "No data"),
      name = NULL
    ) +
    scale_x_date(date_breaks = date_breaks, date_labels = date_labels) +
    labs(
      title = "Data coverage timeline",
      x = NULL, y = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      panel.grid = element_blank(),
      legend.position = "bottom"
    )
}


## 6. Sobreposicao temporal entre DOIS datasets (ex: SCADA vs Curtailments) ----
##    Reaproveita daily_presence() (funcao 3). Util para confirmar que ha
##    dias com dados em ambos antes de tentar cruza-los (ex: no
##    curtailment_response.R).

daily_overlap <- function(dt_a, col_a, label_a, dt_b, col_b, label_b) {

  daily <- rbindlist(list(
    daily_presence(dt_a, col_a, label_a),
    daily_presence(dt_b, col_b, label_b)
  ))

  if (nrow(daily) == 0L) stop("Nenhum dos dois datasets tem dados.")

  full_range <- seq(min(daily$date), max(daily$date), by = "day")

  grid <- CJ(dataset = c(label_a, label_b), date = full_range)
  grid[daily, n := i.n, on = .(dataset, date)]
  grid[is.na(n), n := 0L]
  grid[, has_data := n > 0]

  wide <- dcast(grid, date ~ dataset, value.var = "has_data")
  wide[, overlap := get(label_a) & get(label_b)]

  wide[]
}


## 7. Resumo da sobreposicao (quantos dias em comum, etc.) ----

overlap_summary <- function(overlap_dt, label_a, label_b) {

  days_a       <- sum(overlap_dt[[label_a]])
  days_b       <- sum(overlap_dt[[label_b]])
  days_overlap <- sum(overlap_dt$overlap)

  data.table(
    dataset_a           = label_a,
    dataset_b           = label_b,
    total_days          = nrow(overlap_dt),
    days_a              = days_a,
    days_b              = days_b,
    days_overlap        = days_overlap,
    pct_a_with_overlap  = round(100 * days_overlap / days_a, 1),
    pct_b_with_overlap  = round(100 * days_overlap / days_b, 1)
  )
}


## 8. Plot "calendario" da sobreposicao entre os dois datasets ----

plot_daily_overlap <- function(overlap_dt, label_a, label_b) {

  long <- melt(
    overlap_dt[, c("date", label_a, label_b, "overlap"), with = FALSE],
    id.vars = "date", variable.name = "dataset", value.name = "has_data"
  )
  long[, dataset := factor(dataset, levels = rev(c(label_a, label_b, "overlap")))]

  ggplot(long, aes(x = date, y = dataset, fill = has_data)) +
    geom_tile(colour = "white", linewidth = 0.1) +
    scale_fill_manual(
      values = c(`TRUE` = "steelblue", `FALSE` = "grey85"),
      labels = c(`TRUE` = "Data available", `FALSE` = "No data"),
      name = NULL
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
    labs(
      title = paste("Data coverage overlap:", label_a, "vs", label_b),
      x = NULL, y = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}


## 9. Presenca diaria POR TURBINA, para UM dataset ----
##    Util quando os dados (ex: SCADA) sao descarregados turbina a turbina,
##    e cada uma pode ter um periodo de cobertura diferente.

daily_presence_by_turbine <- function(dt, datetime_col, turbine_col, label) {

  if (is.null(dt) || nrow(dt) == 0L) {
    return(data.table(dataset = character(), turbine = character(),
                      date = as.Date(character()), n = integer()))
  }

  dt <- as.data.table(dt)
  dc_tz <- attr(dt[[datetime_col]], "tzone")
  daily <- dt[, .(n = .N), by = .(turbine = get(turbine_col), date = as.Date(get(datetime_col), tz = dc_tz))]
  daily[, dataset := label]
  daily[]
}


## 10. Sobreposicao temporal entre DOIS datasets, POR TURBINA ----
##     (ex: para cada turbina, em que dias temos SCADA E Curtailments)

daily_overlap_by_turbine <- function(dt_a, col_a, turbine_col_a, label_a,
                                     dt_b, col_b, turbine_col_b, label_b) {

  daily <- rbindlist(list(
    daily_presence_by_turbine(dt_a, col_a, turbine_col_a, label_a),
    daily_presence_by_turbine(dt_b, col_b, turbine_col_b, label_b)
  ))

  if (nrow(daily) == 0L) stop("Nenhum dos dois datasets tem dados.")

  full_range <- seq(min(daily$date), max(daily$date), by = "day")
  turbines   <- sort(unique(daily$turbine))

  grid <- CJ(dataset = c(label_a, label_b), turbine = turbines, date = full_range)
  grid[daily, n := i.n, on = .(dataset, turbine, date)]
  grid[is.na(n), n := 0L]
  grid[, has_data := n > 0]

  wide <- dcast(grid, turbine + date ~ dataset, value.var = "has_data")
  wide[, overlap := get(label_a) & get(label_b)]

  wide[]
}


## 11. Resumo da sobreposicao por turbina (quantos dias em comum, por turbina) ----

overlap_summary_by_turbine <- function(overlap_dt, label_a, label_b) {

  summary_dt <- overlap_dt[, .(
    total_days   = .N,
    days_a       = sum(get(label_a)),
    days_b       = sum(get(label_b)),
    days_overlap = sum(overlap)
  ), by = turbine]

  summary_dt[, `:=`(
    pct_a_with_overlap = round(100 * days_overlap / days_a, 1),
    pct_b_with_overlap = round(100 * days_overlap / days_b, 1)
  )]

  setorder(summary_dt, days_overlap)
  summary_dt[]
}


## 12. Plot "calendario" da sobreposicao, uma faixa por turbina ----
##     turbine_sel: vetor de turbinas a mostrar (NULL = todas -- cuidado,
##     pode ficar com muitas linhas se houver muitas turbinas)

plot_daily_overlap_by_turbine <- function(overlap_dt, label_a, label_b, turbine_sel = NULL) {

  dt <- copy(overlap_dt)
  if (!is.null(turbine_sel)) dt <- dt[turbine %in% turbine_sel]

  long <- melt(
    dt[, c("turbine", "date", label_a, label_b, "overlap"), with = FALSE],
    id.vars = c("turbine", "date"), variable.name = "dataset", value.name = "has_data"
  )
  long[, dataset := factor(dataset, levels = rev(c(label_a, label_b, "overlap")))]

  ggplot(long, aes(x = date, y = dataset, fill = has_data)) +
    geom_tile(colour = "white", linewidth = 0.1) +
    facet_wrap(~turbine, ncol = 1) +
    scale_fill_manual(
      values = c(`TRUE` = "steelblue", `FALSE` = "grey85"),
      labels = c(`TRUE` = "Data available", `FALSE` = "No data"),
      name = NULL
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
    labs(
      title = paste("Data coverage overlap by turbine:", label_a, "vs", label_b),
      x = NULL, y = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold")
    )
}


## 13. Presenca diaria de UM dataset, por unidade IDF ----
##     As unidades IDF (`idf`) nao correspondem 1:1 as turbinas (`turbine`)
##     -- cada turbina pode ser protegida por varias unidades IDF -- por
##     isso a cobertura de heartbeats e avaliada em separado, por unidade
##     IDF, e nao fundida no eixo das turbinas usado nas funcoes acima.
##     Reaproveita daily_presence_by_turbine() (funcao 9), so troca o nome
##     da coluna de saida para "idf" (mais claro nos outputs/exports).

daily_presence_by_idf <- function(dt, datetime_col, idf_col, label) {
  daily <- daily_presence_by_turbine(dt, datetime_col, idf_col, label)
  setnames(daily, "turbine", "idf")
  daily[]
}


## 14. Resumo de cobertura diaria por unidade IDF (dias com dados, dias em falta) ----
##     Cada unidade usa o seu proprio periodo (1o ao ultimo dia com dados),
##     tal como data_date_range() (funcao 1) faz para os datasets globais.

presence_summary_by_idf <- function(presence_dt) {

  summary_dt <- presence_dt[, .(
    start          = min(date),
    end            = max(date),
    days_with_data = uniqueN(date)
  ), by = idf]

  summary_dt[, total_days   := as.integer(end - start) + 1L]
  summary_dt[, days_missing := total_days - days_with_data]
  summary_dt[, pct_missing  := round(100 * days_missing / total_days, 1)]

  setorder(summary_dt, -pct_missing)
  summary_dt[]
}


## 15. Plot "calendario" de cobertura diaria por unidade IDF ----
##     idf_sel: vetor de unidades a mostrar (NULL = todas)

plot_daily_presence_by_idf <- function(presence_dt, idf_sel = NULL, label = "Heartbeats") {

  dt <- copy(presence_dt)
  if (!is.null(idf_sel)) dt <- dt[idf %in% idf_sel]

  full_range <- seq(min(dt$date), max(dt$date), by = "day")
  grid <- CJ(idf = sort(unique(dt$idf)), date = full_range)
  grid[dt, has_data := TRUE, on = .(idf, date)]
  grid[is.na(has_data), has_data := FALSE]

  ggplot(grid, aes(x = date, y = idf, fill = has_data)) +
    geom_tile(colour = "white", linewidth = 0.1) +
    scale_fill_manual(
      values = c(`TRUE` = "steelblue", `FALSE` = "grey85"),
      labels = c(`TRUE` = "Data available", `FALSE` = "No data"),
      name = NULL
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
    labs(title = paste("Data coverage:", label), x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}
