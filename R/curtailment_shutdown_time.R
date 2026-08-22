##
## Tempo ate atingir limiares de RPM (2, 1, 0), por curtailment
##
## Para cada curtailment com um RPM de baseline fiavel no `start` (mesmo
## criterio de tolerancia apertada usado em assess_curtailment_response()),
## acompanha o RPM da turbina desde o `start` ate ao `end` desse curtailment
## (a duracao real de cada evento, nao uma janela fixa), e regista o primeiro
## instante em que a turbina atinge cada um dos limiares -- por omissao
## 2 rpm, 1 rpm e 0 rpm.
##
## Curtailments sem match de baseline fiavel (fora da tolerancia de
## start_end_gap_sec) ficam de fora -- so seguimos os que cumprem os nossos
## criterios de qualidade de match.
##
## Nota sobre o limiar 0 rpm: as leituras SCADA sao valores continuos (ex:
## 0.152...), por isso "rpm <= 0" raramente e atingido na pratica -- e
## esperado ver muitos NA nesse limiar especificamente.
##
## Depende de: data.table, ggplot2, scales, R/curtailment_response.R (usa match_nearest_rpm())
##
## Uso:
##   source("R/curtailment_response.R")
##   source("R/curtailment_shutdown_time.R")
##   tt_dt <- time_to_rpm_thresholds(curtl_scada_dt, scada_dt, thresholds = c(2, 1, 0))
##   by_turbine  <- summarise_time_to_threshold(tt_dt)          # identificar turbinas mais lentas
##   bands       <- summarise_time_to_threshold_bands(tt_dt)    # % <40s e % >50s
##   plot_time_to_threshold(tt_dt)
##


## 1. Tempo ate cada limiar de RPM, por curtailment ----

time_to_rpm_thresholds <- function(curtl_dt, scada_dt, thresholds = c(2, 1, 0),
                                   start_end_gap_sec = 2) {

  dt <- as.data.table(curtl_dt)
  dt[, curtailment_id := .I]

  ## baseline no start (tolerancia apertada) -- so seguimos curtailments com match fiavel
  start_events <- dt[, .(id = curtailment_id, turbine, event_time = start)]
  start_match  <- match_nearest_rpm(start_events, scada_dt, max_gap_sec = start_end_gap_sec)

  valid_ids <- start_match[valid_match == TRUE, id]
  dt_valid  <- dt[curtailment_id %in% valid_ids]

  if (nrow(dt_valid) == 0L) {
    return(data.table(
      curtailment_id = integer(), threshold = numeric(), turbine = character(),
      track_id = character(), species = character(), start = as.POSIXct(character()),
      end = as.POSIXct(character()), start_rpm = numeric(),
      hit_time = as.POSIXct(character()), time_to_threshold_sec = numeric()
    ))
  }

  ## leituras RPM entre o start e o end de cada curtailment (duracao real do evento)
  rpm_dt <- scada_dt[readingname == "RPM", .(turbine = turbinelabel, datetime, rpm = value)]

  windows <- dt_valid[, .(curtailment_id, turbine, window_start = start, window_end = end)]

  rpm_window <- rpm_dt[
    windows,
    on = .(turbine, datetime >= window_start, datetime <= window_end),
    allow.cartesian = TRUE,
    .(curtailment_id = i.curtailment_id, window_start = i.window_start,
      datetime = x.datetime, rpm = x.rpm)
  ]

  ## para cada limiar, o 1o instante em que rpm <= limiar
  ## (0 rpm raramente e atingido -- filtra ANTES do min() por-grupo para evitar
  ## o aviso inofensivo mas ruidoso do data.table "no non-missing arguments to
  ## min" quando o filtro nao apanha nenhuma linha em nenhum curtailment)
  hits <- rbindlist(lapply(thresholds, function(th) {
    rpm_hit_rows <- rpm_window[!is.na(rpm) & rpm <= th]
    if (nrow(rpm_hit_rows) == 0L) {
      return(data.table(curtailment_id = integer(), threshold = numeric(),
                        hit_time = as.POSIXct(character()), time_to_threshold_sec = numeric()))
    }
    hit <- rpm_hit_rows[, .(hit_time = min(datetime), window_start = first(window_start)), by = curtailment_id]
    hit[, time_to_threshold_sec := as.numeric(difftime(hit_time, window_start, units = "secs"))]
    hit[, threshold := th]
    hit[, .(curtailment_id, threshold, hit_time, time_to_threshold_sec)]
  }))

  ## grelha completa curtailment x limiar -- os que nunca atingem ficam com NA, nao desaparecem
  grid <- CJ(curtailment_id = unique(dt_valid$curtailment_id), threshold = thresholds)

  out <- merge(
    grid,
    merge(dt_valid[, .(curtailment_id, turbine, track_id, species, start, end)],
          start_match[, .(curtailment_id = id, start_rpm = rpm)],
          by = "curtailment_id"),
    by = "curtailment_id"
  )
  out <- merge(out, hits, by = c("curtailment_id", "threshold"), all.x = TRUE)

  setorder(out, curtailment_id, -threshold)
  out[]
}


## 2. Resumo por turbina e limiar -- identifica as turbinas mais lentas ----

summarise_time_to_threshold <- function(time_to_threshold_dt) {

  safe_range <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) return(c(NA_real_, NA_real_))
    c(min(x), max(x))
  }

  by_turbine <- time_to_threshold_dt[, {
    rng <- safe_range(time_to_threshold_sec)
    .(
      n_curtailments  = uniqueN(curtailment_id),
      n_reached       = sum(!is.na(time_to_threshold_sec)),
      pct_reached     = round(100 * sum(!is.na(time_to_threshold_sec)) / uniqueN(curtailment_id), 1),
      mean_time_sec   = round(mean(time_to_threshold_sec, na.rm = TRUE), 1),
      median_time_sec = round(median(time_to_threshold_sec, na.rm = TRUE), 1),
      sd_time_sec     = round(sd(time_to_threshold_sec, na.rm = TRUE), 1),
      min_time_sec    = rng[1],
      max_time_sec    = rng[2]
    )
  }, by = .(turbine, threshold)]

  setorder(by_turbine, threshold, -mean_time_sec)
  by_turbine[]
}


## 3. Tabela de bandas -- % de curtailments abaixo/acima dos cortes, por limiar ----

summarise_time_to_threshold_bands <- function(time_to_threshold_dt, low_cut = 40, high_cut = 50) {

  time_to_threshold_dt[
    !is.na(time_to_threshold_sec),
    .(
      n_reached     = .N,
      pct_below_cut = round(100 * sum(time_to_threshold_sec < low_cut) / .N, 1),
      pct_above_cut = round(100 * sum(time_to_threshold_sec > high_cut) / .N, 1)
    ),
    by = threshold
  ][order(-threshold)]
}


## 4. Histograma da distribuicao dos tempos, por limiar ----

plot_time_to_threshold <- function(time_to_threshold_dt, threshold_sel = NULL) {

  dt <- time_to_threshold_dt[!is.na(time_to_threshold_sec)]
  if (!is.null(threshold_sel)) dt <- dt[threshold %in% threshold_sel]
  dt[, threshold_lab := paste0("<= ", threshold, " rpm")]
  dt[, threshold_lab := factor(threshold_lab, levels = paste0("<= ", sort(unique(dt$threshold), decreasing = TRUE), " rpm"))]

  ggplot(dt, aes(x = time_to_threshold_sec)) +
    geom_histogram(binwidth = 5, boundary = 0, fill = "steelblue", colour = "black") +
    facet_wrap(~ threshold_lab, ncol = 1, scales = "free_y") +
    scale_x_continuous(breaks = scales::breaks_width(10)) +
    labs(
      x = "Time to reach threshold (s)",
      y = "Number of curtailments",
      title = "Distribution of time to reach RPM thresholds"
    ) +
    theme_minimal()
}
