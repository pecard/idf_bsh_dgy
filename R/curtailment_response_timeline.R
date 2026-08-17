##
## Evolucao temporal da resposta a curtailments (missed/delayed) +
## sobreposicao com a abundancia/fenologia de aves
##
## Constroi uma serie temporal (por omissao semanal) da qualidade de
## resposta a curtailments -- % missed, % delayed, % ok -- ao longo de todo
## o periodo com dados de SCADA, e disponibiliza funcoes para sobrepor essa
## serie a abundancia de uma especie (min individuals por bin, agregada na
## mesma escala temporal), para explorar visualmente se a performance do
## sistema varia com os periodos de maior movimento migratorio.
##
## Usa a MESMA classificacao missed/delayed/ok que
## R/fatality_window_analysis.R (via classify_response_flag(), ver
## R/curtailment_response_classify.R) -- as duas vistas (janela de um
## incidente vs. evolucao farm-wide) olham para a mesma regra.
##
## Modulo generico -- nao depende de fatality_incidents; aplica-se a
## qualquer subconjunto de curtailments/turbinas que se lhe passe (ex:
## curtl_scada_dt, o universo ja usado em 3.5-3.7).
##
## Depende de: data.table, lubridate, ggplot2,
## R/curtailment_response.R, R/curtailment_shutdown_time.R,
## R/curtailment_response_classify.R (fazer source destes 3 antes)
##
## Uso:
##   source("R/curtailment_response_timeline.R")
##
##   response_dt <- classify_response_flag(
##     curtl_scada_dt, scada_dt,
##     start_end_gap_sec = curtailment_start_end_gap_sec, max_next_gap_sec = curtailment_max_next_gap_sec,
##     drop_pct_threshold = curtailment_drop_pct_threshold, rpm_threshold = safe_shutdown_rpm,
##     shutdown_thresholds = shutdown_time_thresholds, shutdown_high_cut_sec = shutdown_time_high_cut
##   )
##
##   timeline_dt <- summarise_response_timeline(response_dt, unit = response_timeline_unit)
##
##   abundance_dt <- summarise_abundance_timeline(min_indiv_bins_dt, unit = response_timeline_unit)
##
##   plots <- plot_response_vs_phenology(timeline_dt, abundance_dt, species_sel = "Steppe-Eagle")
##   plots$response_plot
##   plots$abundance_plot
##


## 1. Serie temporal de missed/delayed/ok, farm-wide (ou por turbina) ----
##
## pct_of_total = % do periodo incluindo no_data; pct_of_known = % so' dos
## casos classificaveis (exclui no_data, fica NA nessa linha) -- mesma logica
## de summarise_response_by_flag() em R/curtailment_response_classify.R,
## aqui repetida por periodo (e turbina, se by_turbine=TRUE).

summarise_response_timeline <- function(response_dt, unit = "week", by_turbine = FALSE) {

  empty <- data.table::data.table(
    period = as.Date(character()), response_flag = character(), n = integer(),
    pct_of_total = numeric(), pct_of_known = numeric()
  )
  if (nrow(response_dt) == 0L) return(empty)

  dt <- data.table::copy(response_dt)
  # as.Date() sem tz= usa UTC por omissao e desloca o limite do periodo ate
  # 5h (Asia/Samarkand = UTC+5) -- mesma familia de bug ja corrigida em
  # R/fatality_window_analysis.R
  dt[, period := lubridate::floor_date(as.Date(start, tz = attr(start, "tzone")), unit = unit)]

  by_cols <- if (by_turbine) c("turbine", "period", "response_flag") else c("period", "response_flag")
  counts <- dt[, .(n = .N), by = by_cols]

  totals_by_cols <- setdiff(by_cols, "response_flag")
  counts[, pct_of_total := round(100 * n / sum(n), 1), by = totals_by_cols]
  counts[, n_known := sum(n[response_flag != "no_data"]), by = totals_by_cols]
  counts[, pct_of_known := data.table::fifelse(
    response_flag == "no_data", NA_real_, round(100 * n / n_known, 1)
  )]
  counts[, n_known := NULL]

  data.table::setorder(counts, period)
  counts[]
}


## 2. Serie temporal de abundancia (min individuals), na mesma escala ----
##
## Reutiliza bins_dt de count_min_individuals_per_bin() (ver
## R/track_min_individuals.R) -- agrega o PICO por periodo (ex: semana), nao
## a media, seguindo o mesmo raciocinio de lower-bound de
## summarise_daily_max_individuals().

summarise_abundance_timeline <- function(bins_dt, unit = "week") {

  empty <- data.table::data.table(spec = character(), period = as.Date(character()), peak_individuals = integer())
  if (nrow(bins_dt) == 0L) return(empty)

  dt <- data.table::copy(bins_dt)
  dt[, period := lubridate::floor_date(as.Date(bin_start, tz = attr(bin_start, "tzone")), unit = unit)]

  out <- dt[, .(peak_individuals = max(n_individuals_min)), by = .(spec, period)]
  data.table::setorder(out, spec, period)
  out[]
}


## 3. Plots -- resposta (missed/delayed % empilhado) e abundancia,
##    alinhados no mesmo eixo temporal (2 plots separados, nao um eixo duplo
##    -- mais legivel e menos enganador; combinar/empilhar visualmente ao
##    inserir no relatorio) ----

plot_response_vs_phenology <- function(timeline_dt, abundance_dt, species_sel) {

  # pct_of_known -- % dos curtailments CLASSIFICAVEIS nesse periodo (exclui
  # no_data), a leitura mais direta de "qualidade de resposta"; pct_of_total
  # fica disponivel em timeline_dt para quem preferir ver diluido pelo no_data
  resp <- timeline_dt[response_flag %in% c("missed", "delayed")]
  aband <- abundance_dt[spec %in% species_sel]

  p_response <- ggplot(resp, aes(x = period, y = pct_of_known, fill = response_flag)) +
    geom_col() +
    scale_fill_manual(values = c(missed = "firebrick", delayed = "orange")) +
    labs(x = NULL, y = "% of classifiable curtailments", fill = NULL,
        title = "Missed / delayed curtailments over time") +
    theme_minimal()

  p_abundance <- ggplot(aband, aes(x = period, y = peak_individuals, colour = spec)) +
    geom_line() +
    geom_point(size = 1) +
    labs(x = "Period", y = "Peak individuals", colour = NULL, title = "Bird abundance over time") +
    theme_minimal()

  list(response_plot = p_response, abundance_plot = p_abundance)
}
