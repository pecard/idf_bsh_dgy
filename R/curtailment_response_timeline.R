##
## Evolucao temporal da resposta a curtailments (no-response events) +
## sobreposicao com a abundancia/fenologia de aves
##
## Constroi uma serie temporal (por omissao semanal) da qualidade de
## resposta a curtailments -- % no-response events, entre os curtailments
## acima da velocidade de cut-in -- ao longo de todo o periodo com dados de
## SCADA, e disponibiliza funcoes para sobrepor essa serie a abundancia de
## uma especie (min individuals por bin, agregada na mesma escala
## temporal), para explorar visualmente se a performance do sistema varia
## com os periodos de maior movimento migratorio.
##
## Ate' 2026-08 esta serie vinha de classify_response_flag() (% missed +
## % delayed, R/curtailment_response_classify.R) -- substituido pela
## classificacao de latencia (R/curtailment_response_latency.R,
## time_to_first_decline()) pela mesma razao que a secção "Curtailment
## Response & Latency" do relatorio: aquela classificacao confundia
## respostas lentas mas reais com falhas genuinas (RPM verificado so' no
## instante exato do "end"), e nao excluia turbinas ja abaixo do cut-in no
## "start" -- ambos inflacionavam a taxa de "missed" mostrada aqui.
## R/fatality_window_analysis.R continua a usar classify_response_flag()
## para a janela de cada incidente (analise diferente, nao afetada por
## esta mudanca).
##
## Modulo generico -- nao depende de fatality_incidents; aplica-se a
## qualquer latency_dt (ex: o calculado em 3.6b sobre curtl_scada_dt).
##
## Depende de: data.table, lubridate, ggplot2
##
## Uso:
##   source("R/curtailment_response_latency.R")
##   source("R/curtailment_response_timeline.R")
##
##   latency_dt <- time_to_first_decline(
##     curtl_scada_dt, scada_dt, decline_pct_threshold = curtailment_latency_decline_pct,
##     start_end_gap_sec = curtailment_start_end_gap_sec, buffer_after_end_sec = shutdown_time_buffer_sec,
##     cutin_rpm = curtailment_cutin_rpm
##   )
##
##   timeline_dt <- summarise_latency_timeline(latency_dt, unit = response_timeline_unit)
##
##   abundance_dt <- summarise_abundance_timeline(min_indiv_bins_dt, unit = response_timeline_unit)
##
##   plots <- plot_response_vs_phenology(timeline_dt, abundance_dt, species_sel = "Steppe-Eagle")
##   plots$response_plot
##   plots$abundance_plot
##


## 1. Serie temporal de no-response events, farm-wide ----
##
## Mesma definicao de no-response event da secção "No-Response Events" do
## relatorio (R/curtailment_response_latency.R): curtailments acima da
## velocidade de cut-in no "start" (below_cutin == FALSE) sem uma queda de
## RPM >= decline_pct_threshold detetada dentro da janela de procura.
## Curtailments abaixo do cut-in ficam de fora do denominador (nao ha
## resposta significativa para medir), tal como nas restantes tabelas de
## latencia -- ver R/curtailment_response_latency.R,
## summarise_latency_by_turbine() (mesma logica, aqui por periodo em vez
## de por turbina).

summarise_latency_timeline <- function(latency_dt, unit = "week") {

  empty <- data.table::data.table(
    period = as.Date(character()), n_curtailments = integer(), n_no_response = integer(),
    pct_no_response = numeric(), mean_latency_sec = numeric(), median_latency_sec = numeric()
  )
  if (nrow(latency_dt) == 0L) return(empty)

  # below_cutin excluido do denominador -- mesma logica de summarise_latency_by_turbine()
  dt <- data.table::copy(latency_dt)[below_cutin == FALSE]
  if (nrow(dt) == 0L) return(empty)

  # as.Date() sem tz= usa UTC por omissao e desloca o limite do periodo ate
  # 5h (Asia/Samarkand = UTC+5) -- mesma familia de bug ja corrigida em
  # R/fatality_window_analysis.R
  dt[, period := lubridate::floor_date(as.Date(start, tz = attr(start, "tzone")), unit = unit)]

  out <- dt[, {
    n_reached <- sum(!is.na(latency_sec))
    .(
      n_curtailments     = .N,
      n_no_response      = .N - n_reached,
      pct_no_response    = round(100 * (.N - n_reached) / .N, 1),
      mean_latency_sec   = round(mean(latency_sec, na.rm = TRUE), 1),
      median_latency_sec = round(median(latency_sec, na.rm = TRUE), 1)
    )
  }, by = period]

  data.table::setorder(out, period)
  out[]
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


## 3. Plots -- resposta (% no-response events), latencia (media/mediana) e
##    abundancia, alinhados no mesmo eixo temporal (plots separados, nao um
##    eixo duplo -- mais legivel e menos enganador; combinar/empilhar
##    visualmente ao inserir no relatorio) ----
##
## latency_plot -- pedido do Paulo (2026-08) depois de ver que, com
## no-response praticamente a 0 farm-wide (secção "No-Response Events" do
## relatorio), o plot de % no-response sozinho fica quase sempre achatado
## perto de 0 -- pouco informativo sobre se a VELOCIDADE tipica de resposta
## varia com a epoca. mean/median_latency_sec (de summarise_latency_timeline())
## sao continuas, por isso mostram essa variacao mesmo quando quase todos os
## curtailments respondem (so' nao respondem devagar ou depressa).

plot_response_vs_phenology <- function(timeline_dt, abundance_dt, species_sel) {

  aband <- abundance_dt[spec %in% species_sel]

  p_response <- ggplot(timeline_dt, aes(x = period, y = pct_no_response)) +
    geom_col(fill = "firebrick") +
    labs(x = NULL, y = "% of curtailments (above cut-in speed)",
        title = "No-response events over time") +
    theme_minimal()

  p_latency <- ggplot(timeline_dt, aes(x = period)) +
    geom_line(aes(y = mean_latency_sec, colour = "Mean")) +
    geom_point(aes(y = mean_latency_sec, colour = "Mean"), size = 1) +
    geom_line(aes(y = median_latency_sec, colour = "Median")) +
    geom_point(aes(y = median_latency_sec, colour = "Median"), size = 1) +
    scale_colour_manual(values = c(Mean = "steelblue", Median = "darkorange")) +
    labs(x = NULL, y = "Latency (s)", colour = NULL, title = "Response latency over time") +
    theme_minimal()

  p_abundance <- ggplot(aband, aes(x = period, y = peak_individuals, colour = spec)) +
    geom_line() +
    geom_point(size = 1) +
    labs(x = "Period", y = "Peak individuals", colour = NULL, title = "Bird abundance over time") +
    theme_minimal()

  list(response_plot = p_response, latency_plot = p_latency, abundance_plot = p_abundance)
}
