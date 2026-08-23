##
## Distancia de seguranca teorica (metodologia KNE), por curtailment
##
## Adaptado dos scripts originais scripts_IDF/curtailments_scada_roll_join.R
## (calculo), curtailments_time_to_curtail_calc.R, curtailments_threshold_dist.R
## e curtailments_safe_distances.R (plots).
##
## Formula: min_safe_dist_m = time_to_threshold_sec * avg_speed_ms
##   time_to_threshold_sec -- tempo (s) desde o sinal de curtailment ate a
##     turbina atingir rpm_threshold (2 rpm por omissao, valor do KNE) --
##     reaproveita time_to_rpm_thresholds() (R/curtailment_shutdown_time.R),
##     ja validado com tolerancia de match apertada (start_end_gap_sec).
##   avg_speed_ms -- velocidade media de voo DO PROPRIO TRACK que despoletou
##     aquele curtailment (nao uma velocidade de referencia por especie),
##     com corte de outliers no percentil 95 e so valores > 0.
##
## dist_margin_m = x2d - min_safe_dist_m
##   x2d -- distancia da ave a turbina no 1o registo do track (aproximacao
##     ao momento do disparo do curtailment)
##   status = "OK" se dist_margin_m > 0 (ave estava mais longe do que o
##     minimo necessario); "Crit" se <= 0 (curtailment atrasado -- a ave
##     podia chegar ao rotor antes deste atingir uma velocidade segura)
##
## turbine_state -- baseado no start_rpm (rpm da turbina no momento do
##   disparo, ja calculado por time_to_rpm_thresholds()):
##   "already_slowing" se start_rpm < already_slowing_rpm_threshold -- a
##     turbina ja estava a abrandar/recuperar de outro curtailment (cenario
##     "beneficio": esta ave aproveitou uma paragem ja em curso, disparada
##     por outra deteção)
##   "full_speed" caso contrario -- cenario mais gravoso, turbina a
##     velocidade normal de operacao no momento do disparo
##
## Depende de: data.table, ggplot2, R/curtailment_response.R,
## R/curtailment_shutdown_time.R (usa time_to_rpm_thresholds())
##
## Uso:
##   source("R/curtailment_response.R")
##   source("R/curtailment_shutdown_time.R")
##   source("R/curtailment_safe_distance.R")
##   safe_dist_dt <- compute_safe_distance(curtl_scada_dt, scada_dt, track_dt)
##   summary_safe_dist <- summarise_safe_distance(safe_dist_dt, prioritysp)
##   plot_safe_distance_hist(safe_dist_dt, species_sel = prioritysp, facet = TRUE)
##   plot_trigger_distance_status(safe_dist_dt, species_sel = prioritysp, facet = TRUE)
##
##   ## separar cenario mais gravoso (full_speed) do cenario de beneficio
##   ## (already_slowing) -- summarise_safe_distance() nao muda, filtra-se antes:
##   summary_full_speed      <- summarise_safe_distance(safe_dist_dt[turbine_state == "full_speed"], prioritysp)
##   summary_already_slowing <- summarise_safe_distance(safe_dist_dt[turbine_state == "already_slowing"], prioritysp)
##


## 1. Distancia de seguranca teorica, por curtailment ----

compute_safe_distance <- function(curtl_dt, scada_dt, track_dt,
                                  start_end_gap_sec = 2, rpm_threshold = 2,
                                  speed_trim_q = 0.95, already_slowing_rpm_threshold = 6) {

  tt_dt <- time_to_rpm_thresholds(
    curtl_dt, scada_dt, thresholds = rpm_threshold, start_end_gap_sec = start_end_gap_sec
  )

  ## velocidade media de voo do track (corte outliers > percentil 95, so valores > 0)
  track_speed_dt <- track_dt[!is.na(speed_ms), {
    q <- as.numeric(quantile(speed_ms, probs = speed_trim_q, na.rm = TRUE))
    .(avg_speed_ms = mean(speed_ms[speed_ms > 0 & speed_ms < q], na.rm = TRUE))
  }, by = track_id]

  ## distancia a turbina no 1o registo do track (aproximacao ao momento do disparo)
  track_dist_dt <- track_dt[, .(x2d = dist[1]), by = track_id]

  out <- merge(tt_dt, track_speed_dt, by = "track_id", all.x = TRUE)
  out <- merge(out, track_dist_dt, by = "track_id", all.x = TRUE)

  out[, min_safe_dist_m := time_to_threshold_sec * avg_speed_ms]
  out[, dist_margin_m := x2d - min_safe_dist_m]
  out[, status := fifelse(
    is.na(dist_margin_m), NA_character_,
    fifelse(dist_margin_m > 0, "OK", "Crit")
  )]
  out[, turbine_state := fifelse(
    is.na(start_rpm), NA_character_,
    fifelse(start_rpm < already_slowing_rpm_threshold, "already_slowing", "full_speed")
  )]

  out[]
}


## 2. Resumo por especie (e pooled), % de curtailments "OK" vs "Crit" ----

summarise_safe_distance <- function(safe_dist_dt, species_sel = NULL) {

  dt <- safe_dist_dt[!is.na(status)]
  if (!is.null(species_sel)) dt <- dt[species %in% species_sel]

  # mean_time_to_threshold_sec/mean_avg_speed_ms/pct_already_slowing --
  # min_safe_dist_m = time_to_threshold_sec * avg_speed_ms (ver formula no
  # topo do ficheiro), por isso um mean_min_safe_dist_m baixo tem sempre 1 de
  # 2 causas (ou as 2): reacao rapida (tempo curto) ou ave lenta (avg_speed
  # baixo) -- e um tempo curto e' normal quando a turbina ja estava a
  # abrandar de um curtailment anterior (pct_already_slowing alto), nao
  # necessariamente resposta rapida a ESTE sinal. Estas 3 colunas expoem os
  # 2 fatores da formula e o contexto de estado da turbina, para nao deixar
  # um mean_min_safe_dist_m pequeno por-si-so' parecer um valor "estranho"
  # sem explicacao (pedido do Paulo, 2026-08, sobre um caso de 14.5m).
  summary_stats <- function(d) {
    d[, {
      n_state_known <- sum(!is.na(turbine_state))
      .(
        n                          = .N,
        n_ok                       = sum(status == "OK"),
        n_crit                     = sum(status == "Crit"),
        pct_ok                     = round(100 * sum(status == "OK") / .N, 1),
        mean_time_to_threshold_sec = round(mean(time_to_threshold_sec, na.rm = TRUE), 1),
        mean_avg_speed_ms          = round(mean(avg_speed_ms, na.rm = TRUE), 1),
        pct_already_slowing        = if (n_state_known > 0) {
          round(100 * sum(turbine_state == "already_slowing", na.rm = TRUE) / n_state_known, 1)
        } else {
          NA_real_
        },
        mean_min_safe_dist_m      = round(mean(min_safe_dist_m, na.rm = TRUE), 1),
        median_min_safe_dist_m    = round(median(min_safe_dist_m, na.rm = TRUE), 1)
      )
    }]
  }

  overall    <- summary_stats(dt)
  by_species <- dt[, summary_stats(.SD), by = species]
  setorder(by_species, pct_ok)

  list(overall = overall[], by_species = by_species[])
}


## 3. Histograma das distancias de seguranca calculadas (min_safe_dist_m) ----

plot_safe_distance_hist <- function(safe_dist_dt, species_sel = NULL, ref_line_m = 600, facet = FALSE) {

  dt <- safe_dist_dt[!is.na(min_safe_dist_m)]
  if (!is.null(species_sel)) dt <- dt[species %in% species_sel]

  p <- ggplot(dt, aes(x = min_safe_dist_m)) +
    geom_histogram(binwidth = 50, boundary = 0, colour = "grey") +
    labs(
      x = "Calculated theoretical safe distance (m)", y = "Count",
      title = "Distribution of theoretical safe distances (KNE method)"
    ) +
    geom_vline(xintercept = ref_line_m, linetype = "dashed") +
    theme_bw()

  if (facet) p <- p + facet_wrap(~species, ncol = 3)
  p
}


## 4. Distancia real ao disparo do curtailment, colorida por estado OK/Crit ----

plot_trigger_distance_status <- function(safe_dist_dt, species_sel = NULL, ref_line_m = 600, facet = FALSE) {

  dt <- safe_dist_dt[!is.na(x2d) & !is.na(status)]
  if (!is.null(species_sel)) dt <- dt[species %in% species_sel]

  p <- ggplot(dt, aes(x = x2d, fill = status)) +
    geom_histogram(binwidth = 50, boundary = 0, position = "dodge", colour = "white") +
    scale_fill_manual(
      values = c("OK" = "#17aeb0", "Crit" = "#ef6a5b"),
      labels = c("OK" = "Safe", "Crit" = "Delayed")
    ) +
    labs(
      x = "Distance to turbine at curtailment trigger (m)", y = "Count", fill = "Curtailment",
      title = "Curtailment trigger distance vs. calculated safe distance"
    ) +
    geom_vline(xintercept = ref_line_m, linetype = "dashed") +
    theme_bw()

  if (facet) p <- p + facet_wrap(~species, ncol = 3)
  p
}
