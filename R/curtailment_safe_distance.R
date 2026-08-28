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
## Depende de: data.table, ggplot2, stats (CI/testes, seccao 5),
## R/curtailment_response.R, R/curtailment_shutdown_time.R (usa
## time_to_rpm_thresholds())
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
##   ## estatistica formal (CI, tendencia mensal, teste entre especies) -- seccao 5
##   ci_overall    <- summarise_safe_distance_ci(safe_dist_dt, prioritysp)
##   ci_by_species <- summarise_safe_distance_ci(safe_dist_dt, prioritysp, by_species = TRUE)
##   by_month      <- summarise_safe_distance_by_month(safe_dist_dt[species %in% prioritysp])
##   p_by_month    <- plot_safe_distance_by_month(by_month)
##   trend         <- test_safe_distance_trend(safe_dist_dt[species %in% prioritysp])
##   species_test  <- test_safe_distance_by_species(safe_dist_dt[species %in% prioritysp])
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

  # sem linhas -- facet_wrap(~species) nao tem valores para facetar e falha
  # com "Faceting variables must have at least one value" (ggplot2::combine_vars()).
  # NULL devolvido em vez de um plot partido -- caller ja trata NULL como
  # "sem dados este periodo, saltar" (mesma convencao usada nas restantes
  # secções do relatorio)
  if (nrow(dt) == 0L) {
    message("plot_safe_distance_hist(): sem curtailments com min_safe_dist_m calculavel para as especies pedidas -- NULL devolvido.")
    return(NULL)
  }

  p <- ggplot(dt, aes(x = min_safe_dist_m)) +
    geom_histogram(binwidth = 50, boundary = 0, colour = "grey") +
    labs(
      x = "Calculated theoretical safe distance (m)", y = "Count",
      title = "Distribution of theoretical safe distances (KNE method)"
    ) +
    geom_vline(xintercept = ref_line_m, linetype = "dashed") +
    theme_bw()

  # scales="free_y" -- pedido do Paulo, 2026-08: especies com poucos
  # curtailments ficavam com uma barra ilegivel, achatada pela escala
  # partilhada com a especie mais frequente
  if (facet) p <- p + facet_wrap(~species, ncol = 3, scales = "free_y")
  p
}


## 4. Distancia real ao disparo do curtailment, colorida por estado OK/Crit ----

plot_trigger_distance_status <- function(safe_dist_dt, species_sel = NULL, ref_line_m = 600, facet = FALSE) {

  dt <- safe_dist_dt[!is.na(x2d) & !is.na(status)]
  if (!is.null(species_sel)) dt <- dt[species %in% species_sel]

  # mesma razao de plot_safe_distance_hist() acima -- sem linhas, facet_wrap()
  # nao tem valores para facetar
  if (nrow(dt) == 0L) {
    message("plot_trigger_distance_status(): sem curtailments com x2d/status calculavel para as especies pedidas -- NULL devolvido.")
    return(NULL)
  }

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


## 5. Estatistica formal sobre pct_ok -- CI, tendencia mensal, teste entre especies ----
##
## Camada adicional sobre o MESMO safe_dist_dt das seccoes 1-4 acima --
## reforca o conjunto de provas do relatorio anual perante os financiadores
## (lenders) com incerteza/significancia estatistica, nao so' % pontuais
## (pedido do Paulo, 2026-08). Nao substitui summarise_safe_distance() --
## os resumos existentes continuam inalterados.
##
## Intervalo de confianca: Wilson score (mais robusto que Normal/Wald para
## proporcoes perto de 0%/100% ou com n pequeno) -- so' usa stats::qnorm,
## sem pacotes adicionais.
##
## Uso:
##   ci_overall    <- summarise_safe_distance_ci(safe_dist_dt, prioritysp)
##   ci_by_species <- summarise_safe_distance_ci(safe_dist_dt, prioritysp, by_species = TRUE)
##   by_month      <- summarise_safe_distance_by_month(safe_dist_dt)
##   trend         <- test_safe_distance_trend(safe_dist_dt)
##   species_test  <- test_safe_distance_by_species(safe_dist_dt)

wilson_ci <- function(n_event, n, conf_level = 0.95) {
  z      <- stats::qnorm(1 - (1 - conf_level) / 2)
  phat   <- n_event / n
  denom  <- 1 + z^2 / n
  center <- (phat + z^2 / (2 * n)) / denom
  half   <- (z / denom) * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2))
  data.table::data.table(
    ci_low_pct  = round(100 * pmax(0, center - half), 1),
    ci_high_pct = round(100 * pmin(1, center + half), 1)
  )
}

# n/n_ok/pct_ok + intervalo de confianca de Wilson a conf_level (95% por
# omissao), farm-wide (by_species = FALSE) ou por especie
summarise_safe_distance_ci <- function(safe_dist_dt, species_sel = NULL, by_species = FALSE, conf_level = 0.95) {

  dt <- safe_dist_dt[!is.na(status)]
  if (!is.null(species_sel)) dt <- dt[species %in% species_sel]

  agg <- function(d) d[, .(n = .N, n_ok = sum(status == "OK"))]

  out <- if (isTRUE(by_species)) dt[, agg(.SD), by = species] else agg(dt)
  out[, pct_ok := round(100 * n_ok / n, 1)]
  out <- cbind(out, wilson_ci(out$n_ok, out$n, conf_level))
  if (isTRUE(by_species)) data.table::setorder(out, pct_ok)
  out[]
}

# n/n_ok/pct_ok + CI de Wilson por mes de calendario (start do curtailment),
# farm-wide -- base para o teste de tendencia e o grafico mensal no relatorio
summarise_safe_distance_by_month <- function(safe_dist_dt, conf_level = 0.95) {

  dt <- safe_dist_dt[!is.na(status) & !is.na(start)]
  dt[, month := data.table::as.IDate(format(start, "%Y-%m-01"))]

  out <- dt[, .(n = .N, n_ok = sum(status == "OK")), by = month]
  out[, pct_ok := round(100 * n_ok / n, 1)]
  out <- cbind(out, wilson_ci(out$n_ok, out$n, conf_level))
  data.table::setorder(out, month)
  out[]
}

# Grafico de linha do % OK por mes (saida de summarise_safe_distance_by_month()),
# com banda do IC de Wilson -- visualiza a tendencia testada por
# test_safe_distance_trend(), farm-wide
plot_safe_distance_by_month <- function(by_month_dt) {

  if (nrow(by_month_dt) == 0L) {
    message("plot_safe_distance_by_month(): sem meses com dados -- NULL devolvido.")
    return(NULL)
  }

  ggplot(by_month_dt, aes(x = month, y = pct_ok)) +
    geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), fill = "grey70", alpha = 0.4) +
    geom_line(colour = "#17aeb0") +
    geom_point(colour = "#17aeb0", size = 2) +
    scale_x_date(date_labels = "%Y-%m") +
    labs(
      x = "Month", y = "% OK (95% Wilson CI band)",
      title = "Monthly trend -- % of curtailments within the KNE safe-distance margin"
    ) +
    theme_bw()
}

# Regressao logistica simples (status ~ mes) -- deteta uma tendencia
# monotona de melhoria/degradacao ao longo do periodo coberto, alem do
# grafico mensal (que so' mostra, nao testa, a tendencia). Requer pelo
# menos min_months meses distintos E ambos os estados (OK e Crit)
# presentes -- caso contrario devolve direction = "insufficient_data" em
# vez de um p_value nao fiavel.
test_safe_distance_trend <- function(safe_dist_dt, min_months = 3) {

  dt <- safe_dist_dt[!is.na(status) & !is.na(start)]
  dt[, month_num := as.integer(format(start, "%Y")) * 12L + as.integer(format(start, "%m"))]
  dt[, month_num := month_num - min(month_num)]
  dt[, ok := as.integer(status == "OK")]

  n_months <- data.table::uniqueN(dt$month_num)
  if (n_months < min_months || data.table::uniqueN(dt$ok) < 2) {
    return(list(p_value = NA_real_, direction = "insufficient_data", n_months = n_months))
  }

  fit <- suppressWarnings(stats::glm(ok ~ month_num, data = dt, family = stats::binomial))
  co  <- summary(fit)$coefficients["month_num", ]

  list(
    p_value   = unname(co["Pr(>|z|)"]),
    estimate  = unname(co["Estimate"]),
    direction = if (unname(co["Estimate"]) > 0) "improving" else "worsening",
    n_months  = n_months
  )
}

# Teste de independencia especie x status (OK/Crit) -- omnibus, nao
# pairwise; especies com menos de min_n curtailments ficam de fora (contagem
# demasiado pequena para o teste ser fiavel). Recorre a simulacao de
# Monte-Carlo (chisq.test(..., simulate.p.value = TRUE)) quando a
# aproximacao qui-quadrado classica emite aviso (contagens esperadas
# baixas), em vez de reportar um p-value pouco fiavel em silencio.
test_safe_distance_by_species <- function(safe_dist_dt, min_n = 5) {

  dt <- safe_dist_dt[!is.na(status)]
  counts <- dt[, .N, by = species]
  keep_species <- counts[N >= min_n, species]
  dt <- dt[species %in% keep_species]

  if (data.table::uniqueN(dt$species) < 2) {
    return(list(p_value = NA_real_, method = "insufficient_data", table = NULL))
  }

  tbl <- table(dt$species, dt$status)
  test <- tryCatch(
    stats::chisq.test(tbl),
    warning = function(w) stats::chisq.test(tbl, simulate.p.value = TRUE, B = 2000)
  )

  list(p_value = test$p.value, method = test$method, table = tbl)
}
