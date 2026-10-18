##
## Falhas reportadas pela equipa IDF -- avaliacao da camara WFOV
##
## A equipa IDF reporta avarias (WFOV, PTU, estereo, ...) que NAO se obtem
## diretamente dos dados do portal. Para as avarias do modulo WFOV (conjunto
## das 8 cameras monoculares que fazem a deteccao inicial dos objetos), o
## TrackReport traz a coluna "WFOV_camera" (camara que capturou o objeto):
## descreve-se, na unidade avariada, o n.o de tracks por camara em 3 periodos
## consecutivos (adicional, antes e depois da data da falha) e infere-se, com um
## modelo bayesiano (MCMC) sobre o TOTAL de tracks da unidade, a probabilidade de
## a taxa de tracks por dia ter descido depois da falha.
##
## E' uma evidencia indireta: depende dos passaros que passaram em cada
## janela (amostra pequena => resultados instaveis) e da data da falha
## reportada (pode ser anterior a' data do registo).
##
## Depende de: data.table, ggplot2, lubridate, R/read_utils.R (list_files_multi_dir)
##

if (!exists("idf_unit_number", mode = "function")) {
  idf_unit_number <- function(idf_label) as.integer(sub(".*?([0-9]+)$", "\\1", as.character(idf_label)))
}

## Procura, no cabecalho do TrackReport, a coluna da camara WFOV
## ("WFOV_camera", "WFOVCamera", ... -- ignora maiusculas e separadores)
wfov_find_column <- function(header) {
  norm <- tolower(gsub("[^A-Za-z0-9]", "", header))
  i <- which(norm %in% c("wfovcamera", "wfovcameraid", "wfovcameranumber"))
  if (length(i) == 0L) i <- grep("wfov", norm)
  if (length(i) == 0L) NA_character_ else header[i[1L]]
}

## Leitura leve dos TrackReport: so' TrackId, DateTimeStamp, TowerNumber e a
## camara WFOV, so' das unidades (`units`, codigos ou numeros) e da janela
## [from, to] pedidas. Devolve track_id, timestamp (hora local), idf (numero
## da unidade), camera (texto). Atributos: "all_range" (cobertura temporal de
## todos os ficheiros lidos), "files_without_column" (ficheiros sem coluna
## WFOV), "n_no_camera" (linhas sem camara, descartadas).
read_track_wfov <- function(databases_dirs, pattern, tz, farm_pattern = NULL, units = NULL, from = NULL, to = NULL) {
  files <- list_files_multi_dir(databases_dirs, pattern, farm_pattern)
  empty <- data.table::data.table(track_id = character(), timestamp = as.POSIXct(character(), tz = tz),
                                  idf = integer(), camera = character())
  if (length(files) == 0L) return(empty)
  units_n <- if (is.null(units)) NULL else unique(idf_unit_number(units))

  ranges <- list(); no_col <- character(); n_no_cam <- 0L
  dt <- data.table::rbindlist(lapply(files, function(f) {
    hdr <- names(data.table::fread(f, nrows = 0L))
    cam_col <- wfov_find_column(hdr)
    if (is.na(cam_col)) { no_col <<- c(no_col, basename(f)); return(NULL) }
    x <- data.table::fread(f, select = c("TrackId", "DateTimeStamp", "TowerNumber", cam_col), na.strings = "NULL", header = TRUE)
    data.table::setnames(x, c("track_id", "timestamp", "idf", "camera"))
    # fonte ja' em hora LOCAL: force_tz (mesmo criterio de R/read_tracks.R)
    x[, timestamp := lubridate::force_tz(timestamp, tz)]
    ranges[[length(ranges) + 1L]] <<- range(x$timestamp)
    if (!is.null(units_n)) x <- x[idf %in% units_n]
    if (!is.null(from)) x <- x[timestamp >= from]
    if (!is.null(to)) x <- x[timestamp <= to]
    x[, track_id := as.character(track_id)]
    x[, camera := trimws(as.character(camera))]
    n_no_cam <<- n_no_cam + x[is.na(camera) | !nzchar(camera) | camera == "NA", .N]
    x[!(is.na(camera) | !nzchar(camera) | camera == "NA")]
  }), use.names = TRUE)
  if (is.null(dt) || nrow(dt) == 0L) dt <- empty
  out <- unique(dt)
  attr(out, "all_range") <- if (length(ranges)) range(do.call(c, ranges)) else NULL
  attr(out, "files_without_column") <- no_col
  attr(out, "n_no_camera") <- n_no_cam
  out
}

## Ordena cameras numericamente quando possivel ("2" antes de "10")
wfov_order_cameras <- function(cams) {
  cams <- unique(as.character(cams))
  cams[order(suppressWarnings(as.numeric(gsub("[^0-9]", "", cams))), cams)]
}

## Descricao de UMA falha em 3 periodos consecutivos da MESMA unidade (mesmo espaco
## aereo): periodo adicional ("Earlier period"), "Before" e "After" a data da falha.
##   days_pre / days_before / days_after: duracao (dias) de cada periodo;
##   days_pre = 0 -> so' Before e After (a linha de base e' o Before).
## So' DESCRITIVO (sem testes): tracks por dia por camara e no total, em cada periodo,
## e variacao % do "After" face a' linha de base (Earlier + Before). Cada TRACK conta
## uma vez por camara (a que o capturou). A inferencia e' feita so' sobre o total da
## unidade, em wfov_bayes_total().
## Devolve list(rates, totals, daily, windows, has_pre).
wfov_three_period <- function(wfov_dt, unit, event_time, days_pre = 7, days_before = 7, days_after = 7) {
  tz <- attr(event_time, "tzone"); if (is.null(tz) || !nzchar(tz[1L])) tz <- ""
  has_pre <- !is.null(days_pre) && !is.na(days_pre) && days_pre > 0
  if (!has_pre) days_pre <- 0
  u <- idf_unit_number(unit)
  t0 <- event_time - days_before * 86400          # inicio do Before
  tp <- t0 - days_pre * 86400                     # inicio do Earlier
  t1 <- event_time + days_after * 86400

  x <- data.table::as.data.table(wfov_dt)[idf == u]
  tr <- x[, .(timestamp = min(timestamp)), by = .(track_id, camera)][timestamp >= tp & timestamp < t1]
  trk <- x[, .(timestamp = min(timestamp)), by = track_id][timestamp >= tp & timestamp < t1]
  cams <- wfov_order_cameras(tr$camera)

  per_name <- c(if (has_pre) "Earlier period", "Before", "After")
  per_days <- c(if (has_pre) days_pre, days_before, days_after)
  per_from <- c(if (has_pre) tp, t0, event_time)
  per_to <- c(per_from[-1L], t1)
  K <- length(per_name)
  n_in <- function(dt, k) dt[timestamp >= per_from[k] & timestamp < per_to[k], .N]

  wins <- data.table::data.table(period = per_name, from = per_from, to = per_to, days = per_days)
  wins[, tracks := vapply(seq_len(K), function(k) n_in(trk, k), integer(1))]
  wins[, tracks_per_day := tracks / days]
  wins[, cameras_active := vapply(seq_len(K), function(k) uniqueN(tr[timestamp >= per_from[k] & timestamp < per_to[k], camera]), integer(1))]

  rows <- c(cams, "All cameras")
  cnt <- function(k) c(vapply(cams, function(cm) n_in(tr[camera == cm], k), integer(1)), n_in(trk, k))
  nA <- cnt(K); nB <- cnt(K - 1L); nP <- if (has_pre) cnt(1L) else rep(NA_real_, length(rows))
  base_rate <- ((if (has_pre) nP else 0) + nB) / (if (has_pre) days_pre + days_before else days_before)
  rates <- data.table::data.table(
    camera = rows, n_earlier = nP, n_before = nB, n_after = nA,
    per_day_earlier = if (has_pre) nP / days_pre else NA_real_, per_day_before = nB / days_before, per_day_after = nA / days_after,
    change_vs_baseline_pct = ifelse(base_rate > 0, 100 * ((nA / days_after) / base_rate - 1), NA_real_)
  )
  daily <- tr[, .(n = .N), by = .(date = as.Date(timestamp, tz = tz), camera)]
  list(rates = rates[], totals = wins[], daily = daily[],
       windows = list(from = tp, event = event_time, to = t1), has_pre = has_pre)
}

## Inferencia BAYESIANA sobre o TOTAL de tracks da unidade (taxa por dia depois vs linha de base)
##
## Dados: contagem diaria de tracks da unidade (dias de 24 h alinhados com a hora da falha),
## so' dias cobertos pelos TrackReport (data_range). Grupos: linha de base (Earlier + Before)
## e After.
## Modelo: y_d ~ BinomialNegativa(media = mu_g, tamanho = k), var = mu + mu^2 / k
##         (sobredispersao entre dias: bandos, dias bons e maus);
##         log mu_base = alpha;  log mu_after = alpha + theta;  theta = log(razao depois / antes)
## Priors fracos: alpha ~ N(log(media base observada), 2.5^2); theta ~ N(0, prior_theta_sd^2)
##         (sem assumir queda); log k ~ N(log 5, 1.5^2) (k de ~0.2 a ~100).
## MCMC: Metropolis de passeio aleatorio (n_chains cadeias de n_iter amostras apos n_warmup de
## aquecimento, passo adaptado no aquecimento), R base, sem dependencias. Diagnosticos:
## Rhat (cadeias divididas) e tamanho efetivo da amostra (ESS) de theta.
## Resultado: variacao % (mediana e intervalo credivel de 95%), P(queda) = P(theta < 0 | dados),
## P(queda >= fall_threshold) e a sensibilidade ao prior de theta (sens_sd). A probabilidade e'
## condicional ao modelo e aos priors; nao separa degradacao da unidade de menos aves.
wfov_bayes_total <- function(wfov_dt, unit, event_time, days_pre = 21, days_before = 7, days_after = 7,
                             fall_threshold = 0.33, n_iter = 10000L, n_warmup = 2000L, n_chains = 4L,
                             seed = 1L, prior_theta_sd = 1, sens_sd = c(0.5, 2), data_range = NULL) {
  u <- idf_unit_number(unit)
  if (is.null(days_pre) || is.na(days_pre)) days_pre <- 0
  x <- data.table::as.data.table(wfov_dt)[idf == u]
  trk <- x[, .(timestamp = min(timestamp)), by = track_id]
  days <- seq.int(-(days_pre + days_before), days_after - 1L)
  d0 <- event_time + days * 86400
  keep <- rep(TRUE, length(days))
  if (!is.null(data_range)) keep <- d0 >= data_range[1L] & (d0 + 86400) <= (data_range[2L] + 3600)
  idx <- floor(as.numeric(difftime(trk$timestamp, event_time, units = "days")))
  y_all <- vapply(days, function(d) sum(idx == d), numeric(1))
  y <- y_all[keep]; after <- days[keep] >= 0
  yb <- y[!after]; ya <- y[after]
  res <- list(summary = NULL, sensitivity = NULL, draws = NULL, n_base_days = length(yb), n_after_days = length(ya),
              n_days_excluded = sum(!keep), threshold = fall_threshold)
  if (length(yb) < 2L || length(ya) < 2L || sum(y) == 0) return(res)

  a0 <- log(mean(yb) + 0.5)
  logpost <- function(par, sd_theta) {
    a <- par[1L]; th <- par[2L]; lk <- par[3L]; k <- exp(lk)
    sum(stats::dnbinom(yb, size = k, mu = exp(a), log = TRUE)) + sum(stats::dnbinom(ya, size = k, mu = exp(a + th), log = TRUE)) +
      stats::dnorm(a, a0, 2.5, log = TRUE) + stats::dnorm(th, 0, sd_theta, log = TRUE) + stats::dnorm(lk, log(5), 1.5, log = TRUE)
  }
  run_chain <- function(sd_theta) {
    par <- c(a0 + stats::rnorm(1, 0, 0.3), stats::rnorm(1, 0, 0.3), log(5) + stats::rnorm(1, 0, 0.5))
    lp <- logpost(par, sd_theta); step <- c(0.1, 0.2, 0.3); acc <- 0L
    out <- matrix(NA_real_, n_iter, 3L)
    for (i in seq_len(n_warmup + n_iter)) {
      prop <- par + stats::rnorm(3L, 0, step)
      lp2 <- logpost(prop, sd_theta)
      if (is.finite(lp2) && log(stats::runif(1L)) < lp2 - lp) { par <- prop; lp <- lp2; acc <- acc + 1L }
      if (i <= n_warmup && i %% 100L == 0L) {            # adapta o passo para ~25% de aceitacao
        step <- step * if (acc / 100 > 0.35) 1.3 else if (acc / 100 < 0.15) 0.7 else 1; acc <- 0L
      }
      if (i > n_warmup) out[i - n_warmup, ] <- par
    }
    out
  }
  run_all <- function(sd_theta) lapply(seq_len(n_chains), function(ch) run_chain(sd_theta))

  old <- NULL; has_seed <- !is.null(seed)
  if (has_seed) { old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL; set.seed(seed) }
  main <- run_all(prior_theta_sd)
  sens <- lapply(sens_sd, function(s) run_all(s))
  if (has_seed && !is.null(old)) assign(".Random.seed", old, envir = globalenv())

  rhat <- function(ch) {                                 # Rhat de cadeias divididas
    h <- floor(nrow(ch[[1L]]) / 2)
    parts <- do.call(c, lapply(ch, function(m) list(m[seq_len(h), 2L], m[h + seq_len(h), 2L])))
    n <- h; m <- length(parts); mu <- vapply(parts, mean, numeric(1)); s2 <- vapply(parts, stats::var, numeric(1))
    W <- mean(s2); B <- n * stats::var(mu)
    sqrt(((n - 1) / n * W + B / n) / W)
  }
  ess <- function(ch) {
    n <- nrow(ch[[1L]])
    rho <- rowMeans(vapply(ch, function(m) stats::acf(m[, 2L], lag.max = 100L, plot = FALSE)$acf[-1L], numeric(100L)))
    cut <- which(rho < 0.05)[1L]; if (is.na(cut)) cut <- 100L
    n * length(ch) / (1 + 2 * sum(rho[seq_len(cut - 1L)]))
  }
  th_of <- function(ch) unlist(lapply(ch, function(m) m[, 2L]))
  summ <- function(ch) {
    th <- th_of(ch); chg <- 100 * (exp(th) - 1)
    data.table::data.table(
      median_change_pct = stats::median(chg), ci_low_pct = unname(stats::quantile(chg, 0.025)), ci_high_pct = unname(stats::quantile(chg, 0.975)),
      p_fall = mean(th < 0), p_fall_threshold = mean(th <= log(1 - fall_threshold)))
  }
  s_main <- summ(main)
  s_main[, `:=`(threshold_pct = 100 * fall_threshold, base_per_day = mean(yb), after_per_day = mean(ya),
                n_base_days = length(yb), n_after_days = length(ya), rhat = rhat(main), ess = ess(main),
                prior_theta_sd = prior_theta_sd)]
  sens_dt <- data.table::rbindlist(c(list(cbind(data.table::data.table(prior_theta_sd = prior_theta_sd), s_main[, .(p_fall, p_fall_threshold)])),
    Map(function(s, ch) cbind(data.table::data.table(prior_theta_sd = s), summ(ch)[, .(p_fall, p_fall_threshold)]), sens_sd, sens)))
  data.table::setorder(sens_dt, prior_theta_sd)
  dr <- th_of(main)
  res$summary <- s_main; res$sensitivity <- sens_dt
  res$draws <- 100 * (exp(dr[round(seq(1, length(dr), length.out = min(4000L, length(dr))))]) - 1)
  res
}

## Contexto da mesma janela com as outras fontes: heartbeats por dia da
## unidade, curtailments e leituras SCADA das turbinas indicadas, antes/depois.
## Taxas por dia COBERTO pelos dados de cada fonte (NA se a janela nao tem dados).
wfov_event_context <- function(unit, event_time, days_before, days_after, heartb_dt, curtl_dt, scada_dt, turbines = NULL, days_pre = 0) {
  t0 <- event_time - days_before * 86400
  t1 <- event_time + days_after * 86400
  covered_days <- function(a, b, rng) {
    if (is.null(rng) || any(is.na(rng))) return(0)
    max(as.numeric(difftime(min(b, rng[2L]), max(a, rng[1L]), units = "days")), 0)
  }
  rate <- function(n, a, b, rng) {
    d <- covered_days(a, b, rng)
    if (d <= 0) NA_real_ else n / d
  }
  u <- idf_unit_number(unit)
  hb <- data.table::as.data.table(heartb_dt)[idf_unit_number(idf) == u]
  hb_rng <- if (nrow(heartb_dt) > 0L) range(heartb_dt$timestamp) else NULL
  rows <- list()
  add <- function(metric, f) {
    rows[[length(rows) + 1L]] <<- data.table::data.table(
      metric = metric, earlier = if (days_pre > 0) f(t0 - days_pre * 86400, t0) else NA_real_, before = f(t0, event_time), after = f(event_time, t1))
  }
  add("Heartbeats of the unit (per day)", function(a, b) rate(hb[timestamp >= a & timestamp < b, .N], a, b, hb_rng))
  if (!is.null(turbines) && length(turbines) > 0L) {
    tb <- paste(turbines, collapse = ", ")
    cu <- data.table::as.data.table(curtl_dt)[turbine %in% turbines]
    cu_rng <- if (nrow(curtl_dt) > 0L) range(curtl_dt$start) else NULL
    add(sprintf("Curtailments, turbine(s) %s (per day)", tb), function(a, b) rate(cu[start >= a & start < b, .N], a, b, cu_rng))
    sc <- data.table::as.data.table(scada_dt)[turbinelabel %in% turbines]
    sc_rng <- if (nrow(scada_dt) > 0L) range(scada_dt$datetime) else NULL
    add(sprintf("SCADA readings, turbine(s) %s (per day)", tb), function(a, b) rate(sc[datetime >= a & datetime < b, .N], a, b, sc_rng))
  }
  out <- data.table::rbindlist(rows)
  out[, change_pct := ifelse(is.na(before) | before == 0 | is.na(after), NA_real_, 100 * (after - before) / before)]
  out[]
}

## Graficos (devolvem list(p, h) como o resto dos calendarios do relatorio)
plot_wfov_rates <- function(res, title) {
  r <- res$rates[camera != "All cameras"]
  if (nrow(r) == 0L) return(NULL)
  d <- rbind(
    if (isTRUE(res$has_pre)) data.frame(camera = r$camera, period = "Earlier period", per_day = r$per_day_earlier),
    data.frame(camera = r$camera, period = "Before", per_day = r$per_day_before),
    data.frame(camera = r$camera, period = "After", per_day = r$per_day_after)
  )
  d$camera <- factor(d$camera, levels = r$camera)
  d$period <- factor(d$period, levels = c("Earlier period", "Before", "After"))
  p <- ggplot2::ggplot(d, ggplot2::aes(camera, per_day, fill = period)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::scale_fill_manual(values = c("Earlier period" = "#9DB4D0", Before = "#4C72B0", After = "#DD8452"), name = NULL) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(title = title, subtitle = "Tracks per day captured by each WFOV camera, by period",
                  x = "WFOV camera", y = "Tracks per day") +
    ggplot2::theme_minimal(base_size = 9) + ggplot2::theme(legend.position = "bottom")
  list(p = p, h = 3.4)
}

plot_wfov_daily <- function(res, title) {
  d <- res$daily
  if (nrow(d) == 0L) return(NULL)
  r <- res$rates[camera != "All cameras"]
  d <- data.table::copy(d)
  d[, camera := factor(camera, levels = rev(r$camera))]
  ev <- as.Date(res$windows$event, tz = attr(res$windows$event, "tzone"))
  days <- seq(as.Date(res$windows$from, tz = attr(res$windows$from, "tzone")),
              as.Date(res$windows$to, tz = attr(res$windows$to, "tzone")), by = "day")
  p <- ggplot2::ggplot(d, ggplot2::aes(date, camera, fill = n)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = n), size = 2.4) +
    ggplot2::geom_vline(xintercept = as.numeric(ev), linetype = "dashed", colour = "red") +
    ggplot2::scale_fill_gradient(low = "#F2F6FA", high = "#2B5C8A", name = "Tracks") +
    ggplot2::scale_x_date(limits = range(days) + c(-0.5, 0.5), breaks = days, date_labels = "%d/%m") +
    ggplot2::labs(title = title, subtitle = "Tracks per day and WFOV camera (dashed line = reported failure date)",
                  x = NULL, y = "WFOV camera") +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))
  list(p = p, h = 2.0 + 0.3 * uniqueN(d$camera))
}

## Distribuicao posterior da variacao % de tracks por dia (depois vs linha de base)
plot_wfov_posterior <- function(bayes, title) {
  if (is.null(bayes$draws)) return(NULL)
  dd <- stats::density(bayes$draws, from = max(-100, min(bayes$draws)), to = max(bayes$draws), n = 512)
  d <- data.frame(x = dd$x, y = dd$y)
  thr <- -100 * bayes$threshold
  s <- bayes$summary
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_area(data = d[d$x <= thr, ], fill = "#DD8452", alpha = 0.7) +
    ggplot2::geom_line(colour = "#2B5C8A") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
    ggplot2::geom_vline(xintercept = thr, linetype = "dotted", colour = "#C44E52") +
    ggplot2::labs(title = title,
                  subtitle = sprintf("Posterior of the change in tracks per day (shaded: fall of %.0f%% or more, probability %.0f%%)", 100 * bayes$threshold, 100 * s$p_fall_threshold),
                  x = "Change in tracks per day, after vs baseline (%)", y = "Posterior density") +
    ggplot2::theme_minimal(base_size = 9) + ggplot2::theme(axis.text.y = ggplot2::element_blank())
  list(p = p, h = 2.8)
}
