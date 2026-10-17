##
## Falhas reportadas pela equipa IDF -- avaliacao da camara WFOV
##
## A equipa IDF reporta avarias (WFOV, PTU, estereo, ...) que NAO se obtem
## diretamente dos dados do portal. Para as avarias do modulo WFOV (conjunto
## das 8 cameras monoculares que fazem a deteccao inicial dos objetos), o
## TrackReport traz a coluna "WFOV_camera" (camara que capturou o objeto):
## compara-se, na unidade avariada, o n.o de tracks por camara em 3 periodos
## consecutivos (adicional, antes e depois da data da falha), com um teste de
## permutacao entre os 3 periodos. Uma camara que deixa de capturar objetos
## aparece como degradacao apos a falha.
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

## Analise de UMA falha em 3 periodos consecutivos da MESMA unidade (mesmo espaco
## aereo): periodo adicional ("Earlier period"), "Before" e "After" a data da falha.
##   days_pre / days_before / days_after: duracao (dias) de cada periodo;
##   days_pre = 0 -> so' Before e After (a linha de base e' o Before).
## Metricas por camara (e para todas as cameras): n.o de tracks por dia e quota (%)
## de cada camara. Cada TRACK conta uma vez por camara (a que o capturou).
##
## Teste de PERMUTACAO Monte Carlo ENTRE OS 3 PERIODOS: as janelas sao divididas em
## BLOCOS de tempo, os rotulos de periodo sao reatribuidos aleatoriamente aos blocos
## (mantendo o n.o de blocos de cada periodo) e as estatisticas sao recalculadas.
## Hipotese nula: os 3 periodos sao permutaveis (nada mudou na unidade).
## block_hours = 24 (omissao): o DIA e' a unidade independente (ritmo diurno e dias
## "bons"/"maus" de passagem); blocos mais curtos tratam horas do mesmo dia como
## independentes e dao p-values demasiado otimistas (simulacao: ~12% de falsos
## positivos com 6 h, ~3% com 24 h).
##   Degradacao (unilateral, "depois" PIOR que a linha de base Earlier + Before):
##     taxa:  D = log(taxa base / taxa depois)   (positivo = menos tracks por dia)
##     quota: D = log(quota base / quota depois) (positivo = a camara perdeu peso)
##     p = (k + 1) / (N + 1), k = permutacoes com D >= observado
##     p ajustado = igual, comparando com o MAXIMO de D entre as cameras (comparacoes multiplas)
## Efeito com incerteza: variacao % depois vs linha de base (tracks por dia) com
## intervalo de 95% por bootstrap de DIAS (reamostragem dos dias da linha de base e dos
## dias do "After", separadamente; percentis 2.5% e 97.5%).
## A variacao Before vs Earlier (sem falha) e' so' descritiva: mostra a oscilacao
## natural entre periodos da propria unidade.
## Devolve list(rates, shares, totals, daily, windows, has_pre, p_total, p_any_camera, p_any_share).
wfov_three_period <- function(wfov_dt, unit, event_time, days_pre = 7, days_before = 7, days_after = 7,
                              n_perm = 10000L, block_hours = 24, seed = 1L, n_boot = 5000L) {
  tz <- attr(event_time, "tzone"); if (is.null(tz) || !nzchar(tz[1L])) tz <- ""
  if (is.null(block_hours) || is.na(block_hours) || block_hours <= 0) block_hours <- 24
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
  nc <- length(cams)

  per_name <- c(if (has_pre) "Earlier period", "Before", "After")
  per_days <- c(if (has_pre) days_pre, days_before, days_after)
  per_from <- c(if (has_pre) tp, t0, event_time)
  K <- length(per_name)

  # matriz de contagens (blocos x cameras + total) para uma duracao de bloco em horas
  count_matrix <- function(bh) {
    nb <- as.integer(ceiling(per_days * 24 / bh))
    off <- c(0L, cumsum(nb))[seq_len(K)]
    blk_of <- function(ts) {
      k <- pmax(findInterval(as.numeric(ts), as.numeric(per_from)), 1L)
      h <- as.numeric(difftime(ts, per_from[k], units = "hours")) / bh
      off[k] + pmin(floor(h) + 1L, nb[k])
    }
    M <- matrix(0L, sum(nb), nc + 1L)
    if (nrow(tr) > 0L) {
      bi <- blk_of(tr$timestamp); ci <- match(tr$camera, cams)
      for (k in seq_len(nrow(tr))) M[bi[k], ci[k]] <- M[bi[k], ci[k]] + 1L
    }
    if (nrow(trk) > 0L) M[, nc + 1L] <- tabulate(blk_of(trk$timestamp), nrow(M))
    list(M = M, lab = rep(seq_len(K), nb))
  }
  cm <- count_matrix(block_hours); M <- cm$M; lab_obs <- cm$lab

  eps <- 0.5
  base_days <- if (has_pre) days_pre + days_before else days_before
  stats_for <- function(lab) {
    cnt <- rowsum(M, lab, reorder = TRUE)           # K x (nc + 1), linhas = periodos 1..K
    cA <- cnt[K, ]
    c_base <- if (has_pre) cnt[1L, ] + cnt[K - 1L, ] else cnt[K - 1L, ]
    D_rate <- log((c_base + eps) / base_days) - log((cA + eps) / days_after)
    share <- function(cn) (cn[seq_len(nc)] + eps) / (sum(cn[seq_len(nc)]) + eps * nc)
    D_share <- if (nc > 0L) log(share(c_base)) - log(share(cA)) else numeric(0)
    c(D_rate, D_share)
  }
  idx_rate <- seq_len(nc + 1L); idx_share <- nc + 1L + seq_len(nc)
  obs <- stats_for(lab_obs)

  total_tracks <- sum(M[, nc + 1L])
  old <- NULL; has_seed <- !is.null(seed)
  if (has_seed) { old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL; set.seed(seed) }
  perm <- NULL
  if (total_tracks > 0L && n_perm >= 1L) {
    perm <- matrix(NA_real_, n_perm, length(obs))
    for (i in seq_len(n_perm)) perm[i, ] <- stats_for(sample(lab_obs))
  }

  # bootstrap de DIAS (blocos de 24 h) para o intervalo da variacao % (tracks por dia)
  dm <- count_matrix(24); Md <- dm$M; lab_d <- dm$lab
  grp <- ifelse(lab_d == K, "after", "base")
  ci_pct <- matrix(NA_real_, nc + 1L, 2L)
  if (total_tracks > 0L && n_boot >= 1L && any(grp == "after") && any(grp == "base")) {
    ib <- which(grp == "base"); ia <- which(grp == "after")
    bs <- matrix(NA_real_, n_boot, nc + 1L)
    for (b in seq_len(n_boot)) {
      mb <- colMeans(Md[sample(ib, length(ib), TRUE), , drop = FALSE])
      ma <- colMeans(Md[sample(ia, length(ia), TRUE), , drop = FALSE])
      bs[b, ] <- ifelse(mb > 0, 100 * (ma / mb - 1), NA_real_)
    }
    ci_pct <- t(apply(bs, 2L, function(v) if (all(is.na(v))) c(NA_real_, NA_real_) else stats::quantile(v, c(0.025, 0.975), na.rm = TRUE, names = FALSE)))
  }
  if (has_seed && !is.null(old)) assign(".Random.seed", old, envir = globalenv())

  p_one <- function(cols, o) {            # unilateral (degradacao): p bruto e ajustado pelo maximo
    if (is.null(perm) || length(cols) == 0L) return(list(p = rep(NA_real_, length(cols)), adj = rep(NA_real_, length(cols))))
    pm <- perm[, cols, drop = FALSE]; N <- nrow(pm)
    mx <- apply(pm, 1L, max)
    list(p = vapply(seq_along(cols), function(j) (sum(pm[, j] >= o[j] - 1e-12) + 1) / (N + 1), numeric(1)),
         adj = vapply(seq_along(cols), function(j) (sum(mx >= o[j] - 1e-12) + 1) / (N + 1), numeric(1)))
  }

  cnt_obs <- rowsum(M, lab_obs, reorder = TRUE)
  rows <- c(cams, "All cameras")
  nP <- if (has_pre) cnt_obs[1L, ] else rep(NA_real_, nc + 1L); nB <- cnt_obs[K - 1L, ]; nA <- cnt_obs[K, ]
  d_P <- if (has_pre) days_pre else NA_real_

  # --- taxas (tracks/dia) ---
  Dr <- obs[idx_rate]
  pr <- p_one(idx_rate[seq_len(nc)], Dr[seq_len(nc)])
  pr_tot <- p_one(idx_rate[nc + 1L], Dr[nc + 1L])$p
  # variacao % observada da media diaria (sem pseudo-contagem): depois vs linha de base
  base_rate <- ((if (has_pre) nP else 0) + nB) / base_days
  chg <- ifelse(base_rate > 0, 100 * ((nA / days_after) / base_rate - 1), NA_real_)
  rates <- data.table::data.table(
    camera = rows,
    n_earlier = nP, n_before = nB, n_after = nA,
    per_day_earlier = nP / d_P, per_day_before = nB / days_before, per_day_after = nA / days_after,
    change_vs_baseline_pct = chg, ci_low_pct = ci_pct[, 1L], ci_high_pct = ci_pct[, 2L],
    p_degradation = c(pr$p, pr_tot), p_degradation_adj = c(pr$adj, pr_tot),
    change_before_vs_earlier_pct = if (has_pre) ifelse(nP > 0, 100 * (nB / days_before) / (nP / d_P) - 100, NA_real_) else NA_real_
  )
  if (!has_pre) rates[, c("per_day_earlier", "n_earlier") := NA_real_]

  # --- quotas (%) de cada camara no total de tracks-camara ---
  tot_cam <- function(v) sum(v[seq_len(nc)])
  pct <- function(v) if (nc == 0L || is.na(tot_cam(v)) || tot_cam(v) == 0) rep(NA_real_, nc) else 100 * v[seq_len(nc)] / tot_cam(v)
  base_cnt <- if (has_pre) nP + nB else nB
  ps <- p_one(idx_share, obs[idx_share])
  shares <- data.table::data.table(
    camera = cams,
    pct_earlier = if (has_pre) pct(nP) else NA_real_, pct_before = pct(nB), pct_after = pct(nA)
  )
  shares[, change_vs_baseline_pp := pct_after - pct(base_cnt)]
  shares[, p_degradation := ps$p]; shares[, p_degradation_adj := ps$adj]
  shares[, change_before_vs_earlier_pp := if (has_pre) pct_before - pct_earlier else NA_real_]

  wins <- data.table::data.table(
    period = per_name, from = per_from, to = c(per_from[-1L], t1), days = per_days)
  wins[, tracks := as.integer(cnt_obs[, nc + 1L])]
  wins[, tracks_per_day := tracks / days]
  wins[, cameras_active := vapply(seq_len(K), function(k) sum(cnt_obs[k, seq_len(nc)] > 0), integer(1))]

  daily <- tr[, .(n = .N), by = .(date = as.Date(timestamp, tz = tz), camera)]
  list(rates = rates[], shares = shares[], totals = wins[], daily = daily[],
       windows = list(from = tp, event = event_time, to = t1), has_pre = has_pre,
       p_total = pr_tot, p_any_camera = if (nc > 0L) suppressWarnings(min(pr$adj, na.rm = TRUE)) else NA_real_,
       p_any_share = if (nc > 0L) suppressWarnings(min(ps$adj, na.rm = TRUE)) else NA_real_,
       n_perm = if (is.null(perm)) 0L else nrow(perm), block_hours = block_hours)
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
    if (isTRUE(res$has_pre)) data.frame(camera = r$camera, period = "Earlier period", per_day = r$per_day_earlier, n = r$n_earlier),
    data.frame(camera = r$camera, period = "Before", per_day = r$per_day_before, n = r$n_before),
    data.frame(camera = r$camera, period = "After", per_day = r$per_day_after, n = r$n_after)
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
