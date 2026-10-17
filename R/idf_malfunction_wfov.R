##
## Falhas reportadas pela equipa IDF -- avaliacao da camara WFOV
##
## A equipa IDF reporta avarias (WFOV, PTU, estereo, ...) que NAO se obtem
## diretamente dos dados do portal. Para as avarias do modulo WFOV (conjunto
## das 8 cameras monoculares que fazem a deteccao inicial dos objetos), o
## TrackReport traz a coluna "WFOV_camera" (camara que capturou o objeto):
## compara-se, para a unidade avariada, a distribuicao dos tracks pelas
## cameras numa janela ANTES e DEPOIS da data da falha. Uma camara que deixa
## de capturar objetos aparece como quebra do padrao.
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

## Comparacao antes/depois para UMA falha.
##   wfov_dt: read_track_wfov() (qualquer unidade; filtra-se aqui por `unit`)
##   unit: codigo/numero da unidade; event_time: POSIXct da falha
##   days_before/days_after: dias das janelas [event - d, event) e [event, event + d]
##   drop_rel: uma camara "perde peso" se a sua % depois <= drop_rel x % antes
##   n_perm / block_hours / seed: teste de PERMUTACAO Monte Carlo (ver wfov_permutation_test)
## Cada TRACK conta uma vez por camara (a camara que o capturou).
## Devolve list(summary, daily, totals, windows, coverage_ok).
wfov_camera_compare <- function(wfov_dt, unit, event_time, days_before = 5, days_after = 5, drop_rel = 0.5,
                                n_perm = 10000L, block_hours = 6, seed = 1L) {
  tz <- attr(event_time, "tzone"); if (is.null(tz) || !nzchar(tz[1L])) tz <- ""
  t0 <- event_time - days_before * 86400
  t1 <- event_time + days_after * 86400
  u <- idf_unit_number(unit)

  x <- data.table::as.data.table(wfov_dt)[idf == u]
  # 1 linha por track x camara; instante = primeiro registo do track nessa camara
  tr <- x[, .(timestamp = min(timestamp)), by = .(track_id, camera)]
  tr <- tr[timestamp >= t0 & timestamp <= t1]
  tr[, period := ifelse(timestamp < event_time, "before", "after")]

  cams <- wfov_order_cameras(tr$camera)
  n_b <- tr[period == "before", .N]; n_a <- tr[period == "after", .N]
  summ <- data.table::data.table(camera = cams)
  summ[, n_before := vapply(camera, function(c) tr[period == "before" & camera == c, .N], integer(1))]
  summ[, n_after  := vapply(camera, function(c) tr[period == "after"  & camera == c, .N], integer(1))]
  summ[, pct_before := if (n_b > 0L) 100 * n_before / n_b else NA_real_]
  summ[, pct_after  := if (n_a > 0L) 100 * n_after / n_a else NA_real_]
  summ[, delta_pp := pct_after - pct_before]
  perm <- wfov_permutation_test(tr, cams, event_time, days_before, days_after, n_perm, block_hours, seed)
  summ[, p_perm := perm$p_camera]
  summ[, p_adj := perm$p_adjusted]
  summ[, status := data.table::fcase(
    n_before == 0L & n_after == 0L, "No tracks",
    n_before > 0L & n_after == 0L, "No tracks after the event",
    n_before == 0L & n_after > 0L, "Only after the event",
    !is.na(pct_before) & !is.na(pct_after) & pct_after <= drop_rel * pct_before, "Share fell by half or more",
    !is.na(pct_before) & !is.na(pct_after) & pct_after >= pct_before / drop_rel, "Share at least doubled",
    default = "No marked change"
  )]

  days_n <- function(a, b) max(as.numeric(difftime(b, a, units = "days")), 0)
  totals <- data.table::data.table(
    period = c("Before", "After"),
    from = c(t0, event_time), to = c(event_time, t1),
    tracks = c(n_b, n_a),
    tracks_per_day = c(n_b / days_before, n_a / days_after),
    cameras_active = c(uniqueN(tr[period == "before", camera]), uniqueN(tr[period == "after", camera]))
  )

  daily <- tr[, .(n = .N), by = .(date = as.Date(timestamp, tz = tz), camera)]
  list(summary = summ[], daily = daily[], totals = totals[], omnibus = perm$omnibus,
       windows = list(from = t0, event = event_time, to = t1),
       n_cameras_before = uniqueN(tr[period == "before", camera]))
}


## Teste de PERMUTACAO Monte Carlo (em vez de Fisher/qui-quadrado).
## Hipotese nula: a distribuicao dos tracks pelas cameras nao depende de a janela
## ser antes ou depois da falha. Para respeitar a dependencia temporal (varios
## tracks do mesmo bando/periodo), permutam-se BLOCOS de tempo (block_hours h,
## alinhados com a data da falha, incluindo blocos sem tracks) e nao tracks
## individuais: os rotulos "antes/depois" sao reatribuidos aleatoriamente aos
## blocos, mantendo o n.o de blocos de cada janela. Em cada permutacao calcula-se,
## por camara, a variacao da % (depois - antes).
##   p_camera:   proporcao de permutacoes com |variacao| >= observada ((k+1)/(N+1))
##   p_adjusted: igual, mas comparando com o MAXIMO |variacao| entre as camaras em
##               cada permutacao (controla comparacoes multiplas entre as cameras)
##   omnibus:    p-value global = p ajustado da camara com maior |variacao| (a soma das
##               |variacoes| dilui-se pelo ruido das restantes cameras e perde poder)
## block_hours = NULL/NA/<=0: blocos de 1 track (equivale a permutar tracks).
## Resultado depende de n_perm (erro Monte Carlo ~ sqrt(p(1-p)/N)) e da seed.
wfov_permutation_test <- function(tr, cams, event_time, days_before, days_after, n_perm = 10000L, block_hours = 6, seed = 1L) {
  nc <- length(cams)
  na_out <- list(p_camera = rep(NA_real_, nc), p_adjusted = rep(NA_real_, nc), omnibus = NA_real_)
  if (nc == 0L || nrow(tr) == 0L || n_perm < 1L) return(na_out)

  per_side <- function(d) if (is.null(block_hours) || is.na(block_hours) || block_hours <= 0) NA_integer_ else as.integer(ceiling(d * 24 / block_hours))
  nb_b <- per_side(days_before); nb_a <- per_side(days_after)
  if (is.na(nb_b)) {
    # blocos de 1 track: cada linha e' um bloco
    blk <- seq_len(nrow(tr)); n_blk <- nrow(tr)
    n_before_blk <- tr[, sum(timestamp < event_time)]
  } else {
    h <- as.numeric(difftime(tr$timestamp, event_time, units = "hours")) / block_hours
    blk <- floor(h) + nb_b + 1L                      # 1 .. nb_b + nb_a
    blk <- pmin(pmax(blk, 1L), nb_b + nb_a)
    n_blk <- nb_b + nb_a; n_before_blk <- nb_b
  }
  # matriz blocos x cameras com a contagem de tracks
  B <- matrix(0L, n_blk, nc, dimnames = list(NULL, cams))
  ci <- match(tr$camera, cams)
  for (k in seq_len(nrow(tr))) B[blk[k], ci[k]] <- B[blk[k], ci[k]] + 1L
  is_before <- if (is.na(nb_b)) {
    # blocos de 1 track: o rotulo observado vem do instante de cada track
    tr$timestamp < event_time
  } else seq_len(n_blk) <= nb_b
  stat <- function(sel) {
    cb <- colSums(B[sel, , drop = FALSE]); ca <- colSums(B[!sel, , drop = FALSE])
    nb <- sum(cb); na <- sum(ca)
    if (nb == 0 || na == 0) return(rep(NA_real_, nc))
    100 * ca / na - 100 * cb / nb
  }
  obs <- stat(is_before)
  if (anyNA(obs)) return(na_out)

  if (!is.null(seed)) { old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL; set.seed(seed) }
  perm <- matrix(NA_real_, n_perm, nc)
  for (i in seq_len(n_perm)) {
    sel <- logical(n_blk); sel[sample.int(n_blk, n_before_blk)] <- TRUE
    perm[i, ] <- stat(sel)
  }
  if (!is.null(seed) && !is.null(old)) assign(".Random.seed", old, envir = globalenv())
  perm <- perm[stats::complete.cases(perm), , drop = FALSE]
  N <- nrow(perm)
  if (N == 0L) return(na_out)
  ap <- abs(perm); ao <- abs(obs)
  p_cam <- vapply(seq_len(nc), function(j) (sum(ap[, j] >= ao[j] - 1e-12) + 1) / (N + 1), numeric(1))
  mx <- apply(ap, 1L, max)
  p_adj <- vapply(seq_len(nc), function(j) (sum(mx >= ao[j] - 1e-12) + 1) / (N + 1), numeric(1))
  om <- min(p_adj)
  list(p_camera = p_cam, p_adjusted = p_adj, omnibus = om, n_perm_used = N)
}

## Contexto da mesma janela com as outras fontes: heartbeats por dia da
## unidade, curtailments e leituras SCADA das turbinas indicadas, antes/depois.
## Taxas por dia COBERTO pelos dados de cada fonte (NA se a janela nao tem dados).
wfov_event_context <- function(unit, event_time, days_before, days_after, heartb_dt, curtl_dt, scada_dt, turbines = NULL) {
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
      metric = metric, before = f(t0, event_time), after = f(event_time, t1))
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
plot_wfov_share <- function(res, title) {
  s <- res$summary
  if (nrow(s) == 0L) return(NULL)
  d <- rbind(
    data.frame(camera = s$camera, period = "Before", pct = s$pct_before, n = s$n_before),
    data.frame(camera = s$camera, period = "After", pct = s$pct_after, n = s$n_after)
  )
  d$camera <- factor(d$camera, levels = s$camera)
  d$period <- factor(d$period, levels = c("Before", "After"))
  d$pct[is.na(d$pct)] <- 0
  p <- ggplot2::ggplot(d, ggplot2::aes(camera, pct, fill = period)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = n), position = ggplot2::position_dodge(width = 0.8), vjust = -0.4, size = 2.6) +
    ggplot2::scale_fill_manual(values = c(Before = "#4C72B0", After = "#DD8452"), name = NULL) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(title = title, subtitle = "Share of tracks captured by each WFOV camera (labels = number of tracks)",
                  x = "WFOV camera", y = "Tracks (%)") +
    ggplot2::theme_minimal(base_size = 9) + ggplot2::theme(legend.position = "bottom")
  list(p = p, h = 3.4)
}

plot_wfov_daily <- function(res, title) {
  d <- res$daily
  if (nrow(d) == 0L) return(NULL)
  s <- res$summary
  d <- data.table::copy(d)
  d[, camera := factor(camera, levels = rev(s$camera))]
  ev <- as.Date(res$windows$event, tz = attr(res$windows$event, "tzone"))
  days <- seq(as.Date(res$windows$from, tz = attr(res$windows$from, "tzone")),
              as.Date(res$windows$to, tz = attr(res$windows$to, "tzone")), by = "day")
  p <- ggplot2::ggplot(d, ggplot2::aes(date, camera, fill = n)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = n), size = 2.6) +
    ggplot2::geom_vline(xintercept = as.numeric(ev), linetype = "dashed", colour = "red") +
    ggplot2::scale_fill_gradient(low = "#F2F6FA", high = "#2B5C8A", name = "Tracks") +
    ggplot2::scale_x_date(limits = range(days) + c(-0.5, 0.5), breaks = days, date_labels = "%d/%m") +
    ggplot2::labs(title = title, subtitle = "Tracks per day and WFOV camera (dashed line = reported failure date)",
                  x = NULL, y = "WFOV camera") +
    ggplot2::theme_minimal(base_size = 9) + ggplot2::theme(panel.grid = ggplot2::element_blank())
  list(p = p, h = 1.6 + 0.3 * uniqueN(d$camera))
}


##
## Benchmark com unidades vizinhas
##
## O volume de tracks de uma unidade depende da atividade REAL das aves, que
## nao se conhece: uma queda de 657 -> 432 tracks pode ser da unidade ou da
## passagem de aves. Unidades vizinhas a funcionar normalmente, no MESMO
## periodo, dao a referencia: se a atividade desceu em todas, a variacao da
## unidade avariada nao e' especifica dela. A medida e' a razao
## (tracks/dia depois) / (tracks/dia antes) da unidade, dividida pela mesma
## razao nas unidades de referencia (agrupadas).
##

## Escolhe as unidades de referencia: as `k` mais proximas (distancia entre
## unidades IDF, CRS metrico) com heartbeats completos em TODA a janela
## (>= min_completeness do esperado), excluindo a propria unidade e as unidades
## em `exclude_units` (ex: outras com falha reportada nessa janela).
## idf_sf: pontos das unidades (coluna id_col, rotulo/numero da unidade).
## heartb_dt: idf (codigo completo), timestamp. Devolve data.table com
## unit_number, unit (codigo completo), dist_m, completeness (ordenada por distancia).
select_benchmark_units <- function(unit, event_time, days_before, days_after, idf_sf, heartb_dt, interval_min = 30,
                                   k = 3L, max_dist_m = Inf, min_completeness = 0.9, exclude_units = NULL, id_col = "imaging_he") {
  empty <- data.table::data.table(unit_number = integer(), unit = character(), dist_m = numeric(), completeness = numeric())
  u <- idf_unit_number(unit)
  num <- idf_unit_number(idf_sf[[id_col]])
  i0 <- which(num == u)
  if (length(i0) == 0L) return(empty)
  xy <- sf::st_coordinates(sf::st_geometry(idf_sf))
  d <- sqrt((xy[, 1L] - xy[i0[1L], 1L])^2 + (xy[, 2L] - xy[i0[1L], 2L])^2)
  cand <- data.table::data.table(unit_number = num, dist_m = d)[!is.na(unit_number) & unit_number != u & dist_m <= max_dist_m]
  if (!is.null(exclude_units)) cand <- cand[!unit_number %in% idf_unit_number(exclude_units)]
  if (nrow(cand) == 0L) return(empty)
  cand <- unique(cand[order(dist_m)], by = "unit_number")

  t0 <- event_time - days_before * 86400; t1 <- event_time + days_after * 86400
  hb <- data.table::as.data.table(heartb_dt)
  hb[, unit_number := idf_unit_number(idf)]
  hb_rng <- if (nrow(hb) > 0L) range(hb$timestamp) else NULL
  win_ok <- !is.null(hb_rng) && hb_rng[1L] <= t0 && hb_rng[2L] >= t1
  expected <- as.numeric(difftime(t1, t0, units = "mins")) / interval_min
  hb_w <- hb[timestamp >= t0 & timestamp <= t1, .N, by = .(unit_number, unit = idf)]
  cand[, unit := vapply(unit_number, function(n) { x <- hb_w[unit_number == n]; if (nrow(x)) x$unit[which.max(x$N)] else NA_character_ }, character(1))]
  cand[, completeness := vapply(unit_number, function(n) min(1, sum(hb_w[unit_number == n, N]) / expected), numeric(1))]
  if (!win_ok) cand[, completeness := NA_real_]   # dados de heartbeat nao cobrem a janela: nao se pode confirmar
  out <- cand[!is.na(completeness) & completeness >= min_completeness & !is.na(unit)]
  out <- head(out, k)
  out[, .(unit_number, unit, dist_m, completeness)]
}

## Indice de bloco (1 .. n_blk) de cada instante: blocos de block_hours h alinhados com a falha
wfov_block_index <- function(timestamp, event_time, days_before, days_after, block_hours) {
  nb_b <- as.integer(ceiling(days_before * 24 / block_hours)); nb_a <- as.integer(ceiling(days_after * 24 / block_hours))
  h <- as.numeric(difftime(timestamp, event_time, units = "hours")) / block_hours
  list(blk = pmin(pmax(floor(h) + nb_b + 1L, 1L), nb_b + nb_a), n_blk = nb_b + nb_a, nb_b = nb_b)
}

## Benchmark de UMA falha. wfov_dt: read_track_wfov() com a unidade e as de referencia.
## ref_units: codigos/numeros das unidades de referencia (agrupadas).
## Devolve list(total, by_camera, refs, n_ref):
##   total:     tracks/dia antes e depois da unidade e da referencia agrupada, razoes, variacao
##              relativa da unidade vs referencia (%), p-value de permutacao
##   by_camera: por camara da unidade, razao depois/antes dividida pela razao da referencia
##              (total) -- p-value de permutacao e ajustado (maximo entre cameras)
##   refs:      cada unidade de referencia (tracks antes/depois, razao) -- amplitude "normal"
## Permutacao: os rotulos antes/depois sao reatribuidos a BLOCOS de tempo, os
## MESMOS para a unidade e a referencia (mantem a atividade comum das aves);
## estatistica = log(razao da unidade) - log(razao da referencia). Sem diferenca
## especifica da unidade, a variacao da unidade e' a da referencia.
wfov_benchmark <- function(wfov_dt, unit, ref_units, event_time, days_before = 5, days_after = 5,
                           n_perm = 10000L, block_hours = 6, seed = 1L) {
  if (is.null(block_hours) || is.na(block_hours) || block_hours <= 0) block_hours <- 6
  u <- idf_unit_number(unit); r <- unique(idf_unit_number(ref_units))
  t0 <- event_time - days_before * 86400; t1 <- event_time + days_after * 86400
  x <- data.table::as.data.table(wfov_dt)[timestamp >= t0 & timestamp <= t1]
  x[, period := ifelse(timestamp < event_time, "before", "after")]

  # tracks distintos (instante = 1.o registo) da unidade e de cada referencia
  trk <- x[, .(timestamp = min(timestamp), camera = camera[1L]), by = .(idf, track_id)]
  trk_u <- trk[idf == u]
  trk_r <- trk[idf %in% r]
  cams <- wfov_order_cameras(x[idf == u, camera])
  cam_u <- x[idf == u, .(timestamp = min(timestamp)), by = .(track_id, camera)]

  rate_ratio <- function(a, b) ((a + 0.5) / days_after) / ((b + 0.5) / days_before)
  cnt <- function(dt, per) if (nrow(dt) == 0L) 0L else dt[, sum(if (per == "before") timestamp < event_time else timestamp >= event_time)]

  refs <- data.table::rbindlist(lapply(r, function(n) {
    d <- trk[idf == n]; b <- cnt(d, "before"); a <- cnt(d, "after")
    data.table::data.table(unit_number = n, tracks_before = b, tracks_after = a,
                           per_day_before = b / days_before, per_day_after = a / days_after,
                           ratio = if (b > 0L) (a / days_after) / (b / days_before) else NA_real_)
  }))

  bu <- cnt(trk_u, "before"); au <- cnt(trk_u, "after"); br <- cnt(trk_r, "before"); ar <- cnt(trk_r, "after")
  obs_u <- log(rate_ratio(au, bu)); obs_r <- log(rate_ratio(ar, br))

  # matrizes por bloco
  ix_u <- wfov_block_index(trk_u$timestamp, event_time, days_before, days_after, block_hours)
  ix_r <- wfov_block_index(trk_r$timestamp, event_time, days_before, days_after, block_hours)
  n_blk <- ix_u$n_blk; nb_b <- ix_u$nb_b
  tot_u <- tabulate(ix_u$blk, n_blk); tot_r <- tabulate(ix_r$blk, n_blk)
  nc <- length(cams)
  Bc <- matrix(0L, n_blk, nc)
  if (nc > 0L && nrow(cam_u) > 0L) {
    ix_c <- wfov_block_index(cam_u$timestamp, event_time, days_before, days_after, block_hours)
    ci <- match(cam_u$camera, cams)
    for (k in seq_len(nrow(cam_u))) Bc[ix_c$blk[k], ci[k]] <- Bc[ix_c$blk[k], ci[k]] + 1L
  }
  lr <- function(v, sel) log(rate_ratio(sum(v[!sel]), sum(v[sel])))
  stat_all <- function(sel) {
    c(total = lr(tot_u, sel) - lr(tot_r, sel),
      vapply(seq_len(nc), function(j) lr(Bc[, j], sel) - lr(tot_r, sel), numeric(1)))
  }
  sel_obs <- seq_len(n_blk) <= nb_b
  obs <- stat_all(sel_obs)

  na_p <- function(n) rep(NA_real_, n)
  p_tot <- NA_real_; p_cam <- na_p(nc); p_adj <- na_p(nc)
  if (br > 0L && n_perm >= 1L) {
    if (!is.null(seed)) { old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL; set.seed(seed) }
    perm <- matrix(NA_real_, n_perm, nc + 1L)
    for (i in seq_len(n_perm)) {
      sel <- logical(n_blk); sel[sample.int(n_blk, nb_b)] <- TRUE
      perm[i, ] <- stat_all(sel)
    }
    if (!is.null(seed) && !is.null(old)) assign(".Random.seed", old, envir = globalenv())
    N <- nrow(perm); ap <- abs(perm); ao <- abs(obs)
    p_tot <- (sum(ap[, 1L] >= ao[1L] - 1e-12) + 1) / (N + 1)
    if (nc > 0L) {
      mx <- apply(ap[, -1L, drop = FALSE], 1L, max)
      p_cam <- vapply(seq_len(nc), function(j) (sum(ap[, j + 1L] >= ao[j + 1L] - 1e-12) + 1) / (N + 1), numeric(1))
      p_adj <- vapply(seq_len(nc), function(j) (sum(mx >= ao[j + 1L] - 1e-12) + 1) / (N + 1), numeric(1))
    }
  }

  total <- data.table::data.table(
    unit_before = bu, unit_after = au, ref_before = br, ref_after = ar,
    unit_ratio = if (bu > 0L) (au / days_after) / (bu / days_before) else NA_real_,
    ref_ratio = if (br > 0L) (ar / days_after) / (br / days_before) else NA_real_,
    relative_vs_benchmark_pct = if (br > 0L) 100 * (exp(obs[1L]) - 1) else NA_real_,
    p_perm = p_tot
  )
  by_cam <- data.table::data.table(camera = cams)
  by_cam[, `:=`(n_before = vapply(camera, function(c) cam_u[camera == c, sum(timestamp < event_time)], numeric(1)),
                n_after  = vapply(camera, function(c) cam_u[camera == c, sum(timestamp >= event_time)], numeric(1)))]
  by_cam[, ratio := ifelse(n_before > 0, (n_after / days_after) / (n_before / days_before), NA_real_)]
  by_cam[, relative_vs_benchmark_pct := if (br > 0L) 100 * (exp(obs[-1L]) - 1) else NA_real_]
  by_cam[, `:=`(p_perm = p_cam, p_adj = p_adj)]
  list(total = total[], by_camera = by_cam[], refs = refs[], n_ref = length(r))
}

plot_wfov_benchmark <- function(bm, unit_label, ref_labels, title) {
  if (is.null(bm) || bm$n_ref == 0L) return(NULL)
  d <- rbind(data.frame(unit = ref_labels[as.character(bm$refs$unit_number)], ratio = bm$refs$ratio, grp = "Reference unit"),
             data.frame(unit = "Reference (pooled)", ratio = bm$total$ref_ratio, grp = "Reference (pooled)"),
             data.frame(unit = unit_label, ratio = bm$total$unit_ratio, grp = "Unit with reported malfunction"))
  d <- d[!is.na(d$ratio), ]
  d$unit <- factor(d$unit, levels = rev(unique(d$unit)))
  p <- ggplot2::ggplot(d, ggplot2::aes(ratio, unit, colour = grp)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_colour_manual(values = c("Reference unit" = "#4C72B0", "Reference (pooled)" = "#2B5C8A", "Unit with reported malfunction" = "#C44E52"), name = NULL) +
    ggplot2::labs(title = title, subtitle = "Tracks per day after / before the reported date (1 = no change)", x = "Ratio after / before", y = NULL) +
    ggplot2::theme_minimal(base_size = 9) + ggplot2::theme(legend.position = "bottom")
  list(p = p, h = 1.8 + 0.25 * nrow(d))
}
