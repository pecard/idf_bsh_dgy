##
## Regional assessment -- Zarafshan (ZRF, norte), Bash (BSH, oeste) e
## Djangeldy (DGY, este): 3 parques a ~100 km entre si, em triangulo.
##
## Hipotese de trabalho (Paulo, 2026-10):
##   - Outono (movimentos pos-nupciais para sul): ZRF, a norte, pode informar
##     sobre a chegada de Egyptian-Vulture e Steppe-Eagle a BSH e DGY.
##   - Primavera (movimentos pre-nupciais para norte): BSH e DGY funcionam
##     como early warning da chegada das mesmas especies a ZRF.
##
## Duas abordagens (ver run_regional_assessment.R, que as encadeia da leitura
## aos plots):
##   1. Padrao anual -- maximo SEMANAL do numero minimo de individuos (bins de
##      2 min, R/track_min_individuals.R) das 2 especies, nos 3 parques.
##   2. Antecipacao -- ZRF antecipa BSH/DGY no outono, e BSH/DGY antecipam ZRF
##      na primavera? Tres leituras complementares, todas por epoca e ano:
##        a. timing da passagem (onset / mediana / fim da passagem
##           acumulada) e diferenca entre parques;
##        b. correlacao cruzada defasada das series diarias (lag em dias);
##        c. "pulsos" (episodios de presenca) do parque lider e se o parque
##           seguidor tem um pulso nos N dias seguintes (vs. antes).
##
## Esforco de monitorizacao: os tracks so' existem quando ha' aves, por isso
## dia sem tracks pode ser ausencia genuina OU dia sem monitorizacao. Um dia
## conta como monitorizado se tiver >= min_tracks_per_day tracks (qualquer
## especie); fora disso, a serie fica NA (nao 0) -- semanas com menos de
## min_monitored_days dias monitorizados tambem ficam NA. Fora do intervalo
## [1o dia, ultimo dia] com tracks de cada parque, tudo e' NA.
##
## Todas as datas (as.Date) usam o fuso do parque (tz=), nunca UTC -- em
## Asia/Samarkand (UTC+5) a meia-noite local cairia no dia anterior.
##
## Parametros: inputs/userSettings_regional_assessment.R (leitura, ini/end, especies,
##   bins/separacao entre tracks) e bloco "PARAMETROS DA ANALISE" de run_regional_assessment.R.
##
## Depende de: data.table, lubridate, ggplot2, R/track_min_individuals.R,
##   R/read_utils.R, R/read_tracks.R, R/data_cache.R (so' o carregamento)
##


## 0. Cores/ordem dos parques (norte -> oeste -> este) ----

regional_farm_levels <- c("ZRF", "BSH", "DGY")
regional_farm_colours <- c(ZRF = "#1b9e77", BSH = "#d95f02", DGY = "#7570b3")


## 1. Leitura dos tracks de um parque (data-raw + pasta remota), com cache incremental ----
##
## Recebe os parametros diretamente (vindos de inputs/userSettings_regional_assessment.R,
## ver run_regional_assessment.R) -- nao le os settings de incidente/anual/mensal.
##
## Cache INCREMENTAL por ficheiro (cache_dir/<farm>/tracks_by_file.fst +
## tracks_manifest.rds): cada CSV lido fica registado com tamanho e data de
## modificacao. Numa nova corrida, ficheiros iguais ao registado vem da cache
## (sem tocar nos CSV originais); so' os ficheiros NOVOS ou ALTERADOS sao
## lidos. Ficheiros que deixaram de aparecer (ex: pasta de rede inacessivel)
## mantem as linhas em cache, com aviso -- nunca se perdem dados por uma pasta
## temporariamente em baixo. Se o padrao de ficheiros mudou, a cache e'
## invalidada e relê tudo; force_reread = TRUE tambem relê tudo.
## A cache guarda so' as colunas necessarias (track_id, timestamp, utm_x,
## utm_y, spec, idf) + source_file; regional_cache_schema identifica essa
## estrutura (mudar as colunas guardadas invalida a cache e relê tudo).

.regional_fst_io <- list(
  read  = function(path) fst::read_fst(path, as.data.table = TRUE),
  write = function(x, path) fst::write_fst(x, path)
)

regional_read_tracks_cached <- function(files, tz, cache_dir, pattern_sig, read_file_fn,
                                        force_reread = FALSE, cache_io = .regional_fst_io) {

  data_file     <- file.path(cache_dir, "tracks_by_file.fst")
  manifest_file <- file.path(cache_dir, "tracks_manifest.rds")
  cols_keep <- c("track_id", "timestamp", "utm_x", "utm_y", "spec", "idf")

  info <- file.info(files)
  now <- data.table::data.table(file = basename(files), size = as.numeric(info$size),
                                mtime = floor(as.numeric(info$mtime)))

  manifest <- data.table::data.table(file = character(), size = numeric(), mtime = numeric())
  cached <- NULL
  if (!force_reread && file.exists(data_file) && file.exists(manifest_file)) {
    m <- readRDS(manifest_file)
    if (identical(m$pattern_sig, pattern_sig)) {
      manifest <- m$files
      cached <- cache_io$read(data_file)
    } else {
      message("regional: cache ignorada -- o padrao de ficheiros mudou; a ler tudo de novo.")
    }
  }

  idx <- match(now$file, manifest$file)
  changed <- is.na(idx) | now$size != manifest$size[idx] | now$mtime != manifest$mtime[idx]
  missing_files <- setdiff(manifest$file, now$file)
  if (length(missing_files) > 0L) {
    message(sprintf(
      "regional: %d ficheiro(s) em cache ja' nao aparecem nas pastas (mantidas as linhas em cache): %s",
      length(missing_files), paste(utils::head(missing_files, 5), collapse = ", ")
    ))
  }

  new_rows <- NULL
  if (any(changed)) {
    message(sprintf("regional: a ler %d ficheiro(s) novo(s)/alterado(s) de %d (os restantes %d vem da cache).",
                    sum(changed), nrow(now), sum(!changed)))
    parts <- lapply(files[changed], function(f) {
      d <- read_file_fn(f)
      if (is.null(d) || nrow(d) == 0L) return(NULL)
      d <- data.table::as.data.table(d)[, ..cols_keep]
      d[, source_file := basename(f)]
      d
    })
    new_rows <- data.table::rbindlist(parts, use.names = TRUE)
  } else {
    message(sprintf("regional: os %d ficheiro(s) estao inalterados -- lidos da cache, sem ler os CSV originais.", nrow(now)))
  }

  kept <- if (!is.null(cached)) cached[!source_file %in% now$file[changed]] else NULL
  tracks <- data.table::rbindlist(list(kept, new_rows), use.names = TRUE)
  if (nrow(tracks) > 0L) attr(tracks$timestamp, "tzone") <- tz

  if (any(changed) || is.null(cached) || length(missing_files) > 0L) {
    new_manifest <- data.table::rbindlist(list(now, manifest[file %in% missing_files]))
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    cache_io$write(tracks, data_file)
    saveRDS(list(pattern_sig = pattern_sig, files = new_manifest), manifest_file)
  }

  list(tracks = tracks, n_files = nrow(now), n_read = sum(changed), n_cached = sum(!changed),
       n_missing = length(missing_files))
}


regional_cache_schema <- "v2-idf"

regional_filter_window <- function(track_dt, ini, end) {
  track_dt[timestamp >= ini & timestamp <= end]
}

## Remove linhas duplicadas: ficheiros que se sobrepoem no tempo (mesmo
## periodo exportado 2x, ex: data-raw + pasta de rede com nomes diferentes)
## repetem o mesmo ponto (track_id + timestamp). read_tracks_data() ja' remove
## linhas TOTALMENTE identicas; esta chave apanha tambem as que diferem numa
## coluna qualquer. Mantem a 1a ocorrencia. Duplicados nao inflacionariam os
## individuos (contam-se track_id distintos), mas pesam nas leituras/cache e
## nas contagens de registos. Devolve o data.table sem duplicados, com o
## numero removido em attr(, "n_removed").
regional_dedup_tracks <- function(track_dt) {
  out <- unique(track_dt, by = c("track_id", "timestamp"))
  attr(out, "n_removed") <- nrow(track_dt) - nrow(out)
  out
}

regional_load_farm <- function(farm, databases_dirs, trackreport_pattern, farm_pattern = NULL,
                               tz, ini, end, cache_dir = file.path("cache", "regional"),
                               force_reread = FALSE, read_file_fn = NULL, cache_io = .regional_fst_io) {

  message(sprintf("\n===== regional: a ler tracks de %s (padrao '%s') =====", farm, trackreport_pattern))

  dirs <- unique(databases_dirs[!is.na(databases_dirs) & nzchar(databases_dirs)])
  files <- list_files_multi_dir(dirs, trackreport_pattern, farm_pattern)

  if (length(files) == 0L) {
    stop(sprintf(
      "regional_load_farm(%s): 0 ficheiros encontrados (pastas: %s; padrao: %s; farm_pattern: %s) -- confirmar pastas e padrao.",
      farm, paste(dirs, collapse = " | "), trackreport_pattern, if (is.null(farm_pattern)) "nenhum" else farm_pattern
    ))
  }

  if (is.null(read_file_fn)) read_file_fn <- function(f) read_tracks_data(NULL, tz = tz, files = f)

  res <- regional_read_tracks_cached(
    files, tz, file.path(cache_dir, farm),
    pattern_sig = paste(trackreport_pattern, if (is.null(farm_pattern)) "" else farm_pattern, regional_cache_schema, sep = "|"),
    read_file_fn = read_file_fn, force_reread = force_reread, cache_io = cache_io
  )
  tracks <- res$tracks

  if (nrow(tracks) == 0L) {
    stop(sprintf("regional_load_farm(%s): 0 tracks nos %d ficheiro(s) lidos.", farm, res$n_files))
  }

  range_all <- range(tracks$timestamp)
  tracks <- regional_filter_window(tracks, ini, end)[, .(track_id, timestamp, utm_x, utm_y, spec, idf)]

  if (nrow(tracks) == 0L) {
    stop(sprintf(
      "regional_load_farm(%s): nenhum track dentro de ini/end (%s a %s); os dados lidos vao de %s a %s.",
      farm, format(ini), format(end), format(range_all[1]), format(range_all[2])
    ))
  }

  n_before <- nrow(tracks)
  tracks <- regional_dedup_tracks(tracks)
  message(sprintf(
    "%s: %d linha(s) duplicada(s) (mesmo track_id + timestamp) removida(s) de %d.",
    farm, attr(tracks, "n_removed"), n_before
  ))

  message(sprintf(
    "%s: %d registos na janela, %s a %s", farm, nrow(tracks),
    format(min(tracks$timestamp), "%Y-%m-%d"), format(max(tracks$timestamp), "%Y-%m-%d")
  ))

  list(farm = farm, tz = tz, tracks = tracks)
}


## 2. Dias monitorizados e intervalo de dados de um parque ----

regional_monitored_days <- function(track_dt, tz, min_tracks_per_day = 10L) {
  d <- track_dt[, .(day = as.Date(timestamp, tz = tz), track_id)]
  per_day <- d[, .(n_tracks = data.table::uniqueN(track_id)), by = day]
  list(
    monitored = sort(per_day[n_tracks >= min_tracks_per_day, day]),
    first_day = min(per_day$day), last_day = max(per_day$day)
  )
}


## 3. Bins de 2 min -> minimo de individuos, com cache em disco ----
##
## count_min_individuals_per_bin() e' o passo lento (grafo de proximidade por
## bin). cache_file guarda o resultado com uma assinatura (especies, bin,
## distancia, n linhas e ultimo timestamp dos tracks dessas especies) -- se
## algo mudar, recalcula.

regional_min_individuals_bins <- function(track_dt, species, bin_min = 2, merge_dist_m = 200,
                                          cache_file = NULL, force_recompute = FALSE) {

  sub <- track_dt[spec %in% species]
  sig <- list(
    species = sort(species), bin_min = bin_min, merge_dist_m = merge_dist_m,
    n_rows = nrow(sub), max_ts = if (nrow(sub) > 0L) as.numeric(max(sub$timestamp)) else NA_real_
  )

  if (!is.null(cache_file) && !force_recompute && file.exists(cache_file)) {
    cached <- readRDS(cache_file)
    if (identical(cached$sig, sig)) {
      message(sprintf("regional: bins de '%s' carregados da cache (%d linhas).", cache_file, nrow(cached$bins)))
      return(cached$bins)
    }
    message(sprintf("regional: cache '%s' desatualizada (assinatura mudou) -- a recalcular.", cache_file))
  }

  bins <- count_min_individuals_per_bin(sub, species = species, bin_min = bin_min, merge_dist_m = merge_dist_m)

  if (!is.null(cache_file)) {
    dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(list(sig = sig, bins = bins), cache_file)
  }
  bins
}


## 4. Maximo diario e semanal (com NA onde nao houve monitorizacao) ----
##
## Devolve 1 linha por especie x dia entre o 1o e o ultimo dia com tracks do
## parque. max_individuals = maximo de n_individuals_min nos bins do dia; 0 se
## o dia foi monitorizado mas a especie nao apareceu; NA se o dia nao foi
## monitorizado e a especie tambem nao apareceu (um bin com a especie prova
## que havia monitorizacao, por isso nunca fica NA).

regional_daily_max <- function(bins_dt, species, monitored_info, tz) {

  grid <- data.table::CJ(
    spec = species,
    day = seq(monitored_info$first_day, monitored_info$last_day, by = "day")
  )

  if (nrow(bins_dt) > 0L) {
    obs <- bins_dt[, .(max_obs = max(n_individuals_min)), by = .(spec, day = as.Date(bin_start, tz = tz))]
    grid <- merge(grid, obs, by = c("spec", "day"), all.x = TRUE)
  } else {
    grid[, max_obs := NA_integer_]
  }

  grid[, monitored := day %in% monitored_info$monitored | !is.na(max_obs)]
  grid[, max_individuals := data.table::fifelse(!is.na(max_obs), as.integer(max_obs),
                                                data.table::fifelse(monitored, 0L, NA_integer_))]
  grid[, max_obs := NULL]
  data.table::setorder(grid, spec, day)
  grid[]
}

## Garante que NADA fica fora da janela ini/end (dias). Para um data.table com
## coluna `day` (daily_dt) devolve TRUE ou para com mensagem.
regional_check_window <- function(dt, ini_date, end_date, what = "daily_dt") {
  if (nrow(dt) == 0L) return(invisible(TRUE))
  out <- dt[day < ini_date | day > end_date]
  if (nrow(out) > 0L) {
    stop(sprintf("regional: %s tem %d linha(s) fora da janela %s a %s (ex: %s).", what, nrow(out),
                 format(ini_date), format(end_date), format(out$day[1])))
  }
  invisible(TRUE)
}

## Semana de calendario (segunda-feira), a MESMA nos 3 parques. Semana com
## menos de min_monitored_days dias monitorizados fica NA.

regional_weekly_max <- function(daily_dt, min_monitored_days = 4L) {

  d <- data.table::copy(daily_dt)
  d[, week_start := lubridate::floor_date(day, "week", week_start = 1)]

  out <- d[, {
    n_mon <- sum(monitored)
    .(
      n_days_monitored = n_mon,
      max_individuals = if (n_mon >= min_monitored_days) max(max_individuals, na.rm = TRUE) else NA_integer_
    )
  }, by = .(spec, week_start)]

  out[, `:=`(iso_year = lubridate::isoyear(week_start), iso_week = lubridate::isoweek(week_start))]
  data.table::setorder(out, spec, week_start)
  out[]
}


## 5. Epocas (outono / primavera) por ano ----
##    seasons: lista nomeada, cada elemento c("MM-DD inicio", "MM-DD fim")

regional_season_dates <- function(seasons, years) {
  data.table::rbindlist(lapply(names(seasons), function(s) {
    data.table::data.table(
      season = s, season_year = years,
      from = as.Date(paste0(years, "-", seasons[[s]][1])),
      to   = as.Date(paste0(years, "-", seasons[[s]][2]))
    )
  }))
}

## Suavizacao centrada (media movel, ignora NA); janela 1 = sem suavizar
regional_smooth <- function(x, n) {
  if (n <= 1L) return(as.numeric(x))
  out <- data.table::frollmean(as.numeric(x), n, align = "center", na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}


## 6. Timing da passagem por epoca: onset / mediana / fim (% acumulado) ----
##
## daily_dt precisa de coluna farm. Soma o maximo diario dentro da epoca
## (dias NA excluidos) e devolve o 1o dia em que o acumulado atinge cada
## fracao. Fica NA se a epoca tiver cobertura de monitorizacao
## < min_season_coverage (fracao dos dias da epoca) ou soma < min_total.

regional_passage_timing <- function(daily_dt, season_dates, onset_share = 0.10,
                                    median_share = 0.50, end_share = 0.90,
                                    min_total = 3, min_season_coverage = 0.7) {

  # SEMPRE Date de tipo double (as.Date(NA) pode vir como integer conforme a
  # versao do R, e data.table exige o mesmo tipo em todos os grupos -- erro real
  # "Column 6 ... is type 'integer' but expecting type 'double'", 2026-10)
  as_date_dbl <- function(x) structure(as.numeric(x), class = "Date")
  first_reach <- function(days, cum_share, share) {
    hit <- which(cum_share >= share)
    if (length(hit) == 0L) as_date_dbl(NA) else as_date_dbl(days[hit[1]])
  }

  rows <- lapply(seq_len(nrow(season_dates)), function(i) {
    sd <- season_dates[i]
    n_window <- as.integer(sd$to - sd$from) + 1L
    sub <- daily_dt[day >= sd$from & day <= sd$to]
    sub[, {
      ok <- !is.na(max_individuals)
      cov <- sum(monitored) / n_window
      total <- sum(max_individuals[ok])
      valid <- cov >= min_season_coverage && total >= min_total
      dd <- day[ok]; cs <- cumsum(max_individuals[ok])
      cum_share <- if (total > 0) cs / total else numeric(0)
      .(
        season = sd$season, season_year = sd$season_year,
        season_coverage = round(cov, 2), season_total = as.numeric(total),
        valid = valid,
        onset  = if (valid) first_reach(dd, cum_share, onset_share)  else as_date_dbl(NA),
        median = if (valid) first_reach(dd, cum_share, median_share) else as_date_dbl(NA),
        end    = if (valid) first_reach(dd, cum_share, end_share)    else as_date_dbl(NA)
      )
    }, by = .(farm, spec)]
  })
  data.table::rbindlist(rows)
}

## Diferenca de timing lider -> seguidor. lead_days > 0 = o seguidor chega
## DEPOIS do lider (o lider antecipa, como na hipotese)
regional_lead_table <- function(timing_dt, pairs) {
  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    p <- pairs[i]
    a <- timing_dt[farm == p$leader & season == p$season,
                   .(spec, season, season_year, leader_onset = onset, leader_median = median, leader_end = end)]
    b <- timing_dt[farm == p$follower & season == p$season,
                   .(spec, season, season_year, follower_onset = onset, follower_median = median, follower_end = end)]
    m <- merge(a, b, by = c("spec", "season", "season_year"))
    if (nrow(m) == 0L) return(NULL)
    m[, `:=`(
      leader = p$leader, follower = p$follower,
      lead_days_onset  = as.integer(follower_onset  - leader_onset),
      lead_days_median = as.integer(follower_median - leader_median),
      lead_days_end    = as.integer(follower_end    - leader_end)
    )]
    m
  })
  out <- data.table::rbindlist(rows)
  if (nrow(out) > 0L) data.table::setcolorder(out, c("spec", "season", "season_year", "leader", "follower"))
  out
}


## 7. Correlacao cruzada defasada (series diarias suavizadas) ----
##
## r(lag) = cor(lider[t], seguidor[t + lag]). lag > 0 com o maximo de r =
## o seguidor repete o padrao do lider `lag` dias depois (o lider antecipa).
## Suavizacao feita na serie completa do parque, ANTES de recortar a epoca.

regional_ccf <- function(daily_dt, pairs, season_dates, max_lag_days = 21L,
                         smooth_days = 3L, min_overlap_days = 20L) {

  rows <- list()
  for (i in seq_len(nrow(pairs))) {
    p <- pairs[i]
    for (sp in unique(daily_dt$spec)) {
      lead_full <- daily_dt[farm == p$leader & spec == sp]
      fol_full  <- daily_dt[farm == p$follower & spec == sp]
      if (nrow(lead_full) == 0L || nrow(fol_full) == 0L) next

      all_days <- data.table::data.table(day = seq(
        min(lead_full$day, fol_full$day), max(lead_full$day, fol_full$day), by = "day"))
      ser <- merge(all_days, lead_full[, .(day, x = max_individuals)], by = "day", all.x = TRUE)
      ser <- merge(ser, fol_full[, .(day, y = max_individuals)], by = "day", all.x = TRUE)
      data.table::setorder(ser, day)
      ser[, `:=`(xs = regional_smooth(x, smooth_days), ys = regional_smooth(y, smooth_days))]

      for (k in seq_len(nrow(season_dates[season == p$season]))) {
        sd <- season_dates[season == p$season][k]
        for (lag in seq(-max_lag_days, max_lag_days)) {
          ser[, y_shift := data.table::shift(ys, n = -lag)]
          w <- ser[day >= sd$from & day <= sd$to]
          ok <- !is.na(w$xs) & !is.na(w$y_shift)
          n_ok <- sum(ok)
          r <- if (n_ok >= min_overlap_days && stats::sd(w$xs[ok]) > 0 && stats::sd(w$y_shift[ok]) > 0) {
            stats::cor(w$xs[ok], w$y_shift[ok])
          } else NA_real_
          rows[[length(rows) + 1L]] <- data.table::data.table(
            spec = sp, season = p$season, season_year = sd$season_year,
            leader = p$leader, follower = p$follower, lag_days = as.integer(lag), r = r, n_overlap = as.integer(n_ok)
          )
        }
      }
    }
  }
  data.table::rbindlist(rows)
}

regional_ccf_summary <- function(ccf_dt, perm_dt = NULL, coverage_dt = NULL) {
  if (nrow(ccf_dt) == 0L) return(ccf_dt)
  max_lag <- max(abs(ccf_dt$lag_days))
  out <- ccf_dt[, {
    ok <- !is.na(r)
    if (!any(ok)) {
      .(best_lag_days = NA_integer_, r_best = NA_real_, r_lag0 = NA_real_, n_overlap_lag0 = NA_integer_)
    } else {
      i <- which.max(ifelse(ok, r, -Inf))
      .(best_lag_days = as.integer(lag_days[i]), r_best = round(r[i], 3),
        r_lag0 = round(r[lag_days == 0][1], 3), n_overlap_lag0 = as.integer(n_overlap[lag_days == 0][1]))
    }
  }, by = .(spec, season, season_year, leader, follower)]

  # melhor lag no limite do intervalo testado: a correlacao cresce ate' ao
  # limite (tendencia comum), nao ha' pico -- nao e' um atraso real
  out[, best_lag_at_limit := !is.na(best_lag_days) & abs(best_lag_days) == max_lag]

  keys <- c("spec", "season", "season_year", "leader", "follower")
  out[, `:=`(p_value = NA_real_, n_perm = NA_integer_)]
  if (!is.null(perm_dt) && nrow(perm_dt) > 0L) {
    m <- perm_dt[, c(keys, "p_value", "n_perm"), with = FALSE]
    out[m, on = keys, `:=`(p_value = i.p_value, n_perm = i.n_perm)]
  }
  out[, `:=`(coverage_both = NA_real_, n_both_days = NA_integer_, n_season_days = NA_integer_)]
  if (!is.null(coverage_dt) && nrow(coverage_dt) > 0L) {
    m <- coverage_dt[, c("season", "season_year", "leader", "follower", "coverage_both", "n_both_days", "n_season_days"), with = FALSE]
    out[m, on = c("season", "season_year", "leader", "follower"),
        `:=`(coverage_both = i.coverage_both, n_both_days = i.n_both_days, n_season_days = i.n_season_days)]
  }
  out[]
}

## Teste de permutacao do pico de correlacao ----
##
## Estatistica = maximo de r(lag) sobre TODOS os lags testados (+-max_lag_days),
## a mesma que a tabela mostra como r_best -- a escolha do melhor entre os 2*L+1
## lags entra no nulo, por isso nao e' inflacionada pela selecao.
## Nulo: a serie do seguidor (suavizada, com o contexto de +-max_lag_days dias
## a volta da epoca) e' ROTACIONADA circularmente por um numero de dias maior
## que max_lag_days -- mantem a autocorrelacao e a tendencia da serie, mas
## destroi o alinhamento com o lider. Rotacoes de ate' max_lag_days dias ficam
## fora (coincidiriam com os proprios lags testados). p = (1 + n_nulo >= obs) /
## (1 + n_nulo validos); minimo possivel = 1 / (1 + n_perm).
## NOTA: testa se o pico de correlacao e' maior do que o esperado ao acaso
## (associacao/sincronia), NAO por si so' se o lider ANTECIPA; a antecipacao le-se
## no lag do pico (positivo) quando o teste e' significativo e o pico nao esta'
## no limite.

.regional_ccf_curve <- function(x_win, y_ext, L, min_overlap) {
  n <- length(x_win)
  vapply(seq.int(-L, L), function(lag) {
    y_al <- y_ext[(L + 1L + lag):(L + n + lag)] # y no dia t + lag, para t = 1..n
    ok <- !is.na(x_win) & !is.na(y_al)
    if (sum(ok) < min_overlap) return(NA_real_)
    xs <- x_win[ok]; ys <- y_al[ok]
    if (stats::sd(xs) == 0 || stats::sd(ys) == 0) return(NA_real_)
    stats::cor(xs, ys)
  }, numeric(1))
}

regional_ccf_permutation <- function(daily_dt, pairs, season_dates, max_lag_days = 21L,
                                     smooth_days = 3L, min_overlap_days = 20L, n_perm = 999L, seed = 1) {
  L <- as.integer(max_lag_days)
  set.seed(seed)
  rows <- list()
  for (i in seq_len(nrow(pairs))) {
    p <- pairs[i]
    sd_season <- season_dates[season_dates$season == p$season]
    for (sp in unique(daily_dt$spec)) {
      lead_full <- daily_dt[farm == p$leader & spec == sp]
      fol_full  <- daily_dt[farm == p$follower & spec == sp]
      if (nrow(lead_full) == 0L || nrow(fol_full) == 0L) next
      xs_all <- stats::setNames(regional_smooth(lead_full$max_individuals, smooth_days), as.character(lead_full$day))
      ys_all <- stats::setNames(regional_smooth(fol_full$max_individuals, smooth_days), as.character(fol_full$day))

      for (k in seq_len(nrow(sd_season))) {
        win <- seq(sd_season$from[k], sd_season$to[k], by = "day")
        ext <- seq(sd_season$from[k] - L, sd_season$to[k] + L, by = "day")
        x_win <- unname(xs_all[as.character(win)])
        y_ext <- unname(ys_all[as.character(ext)])
        n <- length(win); m <- length(ext)

        curve_obs <- .regional_ccf_curve(x_win, y_ext, L, min_overlap_days)
        stat_obs <- if (all(is.na(curve_obs))) NA_real_ else max(curve_obs, na.rm = TRUE)

        p_value <- NA_real_; n_valid <- 0L
        if (!is.na(stat_obs) && m - L - 1L >= L + 1L) {
          shifts <- seq.int(L + 1L, m - L - 1L)
          if (length(shifts) > n_perm) shifts <- sample(shifts, n_perm)
          null <- vapply(shifts, function(s) {
            cv <- .regional_ccf_curve(x_win, y_ext[c((s + 1L):m, seq_len(s))], L, min_overlap_days)
            if (all(is.na(cv))) NA_real_ else max(cv, na.rm = TRUE)
          }, numeric(1))
          n_valid <- sum(!is.na(null))
          if (n_valid > 0L) p_value <- (1 + sum(null >= stat_obs, na.rm = TRUE)) / (1 + n_valid)
        }

        rows[[length(rows) + 1L]] <- data.table::data.table(
          spec = sp, season = p$season, season_year = sd_season$season_year[k],
          leader = p$leader, follower = p$follower,
          stat_obs = stat_obs, p_value = p_value, n_perm = as.integer(n_valid)
        )
      }
    }
  }
  data.table::rbindlist(rows)
}

## Cobertura da epoca por par: fracao dos dias da epoca (inteira, nao so' a parte
## dentro da janela) em que OS DOIS parques foram monitorizados. Epoca parcial =
## cobertura < min_season_coverage.
regional_season_coverage <- function(daily_dt, pairs, season_dates) {
  mon <- daily_dt[, .(monitored = any(monitored)), by = .(farm, day)]
  rows <- list()
  for (i in seq_len(nrow(pairs))) {
    p <- pairs[i]
    sd_season <- season_dates[season_dates$season == p$season]
    ml <- mon$day[mon$farm == p$leader & mon$monitored]
    mf <- mon$day[mon$farm == p$follower & mon$monitored]
    for (k in seq_len(nrow(sd_season))) {
      days <- seq(sd_season$from[k], sd_season$to[k], by = "day")
      n_both <- sum(days %in% ml & days %in% mf)
      rows[[length(rows) + 1L]] <- data.table::data.table(
        season = p$season, season_year = sd_season$season_year[k], leader = p$leader, follower = p$follower,
        n_both_days = as.integer(n_both), n_season_days = length(days), coverage_both = n_both / length(days)
      )
    }
  }
  data.table::rbindlist(rows)
}


## 8. Pulsos (episodios de presenca) e seguimento no outro parque ----
##
## Pulso = dias consecutivos (ou separados por <= merge_gap_days) com
## max_individuals >= min_individuals (e, se relative_share, >= essa fracao do
## pico do parque na epoca). Para cada pulso do lider na epoca,
## procura o 1o pulso do seguidor que comeca nos window_days SEGUINTES
## (forward) e, como controlo, o mais proximo nos window_days ANTERIORES
## (backward) -- se o lider antecipa, forward deve ser bem mais comum.

regional_pulses <- function(daily_dt, min_individuals = 1L, merge_gap_days = 3L,
                            relative_share = NULL, season_dates = NULL) {

  d <- data.table::copy(daily_dt[!is.na(max_individuals)])
  d[, thr := as.numeric(min_individuals)]

  # Limiar RELATIVO ao parque (comparavel entre parques com 49, 22 ou 4 unidades):
  # um dia e' de pulso se o maximo diario >= relative_share x pico da epoca
  # (farm x especie x epoca/ano); dias fora das epocas usam o pico de toda a
  # janela. Nunca abaixo de min_individuals.
  if (!is.null(relative_share)) {
    d[, peak_ref := max(max_individuals), by = .(farm, spec)]
    if (!is.null(season_dates)) {
      for (i in seq_len(nrow(season_dates))) {
        from_i <- season_dates$from[i]; to_i <- season_dates$to[i]
        in_i <- d$day >= from_i & d$day <= to_i
        if (any(in_i)) {
          pk <- d[in_i, .(pk = max(max_individuals)), by = .(farm, spec)]
          d[in_i, peak_ref := pk$pk[match(paste(farm, spec), paste(pk$farm, pk$spec))]]
        }
      }
    }
    d[, thr := pmax(as.numeric(min_individuals), relative_share * peak_ref)]
  }

  d <- d[max_individuals >= thr]
  if (nrow(d) == 0L) {
    return(data.table::data.table(farm = character(), spec = character(), start = as.Date(character()),
                                  end = as.Date(character()), peak = integer()))
  }
  data.table::setorder(d, farm, spec, day)
  d[, grp := cumsum(c(TRUE, diff(as.integer(day)) > merge_gap_days + 1L)), by = .(farm, spec)]
  d[, .(start = min(day), end = max(day), peak = max(max_individuals)), by = .(farm, spec, grp)][, grp := NULL][]
}

regional_match_pulses <- function(pulses, pairs, season_dates, window_days = 10L) {
  rows <- list()
  for (i in seq_len(nrow(pairs))) {
    p <- pairs[i]
    for (k in seq_len(nrow(season_dates[season == p$season]))) {
      sd <- season_dates[season == p$season][k]
      for (sp in unique(pulses$spec)) {
        lp <- pulses[farm == p$leader & spec == sp & start >= sd$from & start <= sd$to]
        fp <- pulses[farm == p$follower & spec == sp]
        if (nrow(lp) == 0L) next
        for (j in seq_len(nrow(lp))) {
          s <- lp$start[j]
          fwd <- fp[start >= s & start <= s + window_days]
          bwd <- fp[start < s & start >= s - window_days]
          rows[[length(rows) + 1L]] <- data.table::data.table(
            spec = sp, season = p$season, season_year = sd$season_year,
            leader = p$leader, follower = p$follower,
            leader_pulse_start = s, leader_pulse_peak = lp$peak[j],
            lag_forward_days = if (nrow(fwd) > 0L) as.integer(min(fwd$start) - s) else NA_integer_,
            follower_pulse_before = nrow(bwd) > 0L
          )
        }
      }
    }
  }
  data.table::rbindlist(rows)
}

regional_pulse_summary <- function(match_dt) {
  if (nrow(match_dt) == 0L) return(match_dt)
  match_dt[, .(
    n_leader_pulses = .N,
    n_followed_within_window = sum(!is.na(lag_forward_days)),
    pct_followed = round(100 * mean(!is.na(lag_forward_days)), 1),
    # as.numeric: median() de inteiros devolve inteiro (n impar) ou double (n par) -- tipos mistos entre grupos partem o data.table
    median_lag_days = if (any(!is.na(lag_forward_days))) as.numeric(stats::median(lag_forward_days, na.rm = TRUE)) else NA_real_,
    n_follower_pulse_before = sum(follower_pulse_before),
    pct_follower_pulse_before = round(100 * mean(follower_pulse_before), 1)
  ), by = .(spec, season, season_year, leader, follower)]
}


## 9. Plots ----

.regional_theme <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank())
}

.regional_farm_factor <- function(x) factor(x, levels = regional_farm_levels)

## 9.1 Serie semanal cronologica -- 1 painel por parque (eixo x comum)
plot_regional_weekly_timeline <- function(weekly_dt, species, window = NULL) {
  d <- data.table::copy(weekly_dt[spec == species])
  if (nrow(d) == 0L) return(NULL)
  d[, farm := .regional_farm_factor(farm)]
  d[, week_mid := week_start + 3] # barra a meio da semana (segunda a domingo)
  na_weeks <- d[is.na(max_individuals)]

  cap <- "Grey bands: weeks with no data or too few monitored days (no data, not zero)."
  if (!is.null(window)) cap <- sprintf("Analysis window: %s to %s.\n%s", format(window[1]), format(window[2]), cap)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = week_mid, y = max_individuals, fill = farm)) +
    ggplot2::geom_rect(
      data = na_weeks,
      ggplot2::aes(xmin = week_mid - 3.5, xmax = week_mid + 3.5, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE, fill = "grey88"
    ) +
    ggplot2::geom_col(data = d[!is.na(max_individuals)], width = 6) +
    ggplot2::facet_grid(farm ~ ., scales = "free_y") +
    ggplot2::scale_fill_manual(values = regional_farm_colours, guide = "none") +
    ggplot2::scale_y_continuous(breaks = scales::breaks_width(1), expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    ggplot2::labs(
      x = "Week (Monday to Sunday)", y = "Weekly maximum of minimum individuals (2-min bins)",
      title = sprintf("%s -- weekly maximum, ZRF / BSH / DGY", species),
      caption = cap
    ) +
    .regional_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7))

  # eixo x fixo na janela (todos os paineis), sem folga
  if (!is.null(window)) p <- p + ggplot2::coord_cartesian(xlim = window, expand = FALSE)
  p
}

## 9.2 Sobreposicao anual -- x = semana ISO, cor = parque, linha por ano
plot_regional_annual_overlay <- function(weekly_dt, species, normalise = FALSE) {
  d <- data.table::copy(weekly_dt[spec == species & !is.na(max_individuals)])
  if (nrow(d) == 0L) return(NULL)
  d[, farm := .regional_farm_factor(farm)]
  d[, yr := factor(iso_year)]
  ylab <- "Weekly maximum of minimum individuals (2-min bins)"
  if (normalise) {
    # as.numeric ANTES: dividir e gravar um decimal numa coluna inteira trunca para 0/1
    # nas versoes recentes do data.table (aviso real, 2026-10)
    d[, max_individuals := as.numeric(max_individuals)]
    d[, max_individuals := max_individuals / max(max_individuals), by = farm]
    ylab <- "Weekly maximum, relative to the farm's own maximum"
  }

  ggplot2::ggplot(d, ggplot2::aes(x = iso_week, y = max_individuals, colour = farm, linetype = yr,
                                  group = interaction(farm, yr))) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 1.1) +
    ggplot2::scale_colour_manual(values = regional_farm_colours, name = "Farm") +
    ggplot2::scale_linetype_discrete(name = "Year") +
    ggplot2::scale_x_continuous(breaks = seq(1, 53, by = 4), limits = c(1, 53)) +
    ggplot2::labs(
      x = "ISO week of year", y = ylab,
      title = sprintf("%s -- annual pattern by farm%s", species, if (normalise) " (relative)" else "")
    ) +
    .regional_theme()
}

## 9.3 Series diarias suavizadas por epoca/ano -- ver a antecipacao a olho
plot_regional_season_series <- function(daily_dt, season_dates, species, smooth_days = 3L) {
  rows <- lapply(seq_len(nrow(season_dates)), function(i) {
    sd <- season_dates[i]
    d <- daily_dt[spec == species]
    d[, smooth := regional_smooth(max_individuals, smooth_days), by = farm]
    w <- d[day >= sd$from & day <= sd$to]
    if (nrow(w) == 0L) return(NULL)
    w[, panel := sprintf("%s %d", sd$season, sd$season_year)]
    w
  })
  d <- data.table::rbindlist(rows)
  if (nrow(d) == 0L) return(NULL)
  d[, farm := .regional_farm_factor(farm)]

  ggplot2::ggplot(d, ggplot2::aes(x = day, y = smooth, colour = farm)) +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::facet_wrap(~ panel, scales = "free_x", ncol = 1) +
    ggplot2::scale_colour_manual(values = regional_farm_colours, name = "Farm") +
    ggplot2::scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
    ggplot2::labs(
      x = NULL, y = sprintf("Daily maximum (%d-day centred mean)", smooth_days),
      title = sprintf("%s -- seasonal series by farm", species)
    ) +
    .regional_theme()
}

## 9.4 Passagem acumulada normalizada por epoca/ano
plot_regional_cumulative <- function(daily_dt, season_dates, species,
                                     onset_share = 0.10, median_share = 0.50, end_share = 0.90) {
  rows <- lapply(seq_len(nrow(season_dates)), function(i) {
    sd <- season_dates[i]
    w <- daily_dt[spec == species & day >= sd$from & day <= sd$to & !is.na(max_individuals)]
    if (nrow(w) == 0L) return(NULL)
    data.table::setorder(w, farm, day)
    w[, cum_share := cumsum(max_individuals) / sum(max_individuals), by = farm]
    w <- w[is.finite(cum_share)]
    w[, panel := sprintf("%s %d", sd$season, sd$season_year)]
    w
  })
  d <- data.table::rbindlist(rows)
  if (nrow(d) == 0L) return(NULL)
  d[, farm := .regional_farm_factor(farm)]

  ggplot2::ggplot(d, ggplot2::aes(x = day, y = cum_share, colour = farm)) +
    ggplot2::geom_hline(yintercept = c(onset_share, median_share, end_share), colour = "grey80", linewidth = 0.3) +
    ggplot2::geom_step(linewidth = 0.8) +
    ggplot2::facet_wrap(~ panel, scales = "free_x", ncol = 1) +
    ggplot2::scale_colour_manual(values = regional_farm_colours, name = "Farm") +
    ggplot2::scale_y_continuous(labels = scales::percent, breaks = c(0, onset_share, median_share, end_share, 1)) +
    ggplot2::scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
    ggplot2::labs(
      x = NULL, y = "Cumulative share of the season's daily maxima",
      title = sprintf("%s -- cumulative passage by farm", species)
    ) +
    .regional_theme()
}

## 9.5 Correlacao cruzada por lag
plot_regional_ccf <- function(ccf_dt, species) {
  d <- ccf_dt[spec == species & !is.na(r)]
  if (nrow(d) == 0L) return(NULL)
  d[, pair := sprintf("%s -> %s", leader, follower)]
  d[, panel := sprintf("%s %d", season, season_year)]

  ggplot2::ggplot(d, ggplot2::aes(x = lag_days, y = r, colour = pair)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70") +
    ggplot2::geom_vline(xintercept = 0, colour = "grey70", linetype = "dashed") +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(~ panel, ncol = 1) +
    ggplot2::scale_colour_brewer(palette = "Dark2", name = "Leader -> follower") +
    ggplot2::labs(
      x = "Lag (days): follower at t + lag vs. leader at t", y = "Correlation (r)",
      title = sprintf("%s -- lagged correlation", species),
      caption = "Peak at positive lag = follower repeats the leader's pattern that many days later."
    ) +
    .regional_theme()
}

## 9.6 Diferenca de timing (dias) por metrica
plot_regional_lead_days <- function(lead_dt, species) {
  d <- lead_dt[spec == species]
  if (nrow(d) == 0L) return(NULL)
  long <- data.table::melt(
    d, id.vars = c("spec", "season", "season_year", "leader", "follower"),
    measure.vars = c("lead_days_onset", "lead_days_median", "lead_days_end"),
    variable.name = "metric", value.name = "lead_days"
  )
  long <- long[!is.na(lead_days)]
  if (nrow(long) == 0L) return(NULL)
  long[, metric := factor(sub("lead_days_", "", metric), levels = c("onset", "median", "end"))]
  long[, pair := sprintf("%s -> %s", leader, follower)]
  long[, panel := sprintf("%s %d", season, season_year)]

  ggplot2::ggplot(long, ggplot2::aes(x = lead_days, y = metric, colour = pair)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey60", linetype = "dashed") +
    ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::facet_wrap(~ panel, ncol = 1) +
    ggplot2::scale_colour_brewer(palette = "Dark2", name = "Leader -> follower") +
    ggplot2::labs(
      x = "Days between leader and follower (positive = follower later, leader anticipates)",
      y = "Passage metric", title = sprintf("%s -- timing difference between farms", species)
    ) +
    .regional_theme()
}


## 10. Tabelas e textos para o relatorio Word (report/regional_assessment_report_template.rmd) ----
##
## O template so' formata -- tudo o que e' resumo/formatacao fica aqui, onde
## pode ser testado. Epocas/pares sem dados suficientes aparecem como
## "insufficient data" (nao ficam em falta, nem como 0).

regional_insufficient <- "insufficient data"

## Resumo do pico semanal, por especie e parque
regional_weekly_peak_summary <- function(weekly_dt) {
  out <- weekly_dt[!is.na(max_individuals), {
    pk <- max(max_individuals)
    .(peak_weekly_max = pk, peak_week_start = as.character(week_start[which(max_individuals == pk)[1]]),
      weeks_with_data = .N, weeks_present = sum(max_individuals > 0))
  }, by = .(spec, farm)]
  out[, farm := factor(farm, levels = regional_farm_levels)]
  data.table::setorder(out, spec, farm)
  out[, farm := as.character(farm)]
  out[]
}

## Grelha esperada: 1 linha por especie x ano x par (na ordem dos pares),
## para a epoca pedida -- assim as epocas sem dados aparecem como linhas
.regional_expected_rows <- function(pairs, season_dates, species, season) {
  # season_target: dentro de [.data.table "season" seria a coluna, nao o argumento
  season_target <- season
  p <- pairs[pairs$season == season_target]
  sd <- season_dates[season_dates$season == season_target]
  if (nrow(p) == 0L || nrow(sd) == 0L) return(data.table::data.table())
  g <- data.table::CJ(spec = species, season_year = sd$season_year, pair_i = seq_len(nrow(p)))
  g[, `:=`(leader = p$leader[pair_i], follower = p$follower[pair_i])]
  g[, pair := sprintf("%s -> %s", leader, follower)]
  data.table::setorder(g, spec, season_year, pair_i)
  g
}

.regional_txt <- function(x, digits = NULL) {
  if (is.null(digits)) {
    ifelse(is.na(x), regional_insufficient, as.character(x))
  } else {
    ifelse(is.na(x), regional_insufficient, format(round(x, digits), nsmall = digits))
  }
}

## Cobertura da epoca ("61%" ou "50% (partial)") e p-valor ("0.012", "<= 0.001")
.regional_cov_text <- function(cov, min_cov) {
  ifelse(is.na(cov), regional_insufficient,
         sprintf("%d%%%s", as.integer(round(100 * cov)), ifelse(cov < min_cov, " (partial)", "")))
}
.regional_p_text <- function(p, n_perm) {
  ifelse(is.na(p), regional_insufficient,
         ifelse(p <= 1 / (n_perm + 1) + 1e-12, sprintf("<= %.3f", p), sprintf("%.3f", p)))
}
.regional_add_coverage <- function(m, coverage_dt) {
  if (is.null(coverage_dt)) return(m)
  merge(m, coverage_dt[, .(season_year, leader, follower, coverage_both)],
        by = c("season_year", "leader", "follower"), all.x = TRUE, sort = FALSE)
}

regional_format_timing_table <- function(lead_dt, pairs, season_dates, species, season,
                                         coverage_dt = NULL, min_season_coverage = 0.7) {
  g <- .regional_expected_rows(pairs, season_dates, species, season)
  if (nrow(g) == 0L) return(data.frame())
  m <- merge(g, lead_dt, by = c("spec", "season_year", "leader", "follower"), all.x = TRUE, sort = FALSE)
  m <- .regional_add_coverage(m, coverage_dt)
  data.table::setorder(m, spec, season_year, pair_i)
  out <- data.frame(
    Species = m$spec, Year = as.character(m$season_year), Pair = m$pair,
    `Leader median date` = .regional_txt(m$leader_median),
    `Follower median date` = .regional_txt(m$follower_median),
    `Lead, onset (d)` = .regional_txt(m$lead_days_onset),
    `Lead, median (d)` = .regional_txt(m$lead_days_median),
    `Lead, end (d)` = .regional_txt(m$lead_days_end),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  if (!is.null(coverage_dt)) out$`Season covered` <- .regional_cov_text(m$coverage_both, min_season_coverage)
  out
}

regional_format_ccf_table <- function(ccf_summary_dt, pairs, season_dates, species, season,
                                      min_season_coverage = 0.7) {
  g <- .regional_expected_rows(pairs, season_dates, species, season)
  if (nrow(g) == 0L) return(data.frame())
  m <- merge(g, ccf_summary_dt, by = c("spec", "season_year", "leader", "follower"), all.x = TRUE, sort = FALSE)
  data.table::setorder(m, spec, season_year, pair_i)
  # melhor lag no limite do intervalo = sem pico (nao e' um atraso)
  lag_txt <- ifelse(is.na(m$best_lag_days), regional_insufficient,
                    ifelse(m$best_lag_at_limit %in% TRUE,
                           sprintf("no peak (at %+d d limit)", m$best_lag_days), as.character(m$best_lag_days)))
  data.frame(
    Species = m$spec, Year = as.character(m$season_year), Pair = m$pair,
    `Best lag (d)` = lag_txt,
    `r at best lag` = .regional_txt(m$r_best, 2),
    `r at lag 0` = .regional_txt(m$r_lag0, 2),
    `p value` = .regional_p_text(m$p_value, m$n_perm),
    `Overlap days (lag 0)` = .regional_txt(m$n_overlap_lag0),
    `Season covered` = .regional_cov_text(m$coverage_both, min_season_coverage),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

## timing_dt serve para distinguir "sem pulsos" de "sem dados": se a epoca do
## lider (ou do seguidor) nao tem timing valido, a linha e' "insufficient data"
regional_format_pulse_table <- function(pulse_summary_dt, timing_dt, pairs, season_dates, species, season,
                                        coverage_dt = NULL, min_season_coverage = 0.7) {
  g <- .regional_expected_rows(pairs, season_dates, species, season)
  if (nrow(g) == 0L) return(data.frame())
  m <- merge(g, pulse_summary_dt, by = c("spec", "season_year", "leader", "follower"), all.x = TRUE, sort = FALSE)
  m <- .regional_add_coverage(m, coverage_dt)
  data.table::setorder(m, spec, season_year, pair_i)
  # indexa o vetor timing_dt$valid (nao timing_dt[...]) -- dentro de [.data.table
  # "farm"/"spec"/"season" seriam as colunas, nao os argumentos
  season_target <- season
  valid_of <- function(farm_q, spec_q, year_q) {
    v <- timing_dt$valid[timing_dt$farm == farm_q & timing_dt$spec == spec_q &
                           timing_dt$season == season_target & timing_dt$season_year == year_q]
    length(v) == 1L && isTRUE(v)
  }
  has_data <- mapply(function(l, f, s, y) valid_of(l, s, y) && valid_of(f, s, y),
                     m$leader, m$follower, m$spec, m$season_year)
  none <- is.na(m$n_leader_pulses)
  txt_int <- function(x) ifelse(!has_data, regional_insufficient, ifelse(none, "0", as.character(x)))
  txt_dash <- function(x) ifelse(!has_data, regional_insufficient, ifelse(none | is.na(x), "-", as.character(x)))
  out <- data.frame(
    Species = m$spec, Year = as.character(m$season_year), Pair = m$pair,
    `Leader pulses` = txt_int(m$n_leader_pulses),
    `Followed within window` = txt_int(m$n_followed_within_window),
    `% followed` = txt_dash(m$pct_followed),
    `Median lag (d)` = txt_dash(m$median_lag_days),
    `% with follower pulse before` = txt_dash(m$pct_follower_pulse_before),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  if (!is.null(coverage_dt)) out$`Season covered` <- .regional_cov_text(m$coverage_both, min_season_coverage)
  out
}

## "Autumn: does ZRF anticipate BSH and DGY?" / "Spring: do BSH and DGY anticipate ZRF?"
regional_season_title <- function(pairs, season, label) {
  season_target <- season # dentro de [.data.table "season" seria a coluna, nao o argumento
  p <- pairs[pairs$season == season_target]
  if (nrow(p) == 0L) return(label)
  join <- function(x) paste(unique(x), collapse = " and ")
  leaders <- unique(p$leader)
  sprintf("%s: %s %s anticipate %s?", label, if (length(leaders) == 1L) "does" else "do", join(p$leader), join(p$follower))
}

## Altura da figura (polegadas) por numero de paineis (epoca x ano)
regional_fig_height <- function(p, per_panel = 2.3, base = 1.3, max_h = 8.5) {
  if (is.null(p)) return(1)
  n <- length(unique(p$data$panel))
  min(max_h, base + per_panel * max(n, 1L))
}



## 11. Esforco de monitorizacao: unidades IDF ativas e area vigiada ----
##
## Os tracks so' dizem que unidades IDF registaram aves (track$idf = numero da
## unidade). Uma unidade conta como ATIVA numa semana se registou pelo menos 1
## track nessa semana (qualquer especie). O numero de unidades instaladas
## (ex: 49 / 22 / 4) nao e' usado -- as ativas variam (avarias, unidades novas).

## Numero da unidade: ultimo grupo de digitos ("BSH68-01" -> 1, "DZH01-01" -> 1,
## "IDF-22" -> 22, "22" -> 22); NA se nao houver digitos
regional_unit_number <- function(x) {
  suppressWarnings(as.integer(sub(".*?([0-9]+)$", "\\1", as.character(x))))
}

## Pares (semana, unidade) com pelo menos 1 track
regional_weekly_active_units <- function(track_dt, tz) {
  d <- track_dt[!is.na(idf), .(day = as.Date(timestamp, tz = tz), unit = regional_unit_number(idf))]
  d <- d[!is.na(unit)]
  d[, week_start := lubridate::floor_date(day, "week", week_start = 1)]
  unique(d[, .(week_start, unit)])
}

## Unidades IDF de um parque a partir do shapefile (pontos, em metros apos
## reprojetar). Codigos sem numero (ou repetidos, ex: 2 registos "DZH-23" de
## cobertura planeada) ficam so' com a 1a ocorrencia.
regional_load_idf_units <- function(shapefile, id_col, crs) {
  x <- sf::st_transform(sf::read_sf(shapefile), crs)
  out <- sf::st_sf(unit = regional_unit_number(x[[id_col]]), geometry = sf::st_geometry(x))
  out <- out[!is.na(out$unit) & !duplicated(out$unit), ]
  out
}

## Area vigiada por semana = uniao dos circulos de raio radius_m das unidades
## ativas (com coordenadas) nessa semana, em km2. Semanas com o mesmo conjunto
## de unidades so' calculam a uniao uma vez.
## Devolve: week_start, n_active_units (todas as ativas), n_units_with_coords,
## area_km2 (NA se nenhuma ativa tem coordenadas).
regional_surveyed_area <- function(weekly_units, idf_units_sf, radius_m) {
  if (nrow(weekly_units) == 0L) {
    return(data.table::data.table(week_start = as.Date(character()), n_active_units = integer(),
                                  n_units_with_coords = integer(), area_km2 = numeric()))
  }
  buf <- sf::st_buffer(sf::st_geometry(idf_units_sf), radius_m)
  unit_ids <- idf_units_sf$unit

  sets <- weekly_units[, .(unit_set = list(sort(unit))), by = week_start]
  sets[, key := vapply(unit_set, paste, "", collapse = ",")]
  keys <- unique(sets$key)
  area_by_key <- vapply(keys, function(k) {
    u <- sets$unit_set[[match(k, sets$key)]]
    idx <- which(unit_ids %in% u)
    if (length(idx) == 0L) return(NA_real_)
    as.numeric(sum(sf::st_area(sf::st_union(buf[idx])))) / 1e6
  }, numeric(1))

  sets[, `:=`(
    n_active_units = lengths(unit_set),
    n_units_with_coords = vapply(unit_set, function(u) sum(u %in% unit_ids), integer(1)),
    area_km2 = unname(area_by_key[key])
  )]
  data.table::setorder(sets, week_start)
  sets[, .(week_start, n_active_units, n_units_with_coords, area_km2)]
}

## Unidades ativas sem coordenadas no shapefile (nao entram na area vigiada)
regional_units_without_coords <- function(weekly_units, idf_units_sf) {
  miss <- weekly_units[!unit %in% idf_units_sf$unit, .(weeks_active = .N), by = unit]
  data.table::setorder(miss, unit)
  miss[]
}


## 12. Indices normalizados pelo esforco ----
##
## weekly_dt e effort_dt precisam de coluna farm. Acrescenta, ao maximo
## semanal: por unidade ativa (max / n_active_units) e densidade (individuos
## por 100 km2 de area vigiada = max / area_km2 * 100). NA sem esforco.

regional_normalise_weekly <- function(weekly_dt, effort_dt) {
  out <- merge(weekly_dt, effort_dt[, .(farm, week_start, n_active_units, n_units_with_coords, area_km2)],
               by = c("farm", "week_start"), all.x = TRUE, sort = FALSE)
  out[, max_per_active_unit := data.table::fifelse(!is.na(n_active_units) & n_active_units > 0L,
                                                    max_individuals / n_active_units, NA_real_)]
  out[, density_per_100km2 := data.table::fifelse(!is.na(area_km2) & area_km2 > 0,
                                                   max_individuals / area_km2 * 100, NA_real_)]
  data.table::setorder(out, farm, spec, week_start)
  out[]
}

## Resumo de esforco por parque (semanas com dados)
regional_effort_summary <- function(effort_dt) {
  effort_dt[, .(
    weeks_with_tracks = .N,
    mean_active_units = round(mean(n_active_units), 1),
    min_active_units = min(n_active_units), max_active_units = max(n_active_units),
    mean_surveyed_area_km2 = round(mean(area_km2, na.rm = TRUE), 1)
  ), by = farm][, farm := factor(farm, levels = regional_farm_levels)][order(farm)][, farm := as.character(farm)][]
}

## Picos brutos e normalizados, por especie e parque
regional_norm_peak_summary <- function(norm_dt) {
  d <- norm_dt[!is.na(max_individuals)]
  out <- d[, .(
    peak_weekly_max = max(max_individuals),
    peak_per_active_unit = round(max(max_per_active_unit, na.rm = TRUE), 3),
    peak_density_per_100km2 = round(max(density_per_100km2, na.rm = TRUE), 3),
    mean_active_units = round(mean(n_active_units, na.rm = TRUE), 1),
    mean_surveyed_area_km2 = round(mean(area_km2, na.rm = TRUE), 1)
  ), by = .(spec, farm)]
  out[, farm := factor(farm, levels = regional_farm_levels)]
  data.table::setorder(out, spec, farm)
  out[, farm := as.character(farm)]
  out[]
}


## 13. Rarefacao: o que veria o parque com so' k unidades ativas? ----
##
## Para cada semana sorteia k das unidades ATIVAS nessa semana (todas, se
## houver k ou menos) e recalcula os bins de 2 min so' com os tracks dessas
## unidades -> maximo semanal. Repete n_draws vezes (seed fixa) e resume a
## mediana e o intervalo p10-p90 por semana. Controla o NUMERO de unidades
## (nao a geometria). Pode demorar: cada sorteio recalcula os bins num
## subconjunto dos tracks. cache_file (rds) guarda o resultado com assinatura.

regional_rarefied_weekly <- function(track_dt, species, weekly_units, k, n_draws, bin_min, merge_dist_m,
                                     tz, monitored_info, min_monitored_days, seed = 1, cache_file = NULL) {

  sp_tracks <- track_dt[spec %in% species & !is.na(idf),
                        .(track_id, timestamp, utm_x, utm_y, spec, unit = regional_unit_number(idf))]
  sp_tracks <- sp_tracks[!is.na(unit)]
  sp_tracks[, week_start := lubridate::floor_date(as.Date(timestamp, tz = tz), "week", week_start = 1)]

  sig <- list(k = k, n_draws = n_draws, seed = seed, species = sort(species), bin_min = bin_min,
              merge_dist_m = merge_dist_m, n_rows = nrow(sp_tracks),
              max_ts = if (nrow(sp_tracks) > 0L) as.numeric(max(sp_tracks$timestamp)) else NA_real_,
              first_day = as.numeric(monitored_info$first_day), last_day = as.numeric(monitored_info$last_day))
  if (!is.null(cache_file) && file.exists(cache_file)) {
    cached <- readRDS(cache_file)
    if (identical(cached$sig, sig)) {
      message(sprintf("regional: rarefacao carregada da cache '%s'.", cache_file))
      return(cached$result)
    }
  }

  set.seed(seed)
  active <- weekly_units[, .(units = list(unit)), by = week_start]
  draws <- vector("list", n_draws)
  for (d in seq_len(n_draws)) {
    sel <- active[, .(unit = {
      u <- units[[1]]
      if (length(u) > k) sample(u, k) else u
    }), by = week_start]
    sub <- sp_tracks[sel, on = .(week_start, unit), nomatch = 0L]
    bins <- count_min_individuals_per_bin(sub, species = species, bin_min = bin_min, merge_dist_m = merge_dist_m)
    daily <- regional_daily_max(bins, species, monitored_info, tz)
    w <- regional_weekly_max(daily, min_monitored_days = min_monitored_days)
    w[, draw := d]
    draws[[d]] <- w
    if (d %% 5L == 0L || d == n_draws) message(sprintf("regional: rarefacao k = %d, sorteio %d de %d.", k, d, n_draws))
  }

  all_w <- data.table::rbindlist(draws)
  result <- all_w[, .(
    rarefied_median = if (any(!is.na(max_individuals))) stats::median(as.numeric(max_individuals), na.rm = TRUE) else NA_real_,
    rarefied_p10 = if (any(!is.na(max_individuals))) as.numeric(stats::quantile(max_individuals, 0.1, na.rm = TRUE)) else NA_real_,
    rarefied_p90 = if (any(!is.na(max_individuals))) as.numeric(stats::quantile(max_individuals, 0.9, na.rm = TRUE)) else NA_real_
  ), by = .(spec, week_start)]
  data.table::setorder(result, spec, week_start)

  if (!is.null(cache_file)) {
    dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
    saveRDS(list(sig = sig, result = result), cache_file)
  }
  result[]
}


## 14. Plots de esforco e normalizacao ----

## Unidades ativas e area vigiada por semana, 3 parques
plot_regional_effort <- function(effort_dt, window = NULL) {
  if (nrow(effort_dt) == 0L) return(NULL)
  e <- data.table::copy(effort_dt)[, .(farm, week_start, n_active_units = as.numeric(n_active_units), area_km2)]
  d <- data.table::melt(
    e, id.vars = c("farm", "week_start"),
    measure.vars = c("n_active_units", "area_km2"), variable.name = "metric", value.name = "value"
  )
  d[, metric := factor(metric, levels = c("n_active_units", "area_km2"),
                       labels = c("Active IDF units", "Surveyed area (km2)"))]
  d[, farm := .regional_farm_factor(farm)]
  d[, week_mid := week_start + 3]

  p <- ggplot2::ggplot(d, ggplot2::aes(x = week_mid, y = value, colour = farm)) +
    ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE) +
    ggplot2::facet_grid(metric ~ ., scales = "free_y") +
    ggplot2::scale_colour_manual(values = regional_farm_colours, name = "Farm") +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    ggplot2::labs(x = "Week (Monday to Sunday)", y = NULL, title = "Monitoring effort by farm",
                  caption = "Active units: at least 1 track in the week. Surveyed area: union of the detection radii of the active units.") +
    .regional_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7))
  if (!is.null(window)) p <- p + ggplot2::coord_cartesian(xlim = window, expand = FALSE)
  p
}

## Maximo semanal bruto, por unidade ativa e por 100 km2, 3 parques sobrepostos
plot_regional_normalised <- function(norm_dt, species, window = NULL) {
  d <- norm_dt[spec == species]
  if (nrow(d) == 0L) return(NULL)
  d <- data.table::melt(
    d[, .(farm, week_start, max_individuals = as.numeric(max_individuals), max_per_active_unit, density_per_100km2)],
    id.vars = c("farm", "week_start"), variable.name = "metric", value.name = "value"
  )
  d[, metric := factor(metric, levels = c("max_individuals", "max_per_active_unit", "density_per_100km2"),
                       labels = c("Weekly maximum (raw)", "Per active IDF unit", "Per 100 km2 surveyed"))]
  d[, farm := .regional_farm_factor(farm)]
  d[, week_mid := week_start + 3]

  p <- ggplot2::ggplot(d, ggplot2::aes(x = week_mid, y = value, colour = farm)) +
    ggplot2::geom_line(linewidth = 0.6, na.rm = TRUE) +
    ggplot2::geom_point(size = 0.8, na.rm = TRUE) +
    ggplot2::facet_grid(metric ~ ., scales = "free_y") +
    ggplot2::scale_colour_manual(values = regional_farm_colours, name = "Farm") +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    ggplot2::labs(x = "Week (Monday to Sunday)", y = NULL,
                  title = sprintf("%s -- effort-normalised weekly maximum", species)) +
    .regional_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7))
  if (!is.null(window)) p <- p + ggplot2::coord_cartesian(xlim = window, expand = FALSE)
  p
}

## Rarefacao: maximo semanal bruto vs mediana (e banda p10-p90) com k unidades
plot_regional_rarefied <- function(rare_dt, weekly_dt, species, k, window = NULL) {
  r <- rare_dt[spec == species]
  if (nrow(r) == 0L) return(NULL)
  w <- weekly_dt[spec == species, .(farm, week_start, raw = as.numeric(max_individuals))]
  r <- merge(r, w, by = c("farm", "week_start"), all.x = TRUE)
  r[, farm := .regional_farm_factor(farm)]
  r[, week_mid := week_start + 3]

  p <- ggplot2::ggplot(r, ggplot2::aes(x = week_mid)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = rarefied_p10, ymax = rarefied_p90, fill = farm), alpha = 0.3, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = rarefied_median, colour = farm), linewidth = 0.7, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = raw), colour = "grey35", linewidth = 0.4, linetype = "dashed", na.rm = TRUE) +
    ggplot2::facet_grid(farm ~ ., scales = "free_y") +
    ggplot2::scale_colour_manual(values = regional_farm_colours, guide = "none") +
    ggplot2::scale_fill_manual(values = regional_farm_colours, guide = "none") +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    ggplot2::labs(
      x = "Week (Monday to Sunday)", y = "Weekly maximum of minimum individuals",
      title = sprintf("%s -- weekly maximum with %d active units per week", species, k),
      caption = "Coloured line and band: median and 10-90% range over random draws of the active units.\nDashed grey: all active units."
    ) +
    .regional_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7))
  if (!is.null(window)) p <- p + ggplot2::coord_cartesian(xlim = window, expand = FALSE)
  p
}
