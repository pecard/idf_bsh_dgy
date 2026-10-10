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


## 1. Leitura dos tracks de um parque (data-raw + pasta remota) ----
##
## Recebe os parametros diretamente (vindos de inputs/userSettings_regional_assessment.R,
## ver run_regional_assessment.R) -- nao le os settings de incidente/anual/mensal.
## Le TODOS os ficheiros que batem com trackreport_pattern (+ farm_pattern,
## 2a camada de filtro) nas pastas dadas, e guarda a leitura bruta numa cache
## fst PROPRIA do regional (cache_dir/<farm>/track_dt_unfilt.fst) -- mudar
## ini/end nao exige reler; force_reread = TRUE quando ha' dados novos ou o
## padrao mudou. So' depois de carregar aplica a janela ini/end e guarda as
## colunas necessarias (track_id, timestamp, utm_x, utm_y, spec).

regional_filter_window <- function(track_dt, ini, end) {
  track_dt[timestamp >= ini & timestamp <= end]
}

regional_load_farm <- function(farm, databases_dirs, trackreport_pattern, farm_pattern = NULL,
                               tz, ini, end, cache_dir = file.path("cache", "regional"),
                               force_reread = FALSE) {

  message(sprintf("\n===== regional: a ler tracks de %s (padrao '%s') =====", farm, trackreport_pattern))

  dirs <- unique(databases_dirs[!is.na(databases_dirs) & nzchar(databases_dirs)])
  cache_file <- file.path(cache_dir, farm, "track_dt_unfilt.fst")

  tracks <- load_or_read_cache(
    cache_file,
    function() read_tracks_data(dirs, trackreport_pattern, tz = tz, farm_pattern = farm_pattern),
    force_reread = force_reread, tz = tz
  )

  if (is.null(tracks) || nrow(tracks) == 0L) {
    stop(sprintf(
      "regional_load_farm(%s): 0 tracks lidos (pastas: %s; padrao: %s) -- confirmar pastas/padrao e apagar %s se a cache estiver vazia.",
      farm, paste(dirs, collapse = " | "), trackreport_pattern, cache_file
    ))
  }

  tracks <- data.table::as.data.table(tracks)
  range_all <- range(tracks$timestamp)
  tracks <- regional_filter_window(tracks, ini, end)[, .(track_id, timestamp, utm_x, utm_y, spec)]

  if (nrow(tracks) == 0L) {
    stop(sprintf(
      "regional_load_farm(%s): nenhum track dentro de ini/end (%s a %s); os dados lidos vao de %s a %s.",
      farm, format(ini), format(end), format(range_all[1]), format(range_all[2])
    ))
  }

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

  first_reach <- function(days, cum_share, share) {
    hit <- which(cum_share >= share)
    if (length(hit) == 0L) as.Date(NA) else days[hit[1]]
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
        season_coverage = round(cov, 2), season_total = total,
        valid = valid,
        onset  = if (valid) first_reach(dd, cum_share, onset_share)  else as.Date(NA),
        median = if (valid) first_reach(dd, cum_share, median_share) else as.Date(NA),
        end    = if (valid) first_reach(dd, cum_share, end_share)    else as.Date(NA)
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
            leader = p$leader, follower = p$follower, lag_days = lag, r = r, n_overlap = n_ok
          )
        }
      }
    }
  }
  data.table::rbindlist(rows)
}

regional_ccf_summary <- function(ccf_dt) {
  if (nrow(ccf_dt) == 0L) return(ccf_dt)
  ccf_dt[, {
    ok <- !is.na(r)
    if (!any(ok)) {
      .(best_lag_days = NA_integer_, r_best = NA_real_, r_lag0 = NA_real_, n_overlap_lag0 = NA_integer_)
    } else {
      i <- which.max(ifelse(ok, r, -Inf))
      .(best_lag_days = lag_days[i], r_best = round(r[i], 3),
        r_lag0 = round(r[lag_days == 0][1], 3), n_overlap_lag0 = n_overlap[lag_days == 0][1])
    }
  }, by = .(spec, season, season_year, leader, follower)]
}


## 8. Pulsos (episodios de presenca) e seguimento no outro parque ----
##
## Pulso = dias consecutivos (ou separados por <= merge_gap_days) com
## max_individuals >= min_individuals. Para cada pulso do lider na epoca,
## procura o 1o pulso do seguidor que comeca nos window_days SEGUINTES
## (forward) e, como controlo, o mais proximo nos window_days ANTERIORES
## (backward) -- se o lider antecipa, forward deve ser bem mais comum.

regional_pulses <- function(daily_dt, min_individuals = 1L, merge_gap_days = 3L) {
  d <- daily_dt[!is.na(max_individuals) & max_individuals >= min_individuals]
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
    median_lag_days = if (any(!is.na(lag_forward_days))) stats::median(lag_forward_days, na.rm = TRUE) else NA_real_,
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
plot_regional_weekly_timeline <- function(weekly_dt, species) {
  d <- data.table::copy(weekly_dt[spec == species])
  if (nrow(d) == 0L) return(NULL)
  d[, farm := .regional_farm_factor(farm)]
  na_weeks <- d[is.na(max_individuals)]

  ggplot2::ggplot(d, ggplot2::aes(x = week_start, y = max_individuals, fill = farm)) +
    ggplot2::geom_col(data = d[!is.na(max_individuals)], width = 6) +
    ggplot2::geom_point(data = na_weeks, ggplot2::aes(y = 0), shape = 4, colour = "grey55", size = 1.2, na.rm = TRUE) +
    ggplot2::facet_grid(farm ~ ., scales = "free_y") +
    ggplot2::scale_fill_manual(values = regional_farm_colours, guide = "none") +
    ggplot2::scale_y_continuous(breaks = scales::breaks_width(1), expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    ggplot2::labs(
      x = "Week (Monday)", y = "Weekly maximum of minimum individuals (2-min bins)",
      title = sprintf("%s -- weekly maximum, ZRF / BSH / DGY", species),
      caption = "Crosses mark weeks without enough monitored days (no data, not zero)."
    ) +
    .regional_theme() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 7))
}

## 9.2 Sobreposicao anual -- x = semana ISO, cor = parque, linha por ano
plot_regional_annual_overlay <- function(weekly_dt, species, normalise = FALSE) {
  d <- data.table::copy(weekly_dt[spec == species & !is.na(max_individuals)])
  if (nrow(d) == 0L) return(NULL)
  d[, farm := .regional_farm_factor(farm)]
  d[, yr := factor(iso_year)]
  ylab <- "Weekly maximum of minimum individuals (2-min bins)"
  if (normalise) {
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
