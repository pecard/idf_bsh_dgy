##
## Contagem SEMANAL de observacoes (tracks) por especie, a partir de track_dt
##
## Mesma logica de weekly_species_presence() / plot_species_presence_heatmap() de
## explore_bsh_dgy_comparison.R (bins de 7 dias ancorados a uma data, 1 tile por
## especie x semana), mas com QUANTIDADES em vez de presenca/ausencia (0/1):
##   - n_tracks:  n.o de tracks distintos (track_id) com o 1.o registo nessa semana
##   - n_records: n.o de registos (linhas de track_dt) nessa semana
##   - days_with_tracks: n.o de dias da semana com pelo menos 1 track (esforco/cobertura)
## A janela e' a do PROPRIO dado (1.o ao ultimo dia com tracks), salvo start/end.
##
## tz TEM de ser passado e usado em todo as.Date(POSIXct): as.Date() sem tz assume UTC e
## desloca a data 1 dia para tras em Asia/Samarkand (UTC+5) -- ver
## explore_bsh_dgy_comparison.R (mesmo cuidado).
##
## Depende de: data.table, ggplot2
##

## track_dt: track_id, timestamp (POSIXct, hora local), spec (especie)
## species_list: especies a mostrar individualmente; "All species" (todas) e' acrescentada
## Devolve list(weekly, window):
##   weekly: species, week_start, week_end, n_tracks, n_records, days_with_tracks, is_partial
##   window: list(start, end) -- datas (Date) do 1.o e do ultimo dia com tracks
weekly_track_counts <- function(track_dt, species_list, tz, start = NULL, end = NULL) {
  dt <- data.table::as.data.table(track_dt)[, .(track_id = as.character(track_id), timestamp, spec)]
  dt[, day := as.Date(timestamp, tz = tz)]
  if (is.null(start)) start <- min(dt$day)
  if (is.null(end)) end <- max(dt$day)
  start <- as.Date(start); end <- as.Date(end)
  dt <- dt[day >= start & day <= end]
  dt[, week_start := start + (as.integer(day - start) %/% 7L) * 7L]

  week_starts <- seq(start, end, by = 7)
  species_all <- c("All species", species_list)
  grid <- data.table::CJ(species = species_all, week_start = week_starts)

  per_species <- function(d, sp) {
    d[, .(n_tracks = data.table::uniqueN(track_id), n_records = .N, days_with_tracks = data.table::uniqueN(day)), by = week_start][, species := sp]
  }
  counts <- data.table::rbindlist(c(
    list(per_species(dt, "All species")),
    lapply(species_list, function(sp) per_species(dt[spec == sp], sp))
  ), use.names = TRUE)

  out <- merge(grid, counts, by = c("species", "week_start"), all.x = TRUE)
  for (col in c("n_tracks", "n_records", "days_with_tracks")) data.table::set(out, which(is.na(out[[col]])), col, 0L)
  out[, week_end := pmin(week_start + 6L, end)]
  out[, is_partial := (week_start + 6L) > end]
  data.table::setcolorder(out, c("species", "week_start", "week_end", "n_tracks", "n_records", "days_with_tracks", "is_partial"))
  out[, species := factor(species, levels = species_all)]
  data.table::setorder(out, species, week_start)
  out[, species := as.character(species)]
  list(weekly = out[], window = list(start = start, end = end))
}

## Resumo por especie na janela: total de tracks/registos, n.o de semanas com presenca e
## semana de maximo (de tracks)
summarise_weekly_track_counts <- function(weekly) {
  w <- data.table::as.data.table(weekly)
  out <- w[, .(
    n_tracks = sum(n_tracks), n_records = sum(n_records),
    n_weeks_present = sum(n_tracks > 0L), n_weeks_total = .N,
    peak_week_start = week_start[which.max(n_tracks)], peak_week_tracks = max(n_tracks)
  ), by = species]
  out[, pct_weeks_present := round(100 * n_weeks_present / n_weeks_total, 1)]
  out[n_tracks == 0L, `:=`(peak_week_start = as.Date(NA), peak_week_tracks = NA_integer_)]
  out[]
}

## Heatmap: 1 tile por especie x semana, cor = n.o de tracks (escala raiz quadrada; zero a cinza,
## como "Absent" na comparacao BSH-DGY), com a contagem escrita no tile. Mesmo tema da comparacao
## BSH-DGY (rotulos do eixo x na vertical).
plot_weekly_track_counts_heatmap <- function(weekly, title = "Weekly number of tracks by species",
                                             date_breaks = "4 weeks", label_size = 2.3) {
  w <- data.table::copy(data.table::as.data.table(weekly))
  w[, species := factor(species, levels = rev(unique(species)))]
  w[, fill_n := ifelse(n_tracks > 0L, n_tracks, NA_real_)]
  w[, label := ifelse(n_tracks > 0L, as.character(n_tracks), "")]
  ggplot2::ggplot(w, ggplot2::aes(x = week_start, y = species, fill = fill_n)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = label_size, colour = "black") +
    ggplot2::scale_fill_gradient(name = "Tracks", low = "#cfe2f8", high = "#2a78d6", trans = "sqrt",
                                 na.value = "#e8e8e8") +
    ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = "%d %b %Y", expand = c(0, 0)) +
    ggplot2::labs(x = "Week starting", y = NULL, title = title) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6.5),
      axis.text.y = ggplot2::element_text(size = 7),
      legend.position = "bottom",
      panel.grid = ggplot2::element_blank()
    )
}

## Barras: n.o de tracks por semana (todas as especies) e, a cheio, os das especies prioritarias.
## Semana parcial (ultima) a hachurado claro -- ver is_partial.
plot_weekly_track_counts_bars <- function(weekly, priority_species, title = "Weekly number of tracks",
                                          date_breaks = "4 weeks") {
  w <- data.table::as.data.table(weekly)
  all_w <- w[species == "All species", .(week_start, n_tracks, is_partial)]
  pri_w <- w[species %in% priority_species, .(n_tracks = sum(n_tracks)), by = week_start]
  d <- rbind(
    all_w[, .(week_start, group = "All species", n_tracks, is_partial)],
    merge(pri_w, all_w[, .(week_start, is_partial)], by = "week_start")[, .(week_start, group = "Priority species", n_tracks, is_partial)]
  )
  d[, group := factor(group, levels = c("All species", "Priority species"))]
  ggplot2::ggplot(d, ggplot2::aes(x = week_start, y = n_tracks, fill = group, alpha = is_partial)) +
    ggplot2::geom_col(position = "identity", width = 6) +
    ggplot2::scale_fill_manual(name = NULL, values = c("All species" = "#a9c7ef", "Priority species" = "#2a78d6")) +
    ggplot2::scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.45), guide = "none") +
    ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = "%d %b %Y", expand = c(0.01, 0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = "Week starting", y = "Tracks", title = title,
                  caption = "Lighter bar = partial week (fewer than 7 days in the data period).") +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6.5),
      legend.position = "bottom"
    )
}


##
## Unidades IDF ativas por semana e intensidade POR UNIDADE
##
## O n.o de unidades IDF a recolher dados varia ao longo do tempo (avarias, unidades novas),
## e as contagens brutas de tracks dependem dele. Mesma definicao de R/regional_assessment.R
## (regional_weekly_active_units()): uma unidade e' ATIVA numa semana se registou pelo menos
## 1 ponto de track nessa semana (qualquer especie; track$idf = TowerNumber). Os tracks so' dizem
## que unidades registaram aves -- uma unidade sem aves e uma avariada sao indistinguiveis;
## os heartbeats dariam o funcionamento real.
##

## Numero da unidade: ultimo grupo de digitos ("IDF-22" -> 22); NA se nao houver digitos
## (mesma logica de regional_unit_number(), R/regional_assessment.R)
weekly_unit_number <- function(x) suppressWarnings(as.integer(sub(".*?([0-9]+)$", "\\1", as.character(x))))

## Pares (semana, unidade) com pelo menos 1 registo, com as MESMAS semanas (bins de 7 dias a
## partir de `start`) de weekly_track_counts()
weekly_active_units <- function(track_dt, tz, start, end) {
  start <- as.Date(start); end <- as.Date(end)
  d <- data.table::as.data.table(track_dt)[!is.na(idf), .(day = as.Date(timestamp, tz = tz), unit = weekly_unit_number(idf))]
  d <- d[!is.na(unit) & day >= start & day <= end]
  d[, week_start := start + (as.integer(day - start) %/% 7L) * 7L]
  unique(d[, .(week_start, unit)])
}

## Resumo por semana: n.o de unidades ativas, e sinal de discrepancia (semanas com menos de
## `low_share` da mediana de unidades ativas). Semanas parciais (partial_weeks, datas de inicio) tem
## naturalmente menos unidades ativas (menos dias): ficam fora da mediana e nunca sao sinalizadas.
summarise_active_units <- function(active_dt, week_starts, low_share = 0.8, partial_weeks = NULL) {
  n <- active_dt[, .(n_active_units = .N), by = week_start]
  out <- merge(data.table::data.table(week_start = week_starts), n, by = "week_start", all.x = TRUE)
  out[is.na(n_active_units), n_active_units := 0L]
  out[, is_partial := week_start %in% partial_weeks]
  med <- stats::median(out[is_partial == FALSE, n_active_units])
  out[, low_vs_median := !is_partial & n_active_units < low_share * med]
  out[]
}

## Por unidade: 1.a e ultima semana ativa, n.o de semanas ativas e % das semanas, entre a 1.a e a
## ultima ativa, em que registou (lacunas = possiveis avarias/sem aves)
summarise_unit_activity <- function(active_dt, week_starts) {
  idx <- match(active_dt$week_start, week_starts)
  d <- data.table::copy(active_dt)[, idx := idx]
  out <- d[, .(first_week = min(week_start), last_week = max(week_start), n_weeks_active = .N,
               n_weeks_between = max(idx) - min(idx) + 1L), by = unit]
  out[, pct_weeks_active_between := round(100 * n_weeks_active / n_weeks_between, 1)]
  data.table::setorder(out, unit)
  out[]
}

## Intensidade por unidade: tracks por dia por unidade ativa, por especie e semana =
## n_tracks / (dias da semana na janela x unidades ativas da semana). NA se nao houver
## unidades ativas na semana.
weekly_intensity_per_unit <- function(weekly, active_summary) {
  w <- data.table::as.data.table(weekly)[species != "All species"]
  w <- merge(w, active_summary[, .(week_start, n_active_units)], by = "week_start", all.x = TRUE)
  w[, days_in_week := as.integer(week_end - week_start) + 1L]
  w[, tracks_per_day_per_unit := ifelse(!is.na(n_active_units) & n_active_units > 0L, n_tracks / (days_in_week * n_active_units), NA_real_)]
  w[]
}

## Rotulo legivel da especie ("White-Tailed-Eagle" -> "White Tailed Eagle")
species_label <- function(x) gsub("-", " ", x)

## Classes de contagem com limites CRESCENTES (1-2-5 aprox.): 0 | 1-9 | 10-49 | 50-99 | 100-499 | 500-999 |
## 1000-4999 | >= 5000. Uma escala continua (mesmo logaritmica) deixa as contagens baixas pouco distinguiveis
## na barra de cores; classes discretas dao um degrau de cor claro em cada ordem de grandeza.
## Devolve fator ordenado; "0" e' a 1.a classe. Classes vazias acima do maximo observado sao
## descartadas (o fator mantem todas as classes ate' a do maximo).
count_classes <- function(n, breaks = c(1, 10, 50, 100, 500, 1000, 5000)) {
  breaks <- sort(unique(breaks))
  top <- max(c(n, 0), na.rm = TRUE)
  breaks <- breaks[breaks <= max(top, breaks[1L])]                # nao criar classes acima do maximo
  lbl <- vapply(seq_along(breaks), function(i) {
    if (i == length(breaks)) sprintf("\u2265 %s", format(breaks[i], big.mark = "", scientific = FALSE))
    else sprintf("%s\u2013%s", format(breaks[i], big.mark = "", scientific = FALSE), format(breaks[i + 1L] - 1, big.mark = "", scientific = FALSE))
  }, character(1))
  levels_all <- c("0", lbl)
  cls <- ifelse(n <= 0, 1L, findInterval(n, breaks) + 1L)
  factor(levels_all[cls], levels = levels_all, ordered = TRUE)
}

## Heatmap sem numeros: so' cores. Cor = CLASSE do n.o de tracks por especie e semana (count_classes()),
## zero a cinzento, sem a linha "All species". Paleta sequencial de claro a escuro, 1 cor por classe.
plot_weekly_track_counts_colours <- function(weekly, title = "Weekly number of tracks by species", date_breaks = "4 weeks",
                                             breaks = c(1, 10, 50, 100, 500, 1000, 5000)) {
  w <- data.table::as.data.table(weekly)[species != "All species"]
  w[, species_lbl := factor(species_label(species), levels = rev(unique(species_label(species))))]
  w[, cls := count_classes(n_tracks, breaks)]
  lv <- levels(w$cls)
  cols <- c("#e8e8e8", grDevices::colorRampPalette(c("#e3eefa", "#a6cbe8", "#5aa0d6", "#2f74b5", "#1a4f96", "#0b2f66"))(length(lv) - 1L))
  names(cols) <- lv
  ggplot2::ggplot(w, ggplot2::aes(x = week_start, y = species_lbl, fill = cls)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_manual(name = "Tracks per week", values = cols, drop = FALSE,
                               guide = ggplot2::guide_legend(nrow = 1, byrow = TRUE)) +
    ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = "%d %b %Y", expand = c(0, 0)) +
    ggplot2::labs(x = "Week starting", y = NULL, title = title, caption = "Grey = no track in the week; classes grow by order of magnitude.") +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6.5),
                   axis.text.y = ggplot2::element_text(size = 7), legend.position = "bottom", panel.grid = ggplot2::element_blank())
}

## Presenca/ausencia semanal (0/1) por especie -- mesmo grafico de plot_species_presence_heatmap()
## (explore_bsh_dgy_comparison.R) para 1 so' parque
plot_weekly_presence <- function(weekly, title = "Weekly presence of priority species", date_breaks = "4 weeks",
                                 present_colour = "#2a78d6") {
  w <- data.table::as.data.table(weekly)[species != "All species"]
  w[, species_lbl := factor(species_label(species), levels = rev(unique(species_label(species))))]
  w[, present_lbl := factor(ifelse(n_tracks > 0L, "Present", "Absent"), levels = c("Absent", "Present"))]
  ggplot2::ggplot(w, ggplot2::aes(x = week_start, y = species_lbl, fill = present_lbl)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_manual(name = NULL, values = c(Absent = "#e8e8e8", Present = present_colour), drop = FALSE) +
    ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = "%d %b %Y", expand = c(0, 0)) +
    ggplot2::labs(x = "Week starting", y = NULL, title = title) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6.5),
                   axis.text.y = ggplot2::element_text(size = 7), legend.position = "bottom", panel.grid = ggplot2::element_blank())
}

## Barras semanais da intensidade (tracks/dia/unidade ativa), 1 linha por especie, cada uma com a
## sua escala de y a partir de zero (linhas nao comparaveis em termos absolutos)
plot_weekly_intensity_per_unit <- function(intensity, title = "Weekly track intensity per IDF unit (mean tracks/day/IDF unit)",
                                           date_breaks = "4 weeks", fill = "#2a78d6") {
  d <- data.table::copy(intensity)
  d[, species_lbl := factor(species_label(species), levels = unique(species_label(species)))]
  d[, week_mid := week_start + 3]
  ggplot2::ggplot(d[!is.na(tracks_per_day_per_unit)], ggplot2::aes(x = week_mid, y = tracks_per_day_per_unit)) +
    ggplot2::geom_col(width = 6, fill = fill) +
    ggplot2::facet_grid(species_lbl ~ ., scales = "free_y", switch = "y") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = "%d %b %Y", expand = c(0.01, 0)) +
    ggplot2::labs(x = "Week starting", y = NULL, title = title,
                  caption = "Each row has its own y scale (starting at zero); rows are not comparable in absolute terms.") +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6.5),
                   axis.text.y = ggplot2::element_blank(), strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1, size = 7),
                   strip.placement = "outside", panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "none")
}

## Diagnostico: n.o de unidades IDF ativas por semana (barras; semanas muito abaixo da mediana a laranja)
plot_weekly_active_units <- function(active_summary, title = "IDF units active per week (at least 1 track)", date_breaks = "4 weeks") {
  d <- data.table::copy(active_summary)
  d[, flag := factor(ifelse(low_vs_median, "Below 80% of the median", "Normal"), levels = c("Normal", "Below 80% of the median"))]
  ggplot2::ggplot(d, ggplot2::aes(x = week_start + 3, y = n_active_units, fill = flag)) +
    ggplot2::geom_col(width = 6) +
    ggplot2::scale_fill_manual(name = NULL, values = c(Normal = "#2a78d6", `Below 80% of the median` = "#e8871e"), drop = FALSE) +
    ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = "%d %b %Y", expand = c(0.01, 0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(x = "Week starting", y = "Active IDF units", title = title) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6.5), legend.position = "bottom")
}

## Diagnostico: 1 tile por unidade x semana (ativa/inativa) para ver QUAIS unidades entram/saem
plot_unit_activity_tiles <- function(active_dt, week_starts, title = "IDF unit activity by week (at least 1 track)", date_breaks = "4 weeks") {
  units <- sort(unique(active_dt$unit))
  grid <- data.table::CJ(unit = units, week_start = week_starts)
  grid <- merge(grid, data.table::copy(active_dt)[, active := 1L], by = c("unit", "week_start"), all.x = TRUE)
  grid[, active_lbl := factor(ifelse(is.na(active), "No track", "Active"), levels = c("No track", "Active"))]
  grid[, unit_lbl := factor(as.character(unit), levels = rev(as.character(units)))]
  ggplot2::ggplot(grid, ggplot2::aes(x = week_start, y = unit_lbl, fill = active_lbl)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.2) +
    ggplot2::scale_fill_manual(name = NULL, values = c(`No track` = "#e8e8e8", Active = "#2a78d6"), drop = FALSE) +
    ggplot2::scale_x_date(date_breaks = date_breaks, date_labels = "%d %b %Y", expand = c(0, 0)) +
    ggplot2::labs(x = "Week starting", y = "IDF unit", title = title) +
    ggplot2::theme_minimal(base_size = 9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 6.5),
                   axis.text.y = ggplot2::element_text(size = 6), legend.position = "bottom", panel.grid = ggplot2::element_blank())
}
