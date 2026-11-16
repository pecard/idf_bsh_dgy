## Teste sintetico: R/weekly_track_counts.R
suppressMessages({library(data.table); library(ggplot2)})
source("R/weekly_track_counts.R")
tz <- "Asia/Samarkand"
t0 <- as.POSIXct("2026-01-01 00:30:00", tz = tz)    # perto da meia-noite local: UTC daria o dia anterior
## track_dt_test: nome com sufixo _test (nao colide com o track_dt real do pipeline)
track_dt_test <- rbind(
  data.table(track_id = c("a", "a", "a", "b"), timestamp = t0 + c(0, 60, 120, 3600), spec = "Egyptian-Vulture"),       # semana 1 (dia 1), 3 registos de 'a'
  data.table(track_id = "c", timestamp = t0 + 8 * 86400, spec = "Egyptian-Vulture"),                                # semana 2
  data.table(track_id = c("d", "e"), timestamp = t0 + c(2, 3) * 86400, spec = "Steppe-Eagle"),                       # semana 1
  data.table(track_id = "f", timestamp = t0 + 16 * 86400, spec = "Unlisted-Species"))                               # semana 3 (parcial), so' em "All species"
res <- weekly_track_counts(track_dt_test, c("Egyptian-Vulture", "Steppe-Eagle", "Golden-Eagle"), tz = tz)
w <- res$weekly
stopifnot(res$window$start == as.Date("2026-01-01"), res$window$end == as.Date("2026-01-17"))
stopifnot(uniqueN(w$week_start) == 3L, identical(unique(w$species), c("All species", "Egyptian-Vulture", "Steppe-Eagle", "Golden-Eagle")))
g <- function(sp, k, col) w[species == sp & week_start == as.Date("2026-01-01") + 7 * (k - 1), get(col)]
stopifnot(g("Egyptian-Vulture", 1, "n_tracks") == 2L, g("Egyptian-Vulture", 1, "n_records") == 4L, g("Egyptian-Vulture", 1, "days_with_tracks") == 1L,
          g("Egyptian-Vulture", 2, "n_tracks") == 1L, g("Egyptian-Vulture", 3, "n_tracks") == 0L,
          g("Steppe-Eagle", 1, "n_tracks") == 2L, g("Steppe-Eagle", 1, "days_with_tracks") == 2L,
          g("Golden-Eagle", 1, "n_tracks") == 0L,
          g("All species", 1, "n_tracks") == 4L, g("All species", 3, "n_tracks") == 1L, g("All species", 3, "n_records") == 1L)
stopifnot(identical(w[species == "All species", is_partial], c(FALSE, FALSE, TRUE)), w[species == "All species" & is_partial, week_end] == as.Date("2026-01-17"))
## o 1.o registo (00:30 local) fica no dia 2026-01-01 e nao no dia anterior (UTC)
stopifnot(w[species == "All species", sum(n_records)] == nrow(track_dt_test))

s <- summarise_weekly_track_counts(w)
stopifnot(s[species == "Egyptian-Vulture", n_weeks_present] == 2L, s[species == "Egyptian-Vulture", peak_week_tracks] == 2L,
          s[species == "Egyptian-Vulture", peak_week_start] == as.Date("2026-01-01"), is.na(s[species == "Golden-Eagle", peak_week_start]),
          s[species == "Golden-Eagle", pct_weeks_present] == 0, s[species == "All species", n_tracks] == 6L)

## janela explicita recorta os dados
r2 <- weekly_track_counts(track_dt_test, "Egyptian-Vulture", tz = tz, start = "2026-01-08", end = "2026-01-14")
stopifnot(nrow(r2$weekly) == 2L, r2$weekly[species == "All species", n_tracks] == 1L)

## graficos
ph <- plot_weekly_track_counts_heatmap(w); pb <- plot_weekly_track_counts_bars(w, c("Egyptian-Vulture", "Steppe-Eagle"))
ggsave(tempfile(fileext = ".png"), ph, width = 6, height = 3); ggsave(tempfile(fileext = ".png"), pb, width = 6, height = 3)

## unidades IDF ativas, discrepancias e intensidade por unidade
track_dt_test2 <- rbind(
  data.table(track_id = "a1", timestamp = t0, spec = "Egyptian-Vulture", idf = 22),                       # semana 1: unidades 22
  data.table(track_id = "a1", timestamp = t0 + 60, spec = "Egyptian-Vulture", idf = 24),                  #          e 24 (o mesmo track passa por 2 unidades)
  data.table(track_id = "a2", timestamp = t0 + 86400, spec = "Steppe-Eagle", idf = 22),                   # semana 1
  data.table(track_id = "b1", timestamp = t0 + 8 * 86400, spec = "Egyptian-Vulture", idf = 22),           # semana 2: so' a unidade 22 (24 ficou sem dados)
  data.table(track_id = "b2", timestamp = t0 + 9 * 86400, spec = "Egyptian-Vulture", idf = "IDF-22"),     # idem, codigo com texto
  data.table(track_id = "c1", timestamp = t0 + 14 * 86400, spec = "Egyptian-Vulture", idf = 24),          # semana 3 (1 dia): so' a 24
  data.table(track_id = "x", timestamp = t0 + 2 * 86400, spec = "Egyptian-Vulture", idf = NA))            # sem unidade: nao conta como ativa
wk2 <- weekly_track_counts(track_dt_test2, c("Egyptian-Vulture", "Steppe-Eagle"), tz = tz)
ws <- sort(unique(wk2$weekly$week_start))
act <- weekly_active_units(track_dt_test2, tz, wk2$window$start, wk2$window$end)
stopifnot(nrow(act) == 4L, setequal(act[week_start == ws[1], unit], c(22L, 24L)), identical(act[week_start == ws[2], unit], 22L), identical(act[week_start == ws[3], unit], 24L))
asum <- summarise_active_units(act, ws)
stopifnot(identical(asum$n_active_units, c(2L, 1L, 1L)), identical(asum$low_vs_median, c(FALSE, FALSE, FALSE)))
asum_low <- summarise_active_units(act, c(ws, ws[3] + 7))             # semana sem nenhuma unidade
stopifnot(asum_low$n_active_units[4] == 0L, asum_low$low_vs_median[4])
asum_p <- summarise_active_units(act[week_start != ws[3] | unit != 24L][, .SD], ws, partial_weeks = ws[3])      # semana parcial nunca sinalizada
stopifnot(!asum_p$low_vs_median[3], asum_p$is_partial[3], identical(asum_p$is_partial, c(FALSE, FALSE, TRUE)))
ua <- summarise_unit_activity(act, ws)
stopifnot(ua[unit == 22L, n_weeks_active] == 2L, ua[unit == 24L, n_weeks_between] == 3L, ua[unit == 24L, pct_weeks_active_between] == 66.7)
inten <- weekly_intensity_per_unit(wk2$weekly, asum)
ev1 <- inten[species == "Egyptian-Vulture" & week_start == ws[1]]
stopifnot(!"All species" %in% inten$species, ev1$n_tracks == 2L, ev1$days_in_week == 7L, abs(ev1$tracks_per_day_per_unit - 2 / (7 * 2)) < 1e-12,
          inten[species == "Egyptian-Vulture" & week_start == ws[3], days_in_week] == 1L)
inten0 <- weekly_intensity_per_unit(wk2$weekly, data.table::copy(asum)[2, n_active_units := 0L])
stopifnot(is.na(inten0[species == "Egyptian-Vulture" & week_start == ws[2], tracks_per_day_per_unit]))
stopifnot(species_label("White-Tailed-Eagle") == "White Tailed Eagle")

for (pl in list(plot_weekly_track_counts_colours(w), plot_weekly_presence(w), plot_weekly_intensity_per_unit(inten),
                plot_weekly_active_units(asum), plot_unit_activity_tiles(act, ws))) {
  ggsave(tempfile(fileext = ".png"), pl, width = 6, height = 3)
}
cat("test_weekly_track_counts OK\n")
