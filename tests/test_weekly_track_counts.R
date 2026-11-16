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
cat("test_weekly_track_counts OK\n")
