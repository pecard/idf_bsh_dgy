## Teste sintetico: R/idf_malfunction_wfov.R (3 periodos, permutacao entre periodos)
suppressMessages({library(data.table); library(ggplot2)})
source("R/read_utils.R"); source("R/idf_malfunction_wfov.R")
tz <- "Asia/Samarkand"
ev <- as.POSIXct("2026-05-27 02:30:00", tz = tz)
mk <- function(n, from, cams, prefix, d = 7) data.table(track_id = paste0(prefix, seq_len(n)),
  timestamp = from + runif(n, 0, d * 86400), idf = 58L, camera = sample(cams, n, TRUE))
cams8 <- as.character(1:8)

## degradacao: camara 3 deixa de capturar e o volume cai
set.seed(1)
w <- rbind(mk(300, ev - 14 * 86400, cams8, "p"), mk(310, ev - 7 * 86400, cams8, "b"), mk(180, ev, as.character(c(1:2, 4:8)), "a"),
           data.table(track_id = "x", timestamp = ev, idf = 99L, camera = "1"))
res <- wfov_three_period(w, "GW89-58", ev, 7, 7, 7, n_perm = 3000)
r <- res$rates; sh <- res$shares
stopifnot(isTRUE(res$has_pre), nrow(res$totals) == 3L, res$totals$period[1] == "Earlier period",
          identical(res$totals$tracks, c(300L, 310L, 180L)), nrow(r) == 9L, r$camera[9] == "All cameras",
          r[camera == "3", n_after] == 0L, r[camera == "3", p_degradation_adj] < 0.05, r[camera == "3", exceeds_natural],
          r[camera == "3", p_natural_adj] > 0.05, r[camera == "All cameras", p_degradation] < 0.05,
          r[camera == "1", p_degradation_adj] > 0.05, sh[camera == "3", p_degradation_adj] < 0.05,
          res$p_any_camera < 0.05, abs(r[camera == "All cameras", change_vs_baseline_pct] - (100 * (180 / 7) / (610 / 14) - 100)) < 3)

## sem mudanca: sem degradacao significativa; variacao natural coerente
set.seed(2)
ctl <- rbind(mk(300, ev - 14 * 86400, cams8, "p"), mk(300, ev - 7 * 86400, cams8, "b"), mk(300, ev, cams8, "a"))
r0 <- wfov_three_period(ctl, "GW89-58", ev, 7, 7, 7, n_perm = 2000)
stopifnot(r0$p_total > 0.05, r0$p_any_camera > 0.05, r0$p_any_share > 0.05)

## reprodutivel e a seed nao altera o RNG global
set.seed(42); a <- runif(1); set.seed(42)
r0a <- wfov_three_period(ctl, "GW89-58", ev, 7, 7, 7, n_perm = 500)
stopifnot(identical(runif(1), a))
r0b <- wfov_three_period(ctl, "GW89-58", ev, 7, 7, 7, n_perm = 500)
stopifnot(identical(r0a$rates$p_degradation, r0b$rates$p_degradation))

## sem periodo adicional: so' Before/After
r2 <- wfov_three_period(w, "GW89-58", ev, 0, 7, 7, n_perm = 500)
stopifnot(!r2$has_pre, nrow(r2$totals) == 2L, is.na(r2$rates$exceeds_natural[1]), r2$rates[camera == "3", p_degradation_adj] < 0.05)

## unilateral: um AUMENTO de volume nao e' degradacao
up <- rbind(mk(200, ev - 14 * 86400, cams8, "p"), mk(200, ev - 7 * 86400, cams8, "b"), mk(400, ev, cams8, "a"))
ru <- wfov_three_period(up, "GW89-58", ev, 7, 7, 7, n_perm = 1000)
stopifnot(ru$p_total > 0.9)

## graficos
ggsave(tempfile(fileext = ".png"), plot_wfov_rates(res, "t")$p, width = 6, height = 3)
pd <- plot_wfov_daily(res, "t"); ggsave(tempfile(fileext = ".png"), pd$p, width = 6, height = pd$h)
stopifnot(identical(pd$p$theme$axis.text.x$angle, 90))

## contexto
hb <- data.table(idf = "GW89-58", timestamp = ev - 8 * 86400 + seq(0, 12.9 * 86400, by = 1800))
cx <- wfov_event_context("GW89-58", ev, 5, 5, hb, data.table(turbine = "T94", start = ev - 86400), data.table(turbinelabel = "T94", datetime = ev - 3600), "T94", days_pre = 2)
stopifnot(nrow(cx) == 3L, abs(cx$earlier[1] - 48) < 1)

## leitura de ficheiro
d <- tempfile(); dir.create(d)
fwrite(data.table(TrackId = "a", DateTimeStamp = "2026-05-26 10:00:00", TowerNumber = c(58, 99), WFOV_camera = c(3, 4)), file.path(d, "TrackReport_Default_TST.csv"))
rd <- read_track_wfov(d, "TrackReport_Default_TST", tz, units = "GW89-58")
stopifnot(nrow(rd) == 1L, rd$camera == "3")
cat("test_wfov_camera OK\n")
