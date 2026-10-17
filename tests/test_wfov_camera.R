## Teste sintetico: R/idf_malfunction_wfov.R
suppressMessages({library(data.table); library(ggplot2)})
source("R/read_utils.R"); source("R/idf_malfunction_wfov.R")
tz <- "Asia/Samarkand"
set.seed(1)
ev <- as.POSIXct("2026-05-27 02:30:00", tz = tz)
mk <- function(n, from, cams) data.table(track_id = paste0("t", from, seq_len(n)),
  timestamp = from + runif(n, 0, 5 * 86400), idf = 58L, camera = sample(cams, n, TRUE))
wfov_test <- rbind(mk(160, ev - 5 * 86400, as.character(1:8)), mk(150, ev, as.character(c(1:2, 4:8))),
                   data.table(track_id = "x", timestamp = ev, idf = 99L, camera = "1"))
res <- wfov_camera_compare(wfov_test, "GW89-58", ev, 5, 5)
s <- res$summary
stopifnot(nrow(s) == 8L, s[camera == "3", status] == "No tracks after the event", s[camera == "3", n_after] == 0L,
          sum(s$n_before) == 160L, sum(s$n_after) == 150L, s[camera == "3", p_adj] < 0.05, s[camera == "1", p_adj] > 0.05, res$omnibus < 0.05)
stopifnot(res$totals[period == "After", cameras_active] == 7L)
p1 <- plot_wfov_share(res, "t"); p2 <- plot_wfov_daily(res, "t")
ggsave(tempfile(fileext = ".png"), p1$p, width = 6, height = 3); ggsave(tempfile(fileext = ".png"), p2$p, width = 6, height = 3)
hb <- data.table(idf = "GW89-58", timestamp = ev - 5 * 86400 + seq(0, 9.9 * 86400, by = 1800))
cx <- wfov_event_context("GW89-58", ev, 5, 5, hb, data.table(turbine = "T94", start = ev - 86400), data.table(turbinelabel = "T94", datetime = ev - 3600), "T94")
stopifnot(nrow(cx) == 3L, abs(cx[1, before] - 48) < 1)
# leitura de ficheiro
d <- tempfile(); dir.create(d)
fwrite(data.table(TrackId = "a", DateTimeStamp = "2026-05-26 10:00:00", TowerNumber = c(58, 99), WFOV_camera = c(3, 4)), file.path(d, "TrackReport_Default_TST.csv"))
r <- read_track_wfov(d, "TrackReport_Default_TST", tz, units = "GW89-58")
stopifnot(nrow(r) == 1L, r$camera == "3")
# sem mudanca: omnibus nao significativo; reprodutivel e seed nao altera o RNG global
ctl_test <- rbind(mk(160, ev - 5 * 86400, as.character(1:8)), mk(150, ev, as.character(1:8)))
set.seed(42); a <- runif(1); set.seed(42)
r0 <- wfov_camera_compare(ctl_test, "GW89-58", ev, 5, 5, n_perm = 2000)
stopifnot(r0$omnibus > 0.05, identical(runif(1), a), all(r0$summary$p_adj >= r0$summary$p_perm - 1e-12))
r0b <- wfov_camera_compare(ctl_test, "GW89-58", ev, 5, 5, n_perm = 2000)
stopifnot(identical(r0$summary$p_perm, r0b$summary$p_perm))
r1 <- wfov_camera_compare(ctl_test, "GW89-58", ev, 5, 5, n_perm = 500, block_hours = NA)
stopifnot(all(!is.na(r1$summary$p_perm)))
cat("test_wfov_camera OK\n")
