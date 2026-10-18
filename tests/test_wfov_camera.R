## Teste sintetico: R/idf_malfunction_wfov.R (descricao em 3 periodos + inferencia bayesiana do total)
suppressMessages({library(data.table); library(ggplot2)})
source("R/read_utils.R"); source("R/idf_malfunction_wfov.R")
tz <- "Asia/Samarkand"
ev <- as.POSIXct("2026-05-27 02:30:00", tz = tz)
cams8 <- as.character(0:7)
## tracks com ritmo diurno e dias sobredispersos (multiplicador diario gama, CV 0.5)
sim <- function(drop, base = 92, npre = 21, nafter = 7, drop_cam = NULL) {
  rbindlist(lapply(seq.int(-(npre + 7), nafter - 1L), function(d) {
    n <- rpois(1, base * rgamma(1, 4, 4) * (if (d >= 0) 1 - drop else 1))
    hr <- sample(0:23, n, TRUE, prob = dnorm(0:23, 12, 3.5) + 0.01)
    cm <- sample(cams8, n, TRUE)
    if (!is.null(drop_cam) && d >= 0) cm <- sample(setdiff(cams8, drop_cam), n, TRUE)
    data.table(track_id = paste0(d, "_", seq_len(n)), timestamp = ev + d * 86400 + hr * 3600 + runif(n, 0, 3600), idf = 58L, camera = cm)
  }))
}

## descricao: 3 periodos, tracks por dia e variacao
set.seed(1)
w <- rbind(sim(0.6, drop_cam = "3"), data.table(track_id = "x", timestamp = ev, idf = 99L, camera = "1"))
res <- wfov_three_period(w, "GW89-58", ev, 21, 7, 7)
r <- res$rates
stopifnot(isTRUE(res$has_pre), nrow(res$totals) == 3L, res$totals$period[1] == "Earlier period",
          sum(res$totals$tracks) == w[idf == 58L, uniqueN(track_id)], nrow(r) == 9L, r$camera[9] == "All cameras",
          r[camera == "3", n_after] == 0L, r[camera == "3", change_vs_baseline_pct] == -100,
          r[camera == "All cameras", change_vs_baseline_pct] < -20,
          !any(grepl("^p_|^ci_", names(r))))
r2 <- wfov_three_period(w, "GW89-58", ev, 0, 7, 7)
stopifnot(!r2$has_pre, nrow(r2$totals) == 2L)

## bayesiano: queda clara -> P(queda) ~ 1; sem mudanca -> P(queda >= 33%) baixa
b <- wfov_bayes_total(w, "GW89-58", ev, 21, 7, 7, n_iter = 4000, n_warmup = 1000)
s <- b$summary
stopifnot(s$p_fall > 0.9, s$p_fall_threshold > 0.3, s$ci_high_pct < 0, s$ci_low_pct < s$median_change_pct, s$median_change_pct < s$ci_high_pct,
          s$rhat < 1.05, s$ess > 200, s$n_base_days == 28L, s$n_after_days == 7L, nrow(b$sensitivity) == 3L,
          s$threshold_pct == 33, length(b$draws) > 0)
set.seed(2)
w0 <- sim(0)
b0 <- wfov_bayes_total(w0, "GW89-58", ev, 21, 7, 7, n_iter = 4000, n_warmup = 1000)$summary
stopifnot(b0$p_fall_threshold < 0.2, b0$ci_low_pct < 0, b0$ci_high_pct > 0)

## dias fora da cobertura dos TrackReport sao excluidos (nao contam como zero)
rng <- c(ev - 10 * 86400, ev + 7 * 86400)
bc <- wfov_bayes_total(w, "GW89-58", ev, 21, 7, 7, n_iter = 1500, n_warmup = 500, data_range = rng)
stopifnot(bc$summary$n_base_days == 10L, bc$n_days_excluded > 0)
## poucos dias -> sem resultado, sem erro
stopifnot(is.null(wfov_bayes_total(w, "GW89-58", ev, 0, 0, 7, n_iter = 500, n_warmup = 200)$summary))

## reprodutivel, e a seed nao altera o RNG global
set.seed(42); a <- runif(1); set.seed(42)
ba <- wfov_bayes_total(w0, "GW89-58", ev, 21, 7, 7, n_iter = 1000, n_warmup = 300, seed = 5)
stopifnot(identical(runif(1), a))
bb <- wfov_bayes_total(w0, "GW89-58", ev, 21, 7, 7, n_iter = 1000, n_warmup = 300, seed = 5)
stopifnot(identical(ba$summary$median_change_pct, bb$summary$median_change_pct))

## cobertura do intervalo credivel (modelo bem especificado, queda de 33%): >= ~85% nas simulacoes
set.seed(3)
cover <- mean(replicate(30, { x <- sim(0.33); sm <- wfov_bayes_total(x, "GW89-58", ev, 21, 7, 7, n_iter = 1500, n_warmup = 500, seed = NULL, sens_sd = numeric(0))$summary
  sm$ci_low_pct <= -33 && -33 <= sm$ci_high_pct }))
stopifnot(cover >= 0.8)

## frase nao tecnica: adapta-se aos resultados
mk_s <- function(pf, pt, med, lo, hi, nb = 28, na = 7) data.table(p_fall = pf, p_fall_threshold = pt, threshold_pct = 33, median_change_pct = med,
                                                              ci_low_pct = lo, ci_high_pct = hi, n_base_days = nb, n_after_days = na)
sens3 <- function(a, b, c) data.table(prior_theta_sd = c(0.5, 1, 2), p_fall = c(a, b, c))
t1 <- wfov_plain_summary(mk_s(0.95, 0.45, -31, -54, 8), sens3(0.94, 0.95, 0.96))      # exemplo discutido
stopifnot(grepl("strong evidence", t1), grepl("probability 95%", t1), grepl("uncertain", t1), grepl("-31%", t1), grepl("-54% to \\+8%", t1),
          grepl("at least 33% is 45% \\(about as likely as not\\)", t1), grepl("does not depend on the choice of prior", t1), grepl("few", t1), grepl("bird activity", t1))
t2 <- wfov_plain_summary(mk_s(0.999, 0.97, -52, -63, -38, 28, 14), sens3(0.998, 0.999, 0.999))
stopifnot(grepl("very strong evidence", t2), grepl("reasonably well defined", t2), grepl("\\(very likely\\)", t2), !grepl("which is few", t2))
t3 <- wfov_plain_summary(mk_s(0.55, 0.10, -5, -30, 25), sens3(0.5, 0.55, 0.6))
stopifnot(grepl("no clear evidence", t3), grepl("depends on the choice of prior|changes little", t3))
t4 <- wfov_plain_summary(mk_s(0.03, 0, 25, 5, 45), sens3(0.03, 0.03, 0.03))
stopifnot(grepl("rose, not fell", t4), !grepl("fall of at least", t4))
t5 <- wfov_plain_summary(mk_s(0.85, 0.2, -20, -45, 10), sens3(0.7, 0.85, 0.9))
stopifnot(grepl("moderate evidence", t5), grepl("\\(unlikely\\)", t5), grepl("depends on the choice of prior", t5))

## parametrizacao 5 / 5 / 10 dias e limiar 25%
set.seed(7)
w5 <- sim(0.4, npre = 10, nafter = 10)
b5 <- wfov_bayes_total(w5, "GW89-58", ev, 5, 5, 10, fall_threshold = 0.25, n_iter = 2000, n_warmup = 600)
stopifnot(b5$summary$n_base_days == 10L, b5$summary$n_after_days == 10L, b5$summary$threshold_pct == 25, b5$summary$rhat < 1.1)
cat(wfov_plain_summary(b5$summary, b5$sensitivity), "\n")

## graficos
ggsave(tempfile(fileext = ".png"), plot_wfov_rates(res, "t")$p, width = 6, height = 3)
pd <- plot_wfov_daily(res, "t"); ggsave(tempfile(fileext = ".png"), pd$p, width = 6, height = pd$h)
stopifnot(identical(pd$p$theme$axis.text.x$angle, 90))
pp <- plot_wfov_posterior(b, "t"); ggsave(tempfile(fileext = ".png"), pp$p, width = 6, height = pp$h)

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
