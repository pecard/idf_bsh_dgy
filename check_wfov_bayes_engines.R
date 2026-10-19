##
## Verificacao cruzada dos motores MCMC da avaliacao WFOV: "metropolis" (R base) vs "rstanarm" (Stan)
##
## Corre o modelo do total da unidade e o modelo de 3 periodos (placebo) com os dois motores,
## nos MESMOS dados e parametros, e compara medianas, intervalos credíveis e probabilidades.
## Se concordarem (dentro do erro Monte Carlo) o Metropolis proprio fica validado; se nao
## concordarem, ha' algo a investigar (codigo, prior da dispersao, convergencia).
##
## Pre-requisito: install.packages("rstanarm")  (so' o pacote R; em Windows/macOS ha' binarios no CRAN)
##
## Uso:
##   1) com dados reais: definir project_settings_file (como em run_idf_availability.R, ex:
##      project_settings_file <- "userSettings_IDF_availability_ZRF.R") e dar Source a este ficheiro.
##      Usa as falhas WFOV de idf_malfunctions, com os mesmos days_pre/days_before/days_after,
##      limiar e prior dos settings.
##   2) sem dados (despiste do codigo): engine_check_synthetic <- TRUE (dados simulados).
## Saida: tabela na consola e em outputs/wfov_engine_check_<data>.xlsx; grafico com as posteriores
## dos dois motores em outputs/wfov_engine_check_<data>.png.
##

packages <- c("data.table", "ggplot2", "lubridate", "writexl")
for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}
if (!requireNamespace("rstanarm", quietly = TRUE)) {
  stop("O pacote rstanarm nao esta instalado. Instalar com install.packages(\"rstanarm\") e repetir.")
}

folder_input <- "inputs"
if (!dir.exists(folder_input) || !dir.exists("R")) {
  stop("Working directory nao esta na raiz do projeto (esperava 'inputs' e 'R' em ", getwd(), ").")
}
source("R/read_utils.R")
source("R/idf_malfunction_wfov.R")
if (!exists("engine_check_synthetic")) engine_check_synthetic <- FALSE
run_date <- format(Sys.time(), "%Y%m%d_%H%M")
dir.create("outputs", showWarnings = FALSE)

## Parametros por omissao (sobrepostos pelos settings, se existirem)
get_or <- function(name, default) if (exists(name)) get(name) else default

##
## 1. Dados ----
##

if (engine_check_synthetic) {
  message("Modo sintetico: dados simulados.")
  proj_timezone <- "Asia/Samarkand"
  ev <- as.POSIXct("2026-05-27 02:30:00", tz = proj_timezone)
  set.seed(1)
  sim_days <- seq.int(-26, 9)
  wfov_dt <- rbindlist(lapply(sim_days, function(d) {
    n <- rpois(1, 92 * rgamma(1, 4, 4) * (if (d >= 0) 0.6 else 1))
    hr <- sample(0:23, n, TRUE, prob = dnorm(0:23, 12, 3.5) + 0.01)
    data.table(track_id = paste0(d, "_", seq_len(n)), timestamp = ev + d * 86400 + hr * 3600 + runif(n, 0, 3600),
               idf = 58L, camera = sample(as.character(0:7), n, TRUE))
  }))
  events <- data.frame(unit = "GW89-58", event_time = ev, days_pre = 14, days_before = 7, days_after = 10, stringsAsFactors = FALSE)
  data_range <- range(wfov_dt$timestamp)
  fall_threshold <- 0.25; prior_sd <- 1
} else {
  if (!exists("project_settings_file")) {
    stop("project_settings_file nao definido -- ex: project_settings_file <- \"userSettings_IDF_availability_ZRF.R\" (ou engine_check_synthetic <- TRUE).")
  }
  source(file.path(folder_input, project_settings_file))
  mf <- as.data.frame(idf_malfunctions, stringsAsFactors = FALSE)
  mf <- mf[grepl("WFOV", mf$type, ignore.case = TRUE), , drop = FALSE]
  if (nrow(mf) == 0L) stop("idf_malfunctions nao tem falhas do tipo WFOV.")
  if (!"days_pre" %in% names(mf)) mf$days_pre <- 0
  if (!"days_before" %in% names(mf)) mf$days_before <- 7
  if (!"days_after" %in% names(mf)) mf$days_after <- 7
  mf$event_time <- as.POSIXct(as.character(mf$datetime), tz = proj_timezone)
  events <- mf
  wfov_dt <- read_track_wfov(databases_dir, trackreport_pattern, proj_timezone, farm_pattern, units = mf$unit,
                             from = min(mf$event_time - (mf$days_before + mf$days_pre) * 86400),
                             to = max(mf$event_time + mf$days_after * 86400))
  data_range <- attr(wfov_dt, "all_range")
  fall_threshold <- get_or("wfov_fall_threshold", 0.33); prior_sd <- get_or("wfov_prior_theta_sd", 1)
}

##
## 2. Os dois motores, mesmos dados e parametros ----
##

n_iter <- 10000L; n_warmup <- 2000L        # Metropolis (mais amostras: passo aleatorio e' menos eficiente)
stan_iter <- 2000L; stan_warmup <- 1000L   # Stan (HMC/NUTS)

rows <- list(); draws <- list()
for (i in seq_len(nrow(events))) {
  e <- events[i, ]
  lab <- sprintf("%s %s", e$unit, format(e$event_time, "%Y-%m-%d %H:%M"))
  for (eng in c("metropolis", "rstanarm")) {
    message(sprintf("%s -- motor %s ...", lab, eng))
    tot <- wfov_bayes_total(wfov_dt, e$unit, e$event_time, e$days_pre, e$days_before, e$days_after, fall_threshold = fall_threshold,
                            prior_theta_sd = prior_sd, data_range = data_range, engine = eng,
                            n_iter = n_iter, n_warmup = n_warmup, stan_iter = stan_iter, stan_warmup = stan_warmup)
    if (!is.null(tot$summary)) {
      s <- tot$summary
      rows[[length(rows) + 1L]] <- data.table(event = lab, model = "Unit total (baseline vs after)", engine = eng,
        median_change_pct = s$median_change_pct, ci_low_pct = s$ci_low_pct, ci_high_pct = s$ci_high_pct,
        p_fall = s$p_fall, p_fall_threshold = s$p_fall_threshold, rhat = s$rhat, ess = s$ess, n_divergent = s$n_divergent)
      draws[[length(draws) + 1L]] <- data.table(event = lab, model = "Unit total (baseline vs after)", engine = eng, change_pct = tot$draws)
    }
    if (!is.na(e$days_pre) && e$days_pre >= 2) {
      cs <- wfov_bayes_consecutive(wfov_dt, e$unit, e$event_time, e$days_pre, e$days_before, e$days_after, fall_threshold = fall_threshold,
                                   prior_theta_sd = prior_sd, data_range = data_range, engine = eng,
                                   n_iter = n_iter, n_warmup = n_warmup, stan_iter = stan_iter, stan_warmup = stan_warmup)
      if (!is.null(cs)) {
        for (k in 1:2) {
          s <- cs$summary[k]
          rows[[length(rows) + 1L]] <- data.table(event = lab, model = s$comparison, engine = eng,
            median_change_pct = s$median_change_pct, ci_low_pct = s$ci_low_pct, ci_high_pct = s$ci_high_pct,
            p_fall = s$p_fall, p_fall_threshold = s$p_fall_threshold, rhat = s$rhat, ess = s$ess, n_divergent = s$n_divergent)
        }
        rows[[length(rows) + 1L]] <- data.table(event = lab, model = "P(fall at the reported date larger than the earlier change)", engine = eng,
          p_fall = cs$p_larger_at_event)
        draws[[length(draws) + 1L]] <- data.table(event = lab, model = "Before to After (reported date)", engine = eng, change_pct = cs$draws$event)
        draws[[length(draws) + 1L]] <- data.table(event = lab, model = "Earlier to Before (no failure reported)", engine = eng, change_pct = cs$draws$placebo)
      }
    }
  }
}
res <- rbindlist(rows, fill = TRUE)

##
## 3. Comparacao ----
##
## Concordam se: |dP(queda)| <= 0.02, |dP(queda >= limiar)| <= 0.03, |dmediana| <= max(3 pp, 10% da
## largura do intervalo) e |d extremos do intervalo| <= max(5 pp, 15% da largura) (erro Monte Carlo +
## prior da dispersao ligeiramente diferente no rstanarm).

wide <- dcast(res, event + model ~ engine, value.var = c("median_change_pct", "ci_low_pct", "ci_high_pct", "p_fall", "p_fall_threshold"))
comp <- wide[, .(event, model,
  median_metropolis = median_change_pct_metropolis, median_rstanarm = median_change_pct_rstanarm,
  ci_metropolis = ifelse(is.na(ci_low_pct_metropolis), NA_character_, sprintf("%.0f to %.0f", ci_low_pct_metropolis, ci_high_pct_metropolis)),
  ci_rstanarm = ifelse(is.na(ci_low_pct_rstanarm), NA_character_, sprintf("%.0f to %.0f", ci_low_pct_rstanarm, ci_high_pct_rstanarm)),
  p_fall_metropolis = p_fall_metropolis, p_fall_rstanarm = p_fall_rstanarm,
  p_threshold_metropolis = p_fall_threshold_metropolis, p_threshold_rstanarm = p_fall_threshold_rstanarm,
  diff_p_fall = p_fall_rstanarm - p_fall_metropolis,
  diff_p_threshold = p_fall_threshold_rstanarm - p_fall_threshold_metropolis,
  diff_median_pp = median_change_pct_rstanarm - median_change_pct_metropolis,
  diff_ci_low_pp = ci_low_pct_rstanarm - ci_low_pct_metropolis, diff_ci_high_pp = ci_high_pct_rstanarm - ci_high_pct_metropolis)]
# tolerancias: 3 pp (ou 10% da largura do intervalo, o maior) para a mediana e 5 pp (ou 15% da largura) para os extremos
wid <- wide[, ci_high_pct_metropolis - ci_low_pct_metropolis]
comp[, tol_median := pmax(3, 0.10 * wid)][, tol_ci := pmax(5, 0.15 * wid)]
comp[, verdict := ifelse(is.na(diff_median_pp),
  ifelse(abs(diff_p_fall) <= 0.02, "Agree", "Check"),
  ifelse(abs(diff_p_fall) <= 0.02 & (is.na(diff_p_threshold) | abs(diff_p_threshold) <= 0.03) & abs(diff_median_pp) <= tol_median &
           abs(diff_ci_low_pp) <= tol_ci & abs(diff_ci_high_pp) <= tol_ci, "Agree", "Check"))]
print(comp[, .(event, model, p_fall_metropolis, p_fall_rstanarm, diff_p_fall, diff_median_pp, verdict)])

diag <- res[!is.na(rhat), .(event, model, engine, rhat, ess, n_divergent)]
cat("\nConvergence (Rhat should be < 1.05, ESS > 400, divergences = 0 for Stan):\n"); print(diag)

out_x <- file.path("outputs", sprintf("wfov_engine_check_%s.xlsx", run_date))
write_xlsx(list(Comparison = comp, Convergence = diag, All_results = res,
                Settings = data.table(Parameter = c("fall_threshold", "prior_theta_sd", "metropolis n_iter / n_warmup", "rstanarm iter / warmup", "dispersion prior (rstanarm)"),
                                      Value = c(fall_threshold, prior_sd, sprintf("%d / %d", n_iter, n_warmup), sprintf("%d / %d", stan_iter, stan_warmup),
                                                "exponential(0.1) on reciprocal_dispersion; metropolis uses log-normal(log 5, 1.5) on log k"))),
           out_x)

dd <- rbindlist(draws)
p <- ggplot(dd, aes(change_pct, colour = engine, fill = engine)) +
  geom_density(alpha = 0.2) + facet_wrap(~ event + model, scales = "free", ncol = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
  labs(x = "Change in tracks per day (%)", y = "Posterior density", title = "Posterior of the change: metropolis vs rstanarm") +
  theme_minimal(base_size = 9) + theme(legend.position = "bottom")
out_p <- file.path("outputs", sprintf("wfov_engine_check_%s.png", run_date))
ggsave(out_p, p, width = 6.3, height = 2.2 * max(1, uniqueN(dd[, paste(event, model)])), dpi = 200, limitsize = FALSE)
message("Gravado: ", out_x, " e ", out_p)
message(sprintf("Veredicto: %d de %d comparacoes concordam.", sum(comp$verdict == "Agree"), nrow(comp)))
