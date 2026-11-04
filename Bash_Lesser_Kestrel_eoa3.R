# Bash Lesser Kestrel: single-period EoA calculations using eoa3 directly
# Prepared 2026-10-30. Run in RStudio with Source, or:
#   Rscript Bash_Lesser_Kestrel_eoa3.R
#
# SETUP: install R and JAGS before the first run. eoa3 imports rjags, which
# requires the separate JAGS application, even though postM itself uses no MCMC.
# JAGS: https://mcmc-jags.sourceforge.io/
# The script can install missing R packages; it does not install JAGS or R.
#
# Scope: reproduce the provisional FIXED-g scenarios discussed for Bash.
# This calls the installed eoa3::postM, not a replacement implementation.
# It does not fit trials, propagate parameter uncertainty, correct actual missed
# searches, pool years, infer SDOD efficacy, or predict removal of curtailment.
# All study inputs and user controls are grouped in Sections 1 and 2.
#
# References:
# Dalthorp, Huso & Dail (2017): https://doi.org/10.3133/ds1055
# Package: https://CRAN.R-project.org/package=eoa3
# Manual: https://ddalthorp.r-universe.dev/eoa3/doc/manual.html
# R/fun_M.R: https://rdrr.io/cran/eoa3/src/R/fun_M.R

# ---------------------------------------------------------------------------
# 1. EDITABLE STUDY INPUTS
# ---------------------------------------------------------------------------
study <- list(
  project = "Bash",
  species = "Falco naumanni",
  report = "Bash_1st PCFM_Annual Report_20260824.docx",
  turbines_searched = 79L,
  turbines_total = 79L,
  search_interval_days = 7,
  mean_persistence_days = 18.513,
  persistence_distribution = "exponential", # assumed; not a fitted report model
  search_efficiency_mode = "report_weighted", # or "manual"
  search_efficiency_manual = 0.896742746615087,
  # Table 6 seasonal estimates; Table 3 planned searches used as proxy weights.
  # These weights approximate an annual SE, not a species-specific fitted model.
  se_breeding = 0.938,
  se_nonbreeding = 0.848,
  planned_searches_breeding = 2240,
  planned_searches_nonbreeding = 1896,
  spatial_coverage = 0.60, # provisional expert-informed assumption, not fitted DWP
  k = 0, # formula below is intentionally restricted to k = 0
  annual_allocation = 12L
)

# Each row is analysed INDEPENDENTLY. The zero-count row is a scenario until
# its dates and monitoring coverage are confirmed; it is not a full-year claim.
# "coverage_of_target_mortality" = fraction of all fatalities in the target
# inference period expected to arise during its monitoring window. Set to 1
# to estimate only the monitored window; it is NOT fraction of searches done.
# Do not enter calendar-year fractions here without a mortality timing basis.
periods <- data.frame(
  label = c("PCFM Year 1: two detections", "2026: zero-detection scenario"),
  observed_fatalities = c(2L, 0L),
  monitoring_start = c("2025-04-15", NA_character_),
  monitoring_end = c("2026-04-15", NA_character_),
  target_period = c("First PCFM year", "Monitored window; dates unconfirmed"),
  coverage_of_target_mortality = c(1, 1),
  enabled = c(TRUE, TRUE),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# 2. EDITABLE ANALYSIS / OUTPUT CONTROLS
# ---------------------------------------------------------------------------
control <- list(
  install_missing_packages = TRUE,
  cran_repository = "https://cloud.r-project.org",
  expected_eoa3_version = "1.0.0.2", # other versions run with a notice
  prior = "IbinRef", # fixed-g postM also supports "binRef" or "uniform"
  credible_level = 0.95,
  upper_bound_credibility = 0.95,
  mmax = 1000L, # numerical integration cutoff, NOT a biological maximum
  verify_larger_cutoff = TRUE,
  cutoff_multiplier = 2L,
  posterior_tolerance = 1e-9,
  tail_check_cells = 20L,
  tail_mass_tolerance = 1e-10,
  print_digits = 6L,
  write_files = TRUE,
  output_directory = file.path(getwd(), "Bash_eoa3_results"),
  summary_filename = "mortality_summary.csv",
  posterior_filename = "mortality_posteriors.csv",
  audit_filename = "inputs_and_session.txt"
)

# ---------------------------------------------------------------------------
# 3. INPUT VALIDATION AND PACKAGE SETUP
# ---------------------------------------------------------------------------
scalar_number <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
check_probability <- function(x, label, allow_zero = FALSE) {
  lower_ok <- if (allow_zero) x >= 0 else x > 0
  if (!scalar_number(x) || !isTRUE(lower_ok) || x > 1)
    stop(label, " must be a finite probability in ",
         if (allow_zero) "[0,1]." else "(0,1].", call. = FALSE)
}
positive_fields <- c("turbines_searched", "turbines_total",
                     "search_interval_days", "mean_persistence_days",
                     "planned_searches_breeding", "planned_searches_nonbreeding")
for (nm in positive_fields) {
  if (!scalar_number(study[[nm]]) || study[[nm]] <= 0)
    stop("Invalid positive input: ", nm, call. = FALSE)
}
if (study$turbines_searched > study$turbines_total)
  stop("Searched turbines cannot exceed total turbines.", call. = FALSE)
if (!identical(study$persistence_distribution, "exponential") || study$k != 0)
  stop("This simplified detection formula supports exponential persistence and k=0 only.",
       call. = FALSE)
check_probability(study$spatial_coverage, "spatial_coverage")
check_probability(study$se_breeding, "se_breeding")
check_probability(study$se_nonbreeding, "se_nonbreeding")
if (!study$search_efficiency_mode %in% c("manual", "report_weighted"))
  stop("search_efficiency_mode must be manual or report_weighted.", call. = FALSE)
if (!control$prior %in% c("IbinRef", "binRef", "uniform"))
  stop("Unsupported named prior for fixed-g postM.", call. = FALSE)
if (control$prior == "binRef" && any(periods$enabled & periods$observed_fatalities == 0))
  stop("Use IbinRef or uniform for zero counts; binRef is singular at M=0.", call. = FALSE)
for (nm in c("credible_level", "upper_bound_credibility")) {
  check_probability(control[[nm]], nm)
  if (control[[nm]] == 1) stop(nm, " must be less than 1.", call. = FALSE)
}
if (!scalar_number(study$annual_allocation) || study$annual_allocation < 0 ||
    study$annual_allocation != round(study$annual_allocation))
  stop("annual_allocation must be a non-negative integer.", call. = FALSE)
if (anyNA(periods$enabled) || !is.logical(periods$enabled))
  stop("enabled must contain TRUE/FALSE values.", call. = FALSE)
active <- periods[periods$enabled, , drop = FALSE]
if (!nrow(active)) stop("Enable at least one period.", call. = FALSE)
if (anyNA(active$observed_fatalities) || any(!is.finite(active$observed_fatalities)) ||
    any(active$observed_fatalities < 0) ||
    any(active$observed_fatalities != round(active$observed_fatalities)))
  stop("Observed fatalities must be non-negative integers.", call. = FALSE)
for (v in active$coverage_of_target_mortality)
  check_probability(v, "coverage_of_target_mortality")
if (!scalar_number(control$mmax) || control$mmax != round(control$mmax) ||
    control$mmax <= max(active$observed_fatalities))
  stop("mmax must be an integer larger than every carcass count.", call. = FALSE)

if (!requireNamespace("eoa3", quietly = TRUE) && control$install_missing_packages) {
  message("Installing eoa3 and required R dependencies. JAGS must already be installed.")
  tryCatch(
    utils::install.packages("eoa3", repos = control$cran_repository,
                            dependencies = NA),
    error = function(e) message("Installation error: ", conditionMessage(e))
  )
}
if (!requireNamespace("eoa3", quietly = TRUE)) {
  stop(paste(
    "Cannot load eoa3. Install JAGS from https://mcmc-jags.sourceforge.io/",
    "then restart R and install.packages('eoa3').",
    "On Linux, JAGS development files may also be needed for rjags."
  ), call. = FALSE)
}
installed_version <- as.character(utils::packageVersion("eoa3"))
if (installed_version != control$expected_eoa3_version)
  message("eoa3 version is ", installed_version,
          "; reference version was ", control$expected_eoa3_version, ".")

# ---------------------------------------------------------------------------
# 4. DETECTION PROBABILITY: SAME APPROXIMATION AS THE PREVIOUS CALCULATIONS
# ---------------------------------------------------------------------------
p <- if (study$search_efficiency_mode == "manual") {
  study$search_efficiency_manual
} else {
  stats::weighted.mean(
    c(study$se_breeding, study$se_nonbreeding),
    c(study$planned_searches_breeding, study$planned_searches_nonbreeding)
  )
}
check_probability(p, "Derived searcher efficiency")
turbine_fraction <- study$turbines_searched / study$turbines_total
# Uniform arrival within each regular search interval, exponential survival.
# expm1 is numerically stable when interval / mean persistence is very small.
r <- -study$mean_persistence_days / study$search_interval_days *
  expm1(-study$search_interval_days / study$mean_persistence_days)
g_monitored <- p * r * study$spatial_coverage * turbine_fraction
active$g <- g_monitored * active$coverage_of_target_mortality
if (any(active$g < 1e-5 | active$g > 0.99999))
  stop("g is outside the supported numeric range of this script.", call. = FALSE)

# ---------------------------------------------------------------------------
# 5. OFFICIAL eoa3 POSTERIOR AND TWO INTERVAL CONVENTIONS
# ---------------------------------------------------------------------------
discrete_quantile <- function(probability, masses) {
  which(cumsum(masses) >= probability)[1L] - 1L
}
run_period <- function(row) {
  x <- row$observed_fatalities
  g <- row$g
  posterior <- eoa3::postM(x = x, g = g, prior = control$prior,
                          mmax = control$mmax)
  if (length(posterior) != control$mmax + 1L || anyNA(posterior) ||
      any(!is.finite(posterior)) || any(posterior < 0) ||
      abs(sum(posterior) - 1) > control$posterior_tolerance)
    stop("Invalid posterior for ", row$label, call. = FALSE)
  if (sum(tail(posterior, control$tail_check_cells)) > control$tail_mass_tolerance)
    stop("Posterior reaches numerical cutoff; increase mmax.", call. = FALSE)
  if (control$verify_larger_cutoff) {
    larger <- eoa3::postM(x = x, g = g, prior = control$prior,
                          mmax = control$mmax * control$cutoff_multiplier)
    difference <- sum(abs(posterior - larger[seq_along(posterior)])) +
      sum(larger[-seq_along(posterior)])
    if (!is.finite(difference) || difference > control$posterior_tolerance)
      stop("Cutoff sensitivity detected; increase mmax.", call. = FALSE)
  }
  M <- seq_along(posterior) - 1L
  alpha <- 1 - control$credible_level
  equal_tailed <- vapply(c(alpha / 2, 1 - alpha / 2),
                        discrete_quantile, integer(1), masses = posterior)
  # Package MCI uses a different convention for discrete distributions.
  # Report it alongside equal-tailed intervals, never interchange the labels.
  native_interval <- eoa3::MCI(pMgX = posterior, crlev = control$credible_level)
  upper_bound <- eoa3::calcMstar(
    pMgX = posterior, alpha = 1 - control$upper_bound_credibility
  )
  result <- data.frame(
    period = row$label, target_period = row$target_period,
    monitoring_start = row$monitoring_start, monitoring_end = row$monitoring_end,
    observed = x, detection_probability = g,
    prior = control$prior, posterior_median = discrete_quantile(0.5, posterior),
    posterior_mean = sum(M * posterior), credible_level = control$credible_level,
    equal_tailed_lower = equal_tailed[1], equal_tailed_upper = equal_tailed[2],
    equal_tailed_actual_mass = sum(posterior[M >= equal_tailed[1] & M <= equal_tailed[2]]),
    eoa3_MCI_lower = native_interval[1], eoa3_MCI_upper = native_interval[2],
    eoa3_MCI_actual_mass = sum(posterior[M >= native_interval[1] & M <= native_interval[2]]),
    upper_one_sided_level = control$upper_bound_credibility,
    upper_one_sided_limit = upper_bound, annual_allocation = study$annual_allocation,
    probability_at_most_allocation = sum(posterior[M <= study$annual_allocation]),
    probability_exceeds_allocation = sum(posterior[M > study$annual_allocation]),
    probability_reaches_allocation = sum(posterior[M >= study$annual_allocation]),
    eoa3_version = installed_version, stringsAsFactors = FALSE
  )
  list(summary = result, posterior = data.frame(
    period = row$label, M = M, probability = posterior,
    cumulative_probability = cumsum(posterior), stringsAsFactors = FALSE
  ))
}
results <- lapply(seq_len(nrow(active)), function(i) run_period(active[i, , drop = FALSE]))
summary_table <- do.call(rbind, lapply(results, function(x) x$summary))
posterior_table <- do.call(rbind, lapply(results, function(x) x$posterior))

# ---------------------------------------------------------------------------
# 6. DISPLAY AND SAVE
# ---------------------------------------------------------------------------
cat("\n", study$project, " / ", study$species, "\n", sep = "")
cat("eoa3 version:", installed_version, "\n")
print(data.frame(search_efficiency = p, mean_persistence_days = study$mean_persistence_days,
                 persistence_to_first_search = r, spatial_coverage = study$spatial_coverage,
                 k = study$k, g_monitored = g_monitored), digits = control$print_digits,
      row.names = FALSE)
cat("\nIndependent-period EoA results (no borrowing between periods):\n")
print(summary_table, digits = control$print_digits, row.names = FALSE)
cat(paste0(
  "\nInterpretation:\n",
  "* Equal-tailed intervals reproduce the convention used in the chat.\n",
  "* eoa3_MCI columns are the package's native interval convention.\n",
  "* Spatial coverage ", format(study$spatial_coverage),
  " is provisional; detection parameters are fixed.\n",
  "* Actual missed-search intervals and parameter uncertainty are not modelled.\n",
  "* A zero-count window with unknown dates is not a completed annual result.\n",
  "* Threshold probabilities concern the specified target period. Comparison with\n",
  "  an annual allocation requires an annual target period.\n",
  "* These results do not predict mortality after removing kestrel curtailment.\n"
))
if (control$write_files) {
  dir.create(control$output_directory, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(summary_table, file.path(control$output_directory,
                                           control$summary_filename), row.names = FALSE)
  utils::write.csv(posterior_table, file.path(control$output_directory,
                                             control$posterior_filename), row.names = FALSE)
  audit <- capture.output({
    cat("Bash fixed-g EoA analysis\nRun:", format(Sys.time()), "\n\n")
    cat("STUDY INPUTS\n"); dput(study)
    cat("\nPERIODS\n"); dput(periods)
    cat("\nCONTROLS\n"); dput(control)
    cat("\nRESULTS\n"); print(summary_table)
    cat("\nPACKAGE CITATION\n"); print(utils::citation("eoa3"))
    cat("\nSESSION\n"); print(utils::sessionInfo())
  })
  writeLines(audit, file.path(control$output_directory, control$audit_filename))
  cat("\nFiles saved in:", normalizePath(control$output_directory), "\n")
}
# Objects retained when sourced interactively:
# summary_table, posterior_table, results, study, periods, control, p, r.
