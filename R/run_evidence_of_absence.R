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
