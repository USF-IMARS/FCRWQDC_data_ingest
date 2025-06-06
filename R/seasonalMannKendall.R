library(EnvStats)

seasonalMannKendallVectorized <- function(dates, values, na.rm = TRUE) {
  # (1) drop NAs
  if (na.rm) {
    keep   <- !is.na(values) & !is.na(dates)
    dates  <- dates[keep]
    values <- values[keep]
  }

  # (2) compute numeric month/year
  season <- as.numeric(format(dates, "%m"))
  year   <- as.numeric(format(dates, "%Y"))

  # (3) need at least 2 unique years
  if (length(unique(year)) < 2) {
    warning("Not enough years of data to perform seasonal Mann–Kendall test.")
    return(tibble::tibble(
      slope   = NA_real_,
      tau        = NA_real_,
      chi_square = NA_real_,
      z          = NA_real_))
}

  # (4) run the test inside tryCatch
  fit <- tryCatch({
    fit <- kendallSeasonalTrendTest(values ~ season + year)
  }, error = function(e) {
    warning("kendallSeasonalTrendTest failed: ", e$message)
    return(tibble::tibble(
      slope   = NA_real_,
      tau        = NA_real_,
      chi_square = NA_real_,
      z          = NA_real_))})
  
  # print(summary(fit))
  
  
  # Includes tau, slope, intercept output. Tau is similar to R2.
  out_est <- fit$estimate

  # (5) if we got a single NA back from tryCatch, just return NA
  # or no slope found
  if (length(out_est) == 1 && is.na(out_est) || !("slope" %in% names(out_est))) {
    return(tibble::tibble(
      slope      = NA_real_,
      tau        = NA_real_,
      chi_square = NA_real_,
      z          = NA_real_))
  } else {
    return(tibble::tibble(
      slope   = fit$estimate["slope"],
      tau     = fit$estimate["tau"],
      chi_square = fit[["p.value"]]["Chi-Square (Het)"],
      z          = fit[["p.value"]]["z (Trend)"]))
  }
}