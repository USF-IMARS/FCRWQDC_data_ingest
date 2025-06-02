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
    return(NA_real_)
  }

  # (4) run the test inside tryCatch
  out_est <- tryCatch({
    fit <- kendallSeasonalTrendTest(values ~ season + year)
    fit$estimate
  }, error = function(e) {
    warning("kendallSeasonalTrendTest failed: ", e$message)
    return(NA_real_)
  })

  # (5) if we got a single NA back from tryCatch, just return NA
  if (length(out_est) == 1 && is.na(out_est)) {
    return(NA_real_)
  }

  # (6) out_est is now a named vector: (tau, slope, intercept).
  #     Extract only the "slope" element:
  if (!("slope" %in% names(out_est))) {
    warning("No 'slope' element found in kendallSeasonalTrendTest output.")
    return(NA_real_)
  }
  sen_slope <- out_est["slope"]

  # (7) Return that single slope (it’s already a scalar).
  return(as.numeric(sen_slope))
}