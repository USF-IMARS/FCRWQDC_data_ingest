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
  fit <- tryCatch({
    fit <- kendallSeasonalTrendTest(values ~ season + year)
  }, error = function(e) {
    warning("kendallSeasonalTrendTest failed: ", e$message)
    return(NA_real_)
  })

  out_est <- fit$estimate

  # (5) if we got a single NA back from tryCatch, just return NA
  # or no slope found
  if (length(out_est) == 1 && is.na(out_est) || !("slope" %in% names(out_est))) {
    return(tibble:tibble(
      trend   = NA_real_,
      p.value = NA_real_,
      tau     = NA_real_
    ))
  }


  #Includes tau, slope, intercept output. Tau is similar to R2.
  sen_slope <- 



  # TODO: add p_value & tau
  #Actually Chi-square and z value. The z value is for whether the overall trend
  #is signifiant
  p_value <- out_est$p.value
  

  print(summary(fit))


  # (7) Return that single slope (it’s already a scalar).
  return(list(slope = as.numeric(sen_slope), p_value = as.numeric(p_value)))


  return(tibble::tibble(
    slope   = fit$estimate["slope"],
    p.value = fit$p.value,
    tau     = fit$estimate["tau"]
  ))
}