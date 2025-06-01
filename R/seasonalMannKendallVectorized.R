# 1) Make sure the Kendall package is installed
if (!requireNamespace("Kendall", quietly = TRUE)) {
  install.packages("Kendall")
}

# 2) Define the seasonalMannKendallVectorized function
seasonalMannKendallVectorized <- function(id_vec, date_vec, value_vec) {
  # id_vec, date_vec, and value_vec are three parallel vectors. Within each group,
  # id_vec is constant (but we don't actually use it here other than to match signature).
  #
  # Steps:
  #  1. Remove any rows where date or value is NA.
  #  2. Ensure data are sorted by date.
  #  3. Extract month (1–12) from each date.
  #  4. Call Kendall::SeasonalMannKendall() on (values, months).
  #  5. Return the Kendall tau. If too few observations remain, return NA.
  
  # (a) Drop rows with missing dates or missing values
  ok <- !is.na(date_vec) & !is.na(value_vec)
  dates  <- as.Date(date_vec[ok])
  values <-     value_vec[ok]
  
  # (b) If fewer than 3 data points, bail out with NA
  if (length(values) < 3) {
    return(NA_real_)
  }
  
  # (c) Sort by date
  ord <- order(dates)
  dates  <- dates[ord]
  values <- values[ord]
  
  # (d) Extract month (1 through 12) from each date
  months <- as.integer(format(dates, "%m"))
  
  # (e) Run Seasonal Mann–Kendall
  test_res <- Kendall::SeasonalMannKendall(values, months)
  
  # (f) Return Kendall’s tau (you could also return test_res$sl if you prefer p-value)
  return(test_res$tau)
}