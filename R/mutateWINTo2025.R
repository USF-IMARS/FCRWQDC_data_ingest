mutateWINTo2025 <- function(df, .keep = "none"){
  # map WIN column names to pre-2025 reporting columns
  df %>%
  mutate(
    source = program,
    site = Monitoring.Location.ID,
    datetime = Activity.Start.Date.Time,
    analyte = DEP.Analyte.Name,
    value = DEP.Result.Value.Number,
    units = DEP.Result.Unit,
    latitude = Org.Decimal.Latitude,
    longitude = Org.Decimal.Longitude,
    sample_depth = Activity.Depth,
    .keep = .keep)
}