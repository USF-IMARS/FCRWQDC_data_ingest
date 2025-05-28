getPresenceHeatmapTable <- function(df){
  # 1. Ensure the result values are numeric
  df$DEP.Result.Value.Number <- as.numeric(df$DEP.Result.Value.Number)
  
  # 2. Build a wide “presence” table per Location × Analyte
  presence_table <- df %>%
    group_by(Monitoring.Location.ID, DEP.Analyte.Name) %>%
    summarise(
      # 1 if any non-NA exists, else 0
      presence = ifelse(any(!is.na(DEP.Result.Value.Number)), 1L, 0L),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from  = DEP.Analyte.Name,
      values_from = presence,
      values_fill = list(presence = 0L)
    )
  
  # 3. Pivot back to long for ggplot
  heat_long <- presence_table %>%
    pivot_longer(
      cols      = -Monitoring.Location.ID,
      names_to  = "DEP.Analyte.Name",
      values_to = "presence"
    )
  return(heat_long)
}

plotHeatmap <- function(heat_long){
  # 4. Draw the heatmap with a white→steelblue gradient
  ggplot(heat_long,
         aes(x = DEP.Analyte.Name,
             y = Monitoring.Location.ID,
             fill = presence)) +
    geom_tile(color = "grey90") +
    scale_fill_gradient(
      low    = "white",
      high   = "steelblue",
      limits = c(0, 1),
      breaks = c(0, 1),
      labels = c("0 (none)", "1 (some)")
    ) +
    labs(
      x    = "DEP Analyte Name",
      y    = "Monitoring Location ID",
      fill = "Presence\n(0/1)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1),
      panel.grid   = element_blank()
    )
}