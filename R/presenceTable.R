# Required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

getPresenceHeatmapTable <- function(df){
  # 1. Ensure the result values are numeric
  df$DEP.Result.Value.Number <- as.numeric(df$DEP.Result.Value.Number)
  
  # 2. Build a percentage table per Location × Analyte
  percentage_table <- df %>%
    dplyr::group_by(Monitoring.Location.ID, DEP.Analyte.Name) %>%
    dplyr::summarise(
      # Calculate the percentage of valid (non-NA) values
      total_observations = dplyr::n(),
      valid_observations = sum(!is.na(DEP.Result.Value.Number)),
      percentage = ifelse(.data$total_observations > 0, 
                           100 * .data$valid_observations / .data$total_observations, 0),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from  = "DEP.Analyte.Name",
      values_from = "percentage",
      values_fill = list(percentage = 0)
    )
  
  # 3. Pivot back to long for ggplot
  heat_long <- percentage_table %>%
    tidyr::pivot_longer(
      cols      = -Monitoring.Location.ID,
      names_to  = "DEP.Analyte.Name",
      values_to = "percentage"
    )
  return(heat_long)
}

plotHeatmap <- function(heat_long){
  # 4. Draw the heatmap with a white→steelblue gradient for percentages
  ggplot2::ggplot(heat_long,
         ggplot2::aes(x = .data$DEP.Analyte.Name,
             y = .data$Monitoring.Location.ID,
             fill = .data$percentage)) +
    ggplot2::geom_tile(color = "grey90") +
    ggplot2::scale_fill_gradient(
      low    = "white",
      high   = "steelblue",
      limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0%", "25%", "50%", "75%", "100%")
    ) +
    ggplot2::labs(
      x    = "DEP Analyte Name",
      y    = "Monitoring Location ID",
      fill = "% of Valid\nValues"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid   = ggplot2::element_blank()
    )
}