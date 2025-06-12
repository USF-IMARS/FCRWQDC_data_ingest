# Required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

getPresenceHeatmapTable <- function(df){  
  # Build a percentage table per Location × Analyte
  percentage_table <- df %>%
    dplyr::group_by(site, analyte) %>%
    dplyr::summarise(
      # Calculate the percentage of valid (non-NA) values
      total_observations = dplyr::n(),
      valid_observations = sum(!is.na(value)),
      percentage = ifelse(.data$total_observations > 0, 
                           100 * .data$valid_observations / .data$total_observations, 0),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from  = "analyte",
      values_from = "percentage",
      values_fill = list(percentage = 0)
    )
  
  # Pivot back to long for ggplot
  heat_long <- percentage_table %>%
    tidyr::pivot_longer(
      cols      = -site,
      names_to  = "analyte",
      values_to = "percentage"
    )
  return(heat_long)
}

plotHeatmap <- function(heat_long){
  # 4. Draw the heatmap with a white→steelblue gradient for percentages
  ggplot2::ggplot(heat_long,
         ggplot2::aes(x = .data$analyte,
             y = .data$site,
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
      x    = "Analyte",
      y    = "Site",
      fill = "% of Valid\nValues"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid   = ggplot2::element_blank()
    )
}