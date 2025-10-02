# Required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

#' Generates a data table for the presence heatmap.
#'
#' This function takes a dataframe and calculates the percentage of non-NA
#' observations for each combination of 'site' and a specified analyte column.
#'
#' @param df The input dataframe. Must contain 'site', 'value', and the analyte column.
#' @param analyte_col A character string specifying the name of the column to be
#'   treated as the 'analyte'. Defaults to "analyte".
#' @param site_col A character string specifying the name of the column to be
#'   treated as the 'site'. Defaults to "site".
#' @param value_col A character string specifying the name of the column containing
#'   the values to be checked for presence. Defaults to "value".
#' @return A long-format tibble with columns for site, the analyte name, and percentage.
getPresenceHeatmapTable <- function(df, analyte_col = "analyte", site_col = "site", value_col = "value") {
  # --- Handle Missing Grouping Values ---
  # Instead of filtering, replace NA or empty strings in grouping columns
  # with a placeholder. This prevents errors in pivot functions while keeping
  # all rows for an accurate percentage calculation.
  df_handled <- df %>%
    mutate(
      !!site_col := if_else(is.na(.data[[site_col]]), "(Missing Site)", as.character(.data[[site_col]])),
      !!analyte_col := if_else(is.na(.data[[analyte_col]]) | .data[[analyte_col]] == "", "(Missing Analyte)", as.character(.data[[analyte_col]]))
    )
  
  # Build a percentage table per Location × Analyte
  percentage_table <- df_handled %>%
    # Group by the user-specified site and analyte columns
    # .data[[...]] is used for tidy evaluation with a string variable
    dplyr::group_by(.data[[site_col]], .data[[analyte_col]]) %>%
    dplyr::summarise(
      # Calculate the percentage of valid (non-NA) values
      total_observations = dplyr::n(),
      valid_observations = sum(!is.na(.data[[value_col]])),
      percentage = ifelse(total_observations > 0,
                          100 * valid_observations / total_observations, 0),
      .groups = "drop"
    ) %>%
    # Pivot wider using the specified analyte column
    # all_of() is used to safely select the column from the character variable
    tidyr::pivot_wider(
      names_from  = all_of(analyte_col),
      values_from = "percentage",
      values_fill = list(percentage = 0)
    )
  
  # Pivot back to long format for ggplot
  # The new column containing analyte names is named based on the analyte_col variable
  heat_long <- percentage_table %>%
    tidyr::pivot_longer(
      cols      = -all_of(site_col),
      names_to  = analyte_col,
      values_to = "percentage"
    )
  return(heat_long)
}


#' Plots the presence heatmap.
#'
#' This function takes the long-format data from getPresenceHeatmapTable
#' and creates a ggplot heatmap.
#'
#' @param heat_long The long-format dataframe from getPresenceHeatmapTable.
#' @param analyte_col A character string specifying the name of the column that
#'   contains the analyte names. Defaults to "analyte".
#' @param site_col A character string specifying the name of the column that
#'   contains the site names. Defaults to "site".
#' @return A ggplot object representing the heatmap.
plotHeatmap <- function(heat_long, analyte_col = "analyte", site_col = "site") {
  # Generate nice title-case labels from the column names
  x_axis_label <- tools::toTitleCase(gsub("[._]", " ", analyte_col))
  y_axis_label <- tools::toTitleCase(gsub("[._]", " ", site_col))
  
  
  # Draw the heatmap with a white→steelblue gradient for percentages
  ggplot2::ggplot(heat_long,
                  # Use .data[[...]] to reference columns in aes()
                  ggplot2::aes(x = .data[[analyte_col]],
                               y = .data[[site_col]],
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
      x    = x_axis_label,
      y    = y_axis_label,
      fill = "% of Valid\nValues"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid   = ggplot2::element_blank()
    )
}

# --- Example Usage ---
# Create a sample dataframe with problematic values
# sample_data <- data.frame(
#   location = rep(c("Area 1", "Area 2", NA), each = 20),
#   chemical = rep(c("Lead", "Arsenic", "", "Cadmium"), times = 15),
#   measurement = sample(c(1:10, NA), 60, replace = TRUE),
#   stringsAsFactors = FALSE
# )
#
# # 1. Generate the table using the new column names
# # The function will now handle the NA in 'location' and the "" in 'chemical'
# heatmap_data <- getPresenceHeatmapTable(
#   sample_data,
#   analyte_col = "chemical",
#   site_col = "location",
#   value_col = "measurement"
# )
#
# # 2. Plot the data, specifying the new column names
# # The plot will contain rows/columns for "(Missing Site)" and "(Missing Analyte)"
# plotHeatmap(heatmap_data, analyte_col = "chemical", site_col = "location")

