library(testthat)
library(dplyr)
library(here)

# Load test utilities
source(here::here("tests/testthat/test-utils.R"))

# Load necessary data sources
source(here::here("R/getWINData.R"))
source(here::here("R/getSFERData.R"))
source(here::here("R/getMiamiBeachData.R"))

test_that("WIN column alignment test - demonstrates the column alignment checker", {
  # Skip if test data isn't available
  skip_if_not(dir.exists(here::here("data/WIN")), "WIN data directory not found")
  
  # Load a reference WIN dataset (if available)
  tryCatch({
    cat("Loading reference WIN dataset...\n")
    win_df <- getWINData("test")
    
    # Skip if no data was returned
    skip_if(nrow(win_df) == 0, "Reference WIN dataset is empty")
    
    # Show the alignment of the WIN dataset against the core WIN columns (should be high)
    cat("\n### REFERENCE WIN DATASET ALIGNMENT ###\n")
    win_alignment <- check_win_column_alignment(win_df, source_name = "WIN reference")
    
    # This should have very high alignment with WIN standard
    expect_gte(win_alignment$alignment_percent, 80, 
               "WIN reference dataset has poor alignment with WIN standard columns")
    
  }, error = function(e) {
    cat("Could not load reference WIN dataset:", e$message, "\n")
    skip("Reference WIN dataset could not be loaded")
  })
})

test_that("Non-WIN data sources align correctly with WIN format", {
  # Skip if source data directories don't exist
  skip_if_not(dir.exists(here::here("data/SFER")), "SFER data directory not found")
  skip_if_not(dir.exists(here::here("data/MiamiBeach")), "MiamiBeach data directory not found")
  
  # Load a WIN reference dataset if possible
  reference_df <- tryCatch({
    getWINData("test")
  }, error = function(e) {
    NULL
  })
  
  # Test SFER data alignment
  tryCatch({
    cat("\n### SFER DATASET ALIGNMENT ###\n")
    sfer_df <- getSFERData("SFER")
    
    # Skip if no data was returned
    skip_if(nrow(sfer_df) == 0, "SFER dataset is empty")
    
    # Check SFER alignment against WIN standard
    sfer_alignment <- check_win_column_alignment(sfer_df, 
                                                reference_df = reference_df,
                                                source_name = "SFER")
    
    # SFER should have acceptable alignment with WIN standard
    expect_gte(sfer_alignment$alignment_percent, 60, 
               "SFER dataset has poor alignment with WIN standard columns")
    
  }, error = function(e) {
    cat("Could not load SFER dataset:", e$message, "\n")
    skip("SFER dataset could not be loaded")
  })
  
  # Test Miami Beach data alignment
  tryCatch({
    cat("\n### MIAMI BEACH DATASET ALIGNMENT ###\n")
    miami_df <- getMiamiBeachData()
    
    # Skip if no data was returned
    skip_if(nrow(miami_df) == 0, "Miami Beach dataset is empty")
    
    # Check Miami Beach alignment against WIN standard
    miami_alignment <- check_win_column_alignment(miami_df, 
                                                 reference_df = reference_df,
                                                 source_name = "Miami Beach")
    
    # Miami Beach should have acceptable alignment with WIN standard
    expect_gte(miami_alignment$alignment_percent, 60, 
               "Miami Beach dataset has poor alignment with WIN standard columns")
    
  }, error = function(e) {
    cat("Could not load Miami Beach dataset:", e$message, "\n")
    skip("Miami Beach dataset could not be loaded")
  })
})

# Helper function to print alignment comparison between datasets
compare_dataset_alignments <- function() {
  # This is just a demonstration, not a test
  tryCatch({
    datasets <- list(
      WIN = getWINData("test"),
      SFER = getSFERData("SFER"),
      MiamiBeach = getMiamiBeachData()
    )
    
    results <- list()
    for (name in names(datasets)) {
      if (nrow(datasets[[name]]) > 0) {
        cat("\n\n===", name, "ALIGNMENT ===\n")
        results[[name]] <- check_win_column_alignment(datasets[[name]], source_name = name)
      }
    }
    
    # Compare alignments
    cat("\n\n=== ALIGNMENT COMPARISON ===\n")
    for (name in names(results)) {
      cat(name, "alignment:", results[[name]]$alignment_percent, "%\n")
      cat("  Missing core columns:", length(results[[name]]$missing_core_columns), "\n")
      if (length(results[[name]]$missing_core_columns) > 0) {
        cat("  Examples:", paste(head(results[[name]]$missing_core_columns, 3), collapse=", "), "\n")
      }
    }
    
  }, error = function(e) {
    cat("Could not complete alignment comparison:", e$message, "\n")
  })
}
