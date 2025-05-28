getAllData <- function(){
    
    library(dplyr)
    library(here)
    source(here("R/getListOfPrograms.R"))
    programs <- getListOfPrograms()
    source(here("R/getData.R"))

    # Create a list to store all program dataframes
    all_data_list <- list()

    for (program in programs) {
        cat(paste0("Processing program: ", program, "\n"))
        data <- getData(program)
        # add program name to dataframe
        data$program <- program
        # add to list
        all_data_list[[program]] <- data
    }

    # Combine all dataframes, preserving all columns and filling missing values with NA
    df <- dplyr::bind_rows(all_data_list)
    cat(paste0("Combined dataframe has ", nrow(df), " rows and ", ncol(df), " columns\n"))
    return(df)
}