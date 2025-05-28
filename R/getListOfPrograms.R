getListOfPrograms <- function(){
    # hardcode programs that don't have a WIN file
    programs <- c("SFER", "MiamiBeach")

    # for each WIN data file in /data directory:
    data_files <- list.files(here("data/WIN"), pattern = "_WIN_WAVES_OTIS_.*\\.txt$", full.names = FALSE)
    for (file in data_files) {
        # Extract org_id from filename using regex pattern
        org_id <- gsub("^_WIN_WAVES_OTIS_(.+)\\.txt$", "\\1", file)
        # add org_id to list of programs
        programs <- c(programs, org_id)
    }
    return(programs)
}