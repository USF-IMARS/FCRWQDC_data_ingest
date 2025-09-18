# Loads data from all source files, aligns them, and writes one big csv file.

# Proceed if rendering the whole project, exit otherwise
if (!nzchar(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"))) {
  quit()
}
source(here::here("R/getAllData.R"))
df <- getAllData()
write.csv(df, here::here("data", "exports", "allDataRaw.csv"))
