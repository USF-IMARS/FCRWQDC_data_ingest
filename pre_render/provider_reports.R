# =====================================================================
# === setup
# =====================================================================
# Proceed if rendering the whole project, exit otherwise
if (!nzchar(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"))) {
  quit()
}
if (!nzchar(system.file(package = "librarian"))) {
  install.packages("librarian")
}
librarian::shelf(
  dplyr,
  glue,
  here,
  whisker
)
# =====================================================================
# === basic setup
# =====================================================================
# creates a report template .qmd for each
REPORT_NAME <- "provider_reports"
REPORT_TEMPLATE <- here(glue("{REPORT_NAME}/{REPORT_NAME}_template.qmd"))
REPORTS_DIR <- here(glue("{REPORT_NAME}/{REPORT_NAME}"))

# create the template
# TODO: do this using `double_param_the_yaml()`
templ <- readLines(REPORT_TEMPLATE)
templ <- gsub(
  "21FLKNMS", "{{org_id}}", templ
)

dir.create(REPORTS_DIR, showWarnings=FALSE)

# =====================================================================
# === iterate through the data structure
# =====================================================================
# Set the root directory where the folders are located
source(here("R/getData.R"))
# for each file in /data directory:
data_files <- list.files(here("data"), pattern = "_WIN_WAVES_OTIS_.*\\.txt$", full.names = FALSE)
for (file in data_files) {
  # Extract org_id from filename using regex pattern
  org_id <- gsub("^_WIN_WAVES_OTIS_(.+)\\.txt$", "\\1", file)
  params = list(
    org_id = org_id
  )
  print(glue("=== creating template for '{org_id}' ==="))
  writeLines(
    whisker.render(templ, params),
    file.path(REPORTS_DIR, glue("{org_id}.qmd"))
  )
}
