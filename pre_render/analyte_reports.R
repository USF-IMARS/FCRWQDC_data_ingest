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
REPORT_NAME <- "analyte_reports"
REPORT_TEMPLATE <- here(glue("{REPORT_NAME}/{REPORT_NAME}_template.qmd"))
REPORTS_DIR <- here(glue("{REPORT_NAME}/{REPORT_NAME}"))

# create the template
# TODO: do this using `double_param_the_yaml()`
templ <- readLines(REPORT_TEMPLATE)
templ <- gsub(
  "Ammonia (N)", "{{analyte}}", templ
)

dir.create(REPORTS_DIR, showWarnings=FALSE)

# =====================================================================
# === iterate through the data structure
# =====================================================================
# function to create template
create_template <- function(analyte) {
  params <- list(
    analyte = analyte
  )
  print(glue("=== creating template for '{analyte}' ==="))
  writeLines(
    whisker.render(templ, params),
    file.path(REPORTS_DIR, glue("{analyte}.qmd"))
  )
}

# get list of analytes
source(here("R/getListOfAnalytes.R"))
analytes <- getListOfAnalytes()

for (analyte in analytes) {
  create_template(analyte)
}
