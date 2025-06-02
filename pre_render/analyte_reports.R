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
# Modify the analyte_reports.R script:

# Add debug output to verify template path
# cat("Template path:", REPORT_TEMPLATE, "\n")
# cat("Template exists:", file.exists(REPORT_TEMPLATE), "\n")

# Read the template and verify its content before and after gsub
templ <- readLines(REPORT_TEMPLATE)
# cat("First few lines before gsub:\n", paste(head(templ), collapse="\n"), "\n")

templ <- gsub(
  "Salinity", "{{analyte}}", templ
)

# cat("First few lines after gsub:\n", paste(head(templ), collapse="\n"), "\n")

# In the create_template function, add debug output
create_template <- function(analyte) {
  params <- list(
    analyte = analyte
  )
  print(glue("=== creating template for '{analyte}' ==="))
  
  # Debug: print parameters being used
  # cat("Template parameters:", names(params), "=", unlist(params), "\n")
  
  # Render template and output to file
  rendered <- whisker.render(templ, params)
  
  # Debug: print first few lines of rendered output
  # cat("First few lines of rendered output:\n", paste(head(strsplit(rendered, "\n")[[1]]), collapse="\n"), "\n")
  
  writeLines(
    rendered,
    file.path(REPORTS_DIR, glue("{analyte}.qmd"))
  )
}

dir.create(REPORTS_DIR, showWarnings=FALSE)

# =====================================================================
# === iterate through the data structure
# =====================================================================
# get list of analytes
source(here("R/getListOfAnalytes.R"))
analytes <- getListOfAnalytes()

for (analyte in analytes) {
  create_template(analyte)
}