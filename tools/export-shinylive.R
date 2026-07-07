# Purpose: Export this Shiny app to a static shinylive site.
# Arguments: None. Run from the app repository root.
# Returns: Creates or replaces the shinylive/ directory.
if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop(
    "Package 'shinylive' is required to export the static site. ",
    "Install it with install.packages('shinylive') and rerun this script."
  )
}

appDir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
siteDir <- file.path(appDir, "shinylive")

if (dir.exists(siteDir)) {
  unlink(siteDir, recursive = TRUE)
}

shinylive::export(appDir, siteDir)
message("shinylive site written to: ", siteDir)
