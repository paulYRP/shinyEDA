# Purpose: Define package loading and app-wide constants.
# Arguments: None.
# Returns: Invisibly returns TRUE after loading required packages.
loadAppPackages <- function() {
  pkgs <- c(
    "shiny", "bslib", "DT", "ggplot2", "dplyr", "tidyr",
    "openxlsx", "jsonlite", "htmltools", "scales", "rpart"
  )

  missingPkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missingPkgs) > 0) {
    stop("Missing required packages: ", paste(missingPkgs, collapse = ", "))
  }

  invisible(TRUE)
}

# Purpose: Return the app Bootstrap theme for light or dark display.
# Arguments: Theme mode, either "light" or "dark".
# Returns: A bslib theme object.
appTheme <- function(mode = "light") {
  mode <- match.arg(mode, c("light", "dark"))

  if (identical(mode, "dark")) {
    return(bslib::bs_theme(version = 5, bootswatch = "darkly", primary = "#72c6ca"))
  }

  bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#2f6f73")
}

# Purpose: Return default abnormal-value tokens used during import.
# Arguments: None.
# Returns: Character vector of abnormal-value tokens.
defaultAbnormalValues <- function() {
  c(
    "", "na", "n/a", "nan", "null", "none", "unknown", "unk",
    "u", "other", "others", "--", "-", ".", "999", "9999",
    "-999", "-9999"
  )
}

# Purpose: Return default risk outcome columns used by the workflow.
# Arguments: None.
# Returns: Character vector of risk outcome variable names.
defaultRiskVars <- function() {
  c("RiskStressVal", "RiskDiabetes2Val", "RiskCardiovascularVal")
}

# Purpose: Return default question-style item names.
# Arguments: None.
# Returns: Character vector q1 to q21.
defaultQuestionVars <- function() {
  paste0("q", 1:21)
}

# Purpose: Detect question-style columns in an uploaded dataset.
# Arguments: Data frame.
# Returns: Character vector of detected question variable names.
detectQuestionVars <- function(data) {
  vars <- names(data)
  questionVars <- vars[grepl("^q[A-Za-z]*[0-9]+$", vars)]
  if (length(questionVars) == 0) {
    return(defaultQuestionVars())
  }

  prefixes <- sub("[0-9]+$", "", questionVars)
  suffixes <- as.integer(sub("^.*?([0-9]+)$", "\\1", questionVars))
  questionVars[order(prefixes, suffixes)]
}

# Purpose: Suggest columns that should be converted to numeric.
# Arguments: Data frame.
# Returns: Character vector of EDA-default numeric conversion variable names.
suggestNumericVars <- function(data) {
  intersect(c("RiskDiabetes2Val", "RiskCardiovascularVal"), names(data))
}

# Purpose: Suggest variables that should be converted to integer.
# Arguments: Data frame.
# Returns: Character vector of EDA-default integer conversion variable names.
suggestIntegerVars <- function(data) {
  vars <- names(data)
  vars[tolower(vars) == "age"]
}

# Purpose: Suggest variables that should be converted to factor.
# Arguments: Data frame.
# Returns: Character vector of EDA-default factor conversion variable names.
suggestFactorVars <- function(data) {
  intersect(c("gender", "healthtype", "biotype"), names(data))
}
