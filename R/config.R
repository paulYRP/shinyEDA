# Purpose: Define package loading and app-wide constants.
# Arguments: None.
# Returns: Invisibly returns TRUE after loading required packages.
loadAppPackages <- function() {
  pkgs <- c(
    "shiny", "bslib", "DT", "ggplot2", "dplyr", "tidyr",
    "openxlsx", "jsonlite", "htmltools", "scales", "rpart",
    "MASS", "lme4", "lmerTest", "performance"
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
    return(bslib::bs_theme(version = 5, bootswatch = "darkly", primary = "#72c6ca", base_font = bslib::font_google("Roboto")))
  }

  bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#2f6f73", base_font = bslib::font_google("Roboto"))
}

# Purpose: Return raster resolution for in-app plot previews.
# Arguments: None.
# Returns: Resolution in pixels per inch for shiny::renderPlot().
appPlotResolution <- function() {
  144
}

# Purpose: Return maximum variables displayed in compact plot previews.
# Arguments: None.
# Returns: Integer preview limit.
appPreviewVariableLimit <- function() {
  25L
}

# Purpose: Return maximum matrix variables displayed in compact heatmap previews.
# Arguments: None.
# Returns: Integer preview limit.
appPreviewMatrixLimit <- function() {
  12L
}

# Purpose: Return maximum facets displayed in compact plot previews.
# Arguments: None.
# Returns: Integer preview limit.
appPreviewFacetLimit <- function() {
  6L
}

# Purpose: Check whether a value is one non-missing, non-empty string.
# Arguments: Any R object.
# Returns: TRUE when the first value is usable as a scalar input string.
hasScalarText <- function(x) {
  !is.null(x) && length(x) > 0 && !is.na(x[1]) && nzchar(as.character(x[1]))
}

# Purpose: Return the first scalar string or a default value.
# Arguments: Any R object and fallback string.
# Returns: Character scalar.
scalarText <- function(x, default = "") {
  if (hasScalarText(x)) {
    return(as.character(x[1]))
  }

  default
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

# Purpose: Return additional common missing-value labels for optional selection.
# Arguments: None.
# Returns: Character vector of common missing-value tokens.
commonAbnormalValues <- function() {
  c(
    defaultAbnormalValues(),
    "missing", "not available", "not applicable", "n.a.", "NA",
    "N/A", "prefer not to answer", "refused", "don't know", "dont know"
  )
}

# Purpose: Encode abnormal-value tokens for safe use in selectize choices.
# Arguments: Character vector of tokens.
# Returns: Encoded character vector.
encodeAbnormalValues <- function(values) {
  values <- as.character(values)
  values[is.na(values)] <- "__NA_VALUE__"
  values[values == ""] <- "__EMPTY_STRING__"
  values
}

# Purpose: Decode abnormal-value tokens selected from the UI.
# Arguments: Encoded character vector.
# Returns: Decoded character vector.
decodeAbnormalValues <- function(values) {
  if (is.null(values) || length(values) == 0) {
    return(character(0))
  }

  values <- as.character(values)
  values[values == "__EMPTY_STRING__"] <- ""
  values[values == "__NA_VALUE__"] <- NA_character_
  values
}

# Purpose: Build labels for abnormal-value dropdown choices.
# Arguments: Character vector of abnormal-value tokens.
# Returns: Named character vector for selectize choices.
abnormalValueChoices <- function(data = NULL) {
  values <- unique(c(commonAbnormalValues(), detectAbnormalTokens(data)))
  values <- values[!is.na(values)]
  labels <- ifelse(values == "", "<empty string>", values)
  stats::setNames(encodeAbnormalValues(values), labels)
}

# Purpose: Detect observed values that match common missing-value labels.
# Arguments: Data frame or NULL.
# Returns: Character vector of matching observed tokens.
detectAbnormalTokens <- function(data = NULL) {
  if (is.null(data) || ncol(data) == 0) {
    return(character(0))
  }

  cols <- names(data)[vapply(data, function(x) is.character(x) || is.factor(x), logical(1))]
  if (length(cols) == 0) {
    return(character(0))
  }

  values <- unique(unlist(lapply(data[cols], as.character), use.names = FALSE))
  values <- trimws(values)
  values <- values[!is.na(values) & nchar(values) <= 30]
  keys <- tolower(trimws(commonAbnormalValues()))
  values[tolower(values) %in% keys]
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

# Purpose: Detect response-item column groups such as q1-q21, qa*, qb*, qc* and qd*.
# Arguments: Data frame.
# Returns: Named character vector of response-set choices.
responseSetChoices <- function(data) {
  vars <- names(data)
  responseVars <- vars[grepl("^q[A-Za-z]*[0-9]+$", vars)]
  choices <- c("Custom" = "custom")

  if (length(responseVars) == 0) {
    return(choices)
  }

  prefixes <- unique(sub("[0-9]+$", "", responseVars))
  prefixChoices <- stats::setNames(prefixes, paste0(prefixes, "*"))

  if (all(defaultQuestionVars() %in% vars)) {
    return(c("q1-q21" = "q21Default", prefixChoices, choices))
  }

  c(prefixChoices, choices)
}

# Purpose: Return response columns for a selected response-set choice.
# Arguments: Data frame and selected response-set key.
# Returns: Character vector of matching response columns.
responseSetVars <- function(data, responseSet) {
  vars <- names(data)
  if (identical(responseSet, "q21Default")) {
    return(intersect(defaultQuestionVars(), vars))
  }

  if (!is.null(responseSet) && nzchar(responseSet) && !identical(responseSet, "custom")) {
    responseVars <- vars[grepl("^q[A-Za-z]*[0-9]+$", vars)]
    responseVars <- responseVars[sub("[0-9]+$", "", responseVars) == responseSet]
    suffixes <- as.integer(sub("^.*?([0-9]+)$", "\\1", responseVars))
    return(responseVars[order(suffixes)])
  }

  intersect(detectQuestionVars(data), vars)
}

# Purpose: Choose the default response-item set.
# Arguments: Data frame.
# Returns: Response-set key.
defaultResponseSet <- function(data) {
  choices <- responseSetChoices(data)
  if ("q21Default" %in% choices) {
    return("q21Default")
  }
  unname(choices[1])
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
