# Purpose: Recode common DASS-style response labels to integer scores.
# Arguments: A vector of response labels or numeric scores.
# Returns: Integer score vector where possible.
scoreD21Response <- function(x) {
  if (is.numeric(x) || is.integer(x)) {
    return(as.integer(x))
  }

  key <- tolower(trimws(as.character(x)))
  out <- rep(NA_integer_, length(key))
  out[key == "never"] <- 0L
  out[key == "rarely"] <- 1L
  out[key == "sometimes"] <- 2L
  out[key == "often"] <- 3L
  suppressWarnings(out[is.na(out)] <- as.integer(key[is.na(out)]))
  out
}

# Purpose: Add DASS21 stress, anxiety and depression scores when q1-q21 exist.
# Arguments: Data frame.
# Returns: Data frame with StressD21, AnxietyD21 and DepressionD21 when possible.
addD21Scores <- function(data) {
  qVars <- paste0("q", 1:21)
  if (!all(qVars %in% names(data))) {
    return(data)
  }

  out <- data
  out[qVars] <- lapply(out[qVars], scoreD21Response)
  out$StressD21 <- rowSums(out[c("q1", "q6", "q8", "q11", "q12", "q14", "q18")], na.rm = FALSE) * 2
  out$AnxietyD21 <- rowSums(out[c("q2", "q4", "q7", "q9", "q15", "q19", "q20")], na.rm = FALSE) * 2
  out$DepressionD21 <- rowSums(out[c("q3", "q5", "q10", "q13", "q16", "q17", "q21")], na.rm = FALSE) * 2
  out
}

# Purpose: Add BMI when height and weight are available.
# Arguments: Data frame.
# Returns: Data frame with BMI column when possible.
addBmiValue <- function(data) {
  if (!all(c("height", "weight") %in% names(data))) {
    return(data)
  }

  out <- data
  height <- suppressWarnings(as.numeric(out$height))
  weight <- suppressWarnings(as.numeric(out$weight))
  out$BMI <- weight / (height / 100)^2
  out
}

# Purpose: Return default range rules used by the original workflow when variables exist.
# Arguments: Data frame.
# Returns: Data frame with variable, min and max columns.
defaultRangeRules <- function(data) {
  rules <- data.frame(
    variable = c("BMI", "LBM", "Waist"),
    min = c(12, 0, 50),
    max = c(70, 120, 180),
    stringsAsFactors = FALSE
  )
  rules[rules$variable %in% names(data), , drop = FALSE]
}

# Purpose: Convert range rules to editable text.
# Arguments: Range-rule data frame.
# Returns: Character string, one rule per line.
formatRangeRules <- function(rules) {
  if (is.null(rules) || nrow(rules) == 0) {
    return("")
  }

  paste(paste(rules$variable, rules$min, rules$max, sep = ", "), collapse = "\n")
}

# Purpose: Parse editable range-rule text.
# Arguments: Text containing variable, minimum and maximum per line.
# Returns: Data frame with variable, min and max columns.
parseRangeRules <- function(text, data = NULL) {
  if (is.null(text) || !nzchar(trimws(text))) {
    return(data.frame(variable = character(), min = numeric(), max = numeric()))
  }

  lines <- trimws(unlist(strsplit(text, "\n", fixed = TRUE)))
  lines <- lines[nzchar(lines)]
  out <- lapply(lines, function(line) {
    parts <- trimws(unlist(strsplit(line, ",", fixed = TRUE)))
    if (length(parts) < 3) {
      return(NULL)
    }
    data.frame(
      variable = parts[1],
      min = suppressWarnings(as.numeric(parts[2])),
      max = suppressWarnings(as.numeric(parts[3])),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, out)
  if (is.null(out) || nrow(out) == 0) {
    return(data.frame(variable = character(), min = numeric(), max = numeric()))
  }

  out <- out[!is.na(out$min) & !is.na(out$max) & out$min <= out$max, , drop = FALSE]
  if (!is.null(data)) {
    out <- out[out$variable %in% names(data), , drop = FALSE]
  }
  row.names(out) <- NULL
  out
}

# Purpose: Apply selected value-range rules.
# Arguments: Data frame, range-rule table, enabled flag and action.
# Returns: Data frame after setting outside-range values to NA or removing rows.
applyRangeRules <- function(data, rangeRules = NULL, enabled = TRUE, action = "setNA") {
  if (!isTRUE(enabled)) {
    return(data)
  }

  out <- data
  if (is.null(rangeRules) || nrow(rangeRules) == 0) {
    return(out)
  }
  rangeRules <- rangeRules[rangeRules$variable %in% names(out), , drop = FALSE]
  if (nrow(rangeRules) == 0) {
    return(out)
  }

  removeRows <- rep(FALSE, nrow(out))
  for (i in seq_len(nrow(rangeRules))) {
    var <- rangeRules$variable[i]
    x <- suppressWarnings(as.numeric(out[[var]]))
    outside <- !is.na(x) & (x < rangeRules$min[i] | x > rangeRules$max[i])
    if (identical(action, "removeRows")) {
      removeRows <- removeRows | outside
    } else {
      out[[var]][outside] <- NA
    }
  }

  if (identical(action, "removeRows")) {
    out <- out[!removeRows, , drop = FALSE]
  }

  out
}

# Purpose: Return default columns removed before modelling.
# Arguments: Data frame.
# Returns: Character vector of columns present in the data.
defaultModelDropColumns <- function(data) {
  responseVars <- names(data)[grepl("^q[A-Za-z]*[0-9]+$", names(data))]
  intersect(
    c("RiskStress", "RiskDiabetes2", "RiskCardiovascular", "birthdate",
      "answered_at", "weight", "height", "verified", responseVars),
    names(data)
  )
}

# Purpose: Lump rare factor levels into OTHER using base R.
# Arguments: A vector and minimum count to keep a level.
# Returns: Factor vector.
lumpRareLevels <- function(x, minCount = 10) {
  f <- factor(x)
  tab <- table(f, useNA = "no")
  rare <- names(tab)[tab < minCount]
  y <- as.character(f)
  y[y %in% rare] <- "OTHER"
  factor(y)
}

# Purpose: Remove rows listed as outliers by a selected method.
# Arguments: Data frame, outlier result list, method name.
# Returns: Data frame with selected row positions removed.
removeOutlierRows <- function(data, outliers, method = "none") {
  if (is.null(outliers) || identical(method, "none")) {
    return(data)
  }
  if (is.null(outliers$detail) || nrow(outliers$detail) == 0) {
    return(data)
  }

  idx <- unique(na.omit(as.integer(outliers$detail$index[outliers$detail$method == method])))
  if (length(idx) == 0) {
    return(data)
  }

  if ("index" %in% names(data)) {
    return(data[!as.integer(data$index) %in% idx, , drop = FALSE])
  }

  data[!seq_len(nrow(data)) %in% idx, , drop = FALSE]
}

# Purpose: Return default risk outcome variables.
# Arguments: Data frame.
# Returns: Character vector of likely outcomes.
defaultAnalysisOutcomes <- function(data) {
  riskVars <- intersect(defaultRiskVars(), names(data))
  if (length(riskVars) > 0) {
    return(riskVars)
  }

  numericVars <- setdiff(getNumericVars(data), c("index", "user_id", "timepoint"))
  head(numericVars, 3)
}

# Purpose: Return default adjustment variables used in the R Markdown analyses.
# Arguments: Data frame.
# Returns: Character vector of variables present in the data.
defaultAdjustmentVars <- function(data) {
  intersect(c("BMI", "Waist", "LBM", "gender", "healthtype", "biotype", "Age"), names(data))
}

# Purpose: Return default DASS-style focal predictors.
# Arguments: Data frame.
# Returns: Character vector of variables present in the data.
defaultFocalPredictors <- function(data) {
  intersect(c("StressD21", "AnxietyD21", "DepressionD21"), names(data))
}

# Purpose: Return analysis dataset-version choices based on outlier handling.
# Arguments: None.
# Returns: Named character vector for selectInput choices.
analysisOutlierChoices <- function() {
  c(
    "No outlier removal" = "none",
    "Three-sigma removal" = "threeSigma",
    "Hampel removal" = "hampel",
    "Boxplot-rule removal" = "boxplotRule"
  )
}

# Purpose: Prepare cross-sectional model data following the app's GLM defaults.
# Arguments: Clean data and analysis-preparation options.
# Returns: Model-ready data frame.
prepareGlmData <- function(data, timeVar = "", timeLevel = "", userVar = "", outlierMethod = "none",
                           outliers = NULL, removeDuplicates = TRUE, duplicateCols = NULL,
                           rareLevelMin = 10, applyRangeFilters = TRUE, rangeRules = NULL,
                           rangeAction = "setNA", columnsToDrop = NULL) {
  out <- addD21Scores(data)
  out <- addBmiValue(out)

  if (nzchar(timeVar) && timeVar %in% names(out) && nzchar(timeLevel)) {
    out <- out[as.character(out[[timeVar]]) == timeLevel, , drop = FALSE]
  }

  out <- applyRangeRules(out, rangeRules, applyRangeFilters, rangeAction)

  duplicateCols <- intersect(duplicateCols, names(out))
  if (isTRUE(removeDuplicates) && length(duplicateCols) > 0) {
    out <- out[!duplicated(out[duplicateCols]), , drop = FALSE]
  }

  if ("index" %in% names(out)) {
    out$index <- as.integer(out$index)
  }
  if ("Age" %in% names(out)) {
    out$Age <- as.integer(out$Age)
  }
  if (nzchar(userVar) && userVar %in% names(out)) {
    out[[userVar]] <- factor(out[[userVar]])
  }
  if (nzchar(timeVar) && timeVar %in% names(out)) {
    out[[timeVar]] <- factor(out[[timeVar]])
  }
  for (v in intersect(c("gender", "healthtype"), names(out))) {
    out[[v]] <- factor(out[[v]])
  }
  if ("biotype" %in% names(out)) {
    out$biotype <- lumpRareLevels(out$biotype, rareLevelMin)
  }

  out <- removeOutlierRows(out, outliers, outlierMethod)

  dropVars <- intersect(columnsToDrop, names(out))

  out[, setdiff(names(out), dropVars), drop = FALSE]
}

# Purpose: Prepare all cross-sectional outlier-filtered dataset versions.
# Arguments: Clean data, time/user settings and optional outlier results.
# Returns: Named list of model-ready data frames.
prepareGlmDataVersions <- function(data, timeVar = "", timeLevel = "", userVar = "",
                                   outliers = NULL, removeDuplicates = TRUE, duplicateCols = NULL,
                                   rareLevelMin = 10, applyRangeFilters = TRUE, rangeRules = NULL,
                                   rangeAction = "setNA", columnsToDrop = NULL) {
  methods <- unname(analysisOutlierChoices())
  out <- lapply(methods, function(method) {
    prepareGlmData(
      data,
      timeVar = timeVar,
      timeLevel = timeLevel,
      userVar = userVar,
      outlierMethod = method,
      outliers = outliers,
      removeDuplicates = removeDuplicates,
      duplicateCols = duplicateCols,
      rareLevelMin = rareLevelMin,
      applyRangeFilters = applyRangeFilters,
      rangeRules = rangeRules,
      rangeAction = rangeAction,
      columnsToDrop = columnsToDrop
    )
  })
  names(out) <- methods
  out
}

# Purpose: Prepare longitudinal model data following the app's LME defaults.
# Arguments: Clean data and analysis-preparation options.
# Returns: Model-ready data frame.
prepareLmeData <- function(data, timeVar = "", timeLevels = NULL, userVar = "", outlierMethod = "none",
                           outliers = NULL, removeDuplicates = TRUE, duplicateCols = NULL,
                           rareLevelMin = 10, applyRangeFilters = TRUE, rangeRules = NULL,
                           rangeAction = "setNA", columnsToDrop = NULL) {
  out <- addD21Scores(data)
  out <- addBmiValue(out)

  if (nzchar(timeVar) && timeVar %in% names(out) && length(timeLevels) > 0) {
    out <- out[as.character(out[[timeVar]]) %in% as.character(timeLevels), , drop = FALSE]
    out[[timeVar]] <- factor(out[[timeVar]], levels = as.character(timeLevels))
  }

  out <- applyRangeRules(out, rangeRules, applyRangeFilters, rangeAction)

  duplicateCols <- intersect(duplicateCols, names(out))
  if (isTRUE(removeDuplicates) && length(duplicateCols) > 0) {
    out <- out[!duplicated(out[duplicateCols]), , drop = FALSE]
  }

  if ("index" %in% names(out)) {
    out$index <- as.integer(out$index)
  }
  if ("Age" %in% names(out)) {
    out$Age <- as.integer(out$Age)
  }
  if (nzchar(userVar) && userVar %in% names(out)) {
    out[[userVar]] <- factor(out[[userVar]])
  }
  for (v in intersect(c("gender", "healthtype"), names(out))) {
    out[[v]] <- factor(out[[v]])
  }
  if ("biotype" %in% names(out)) {
    out$biotype <- lumpRareLevels(out$biotype, rareLevelMin)
  }

  out <- removeOutlierRows(out, outliers, outlierMethod)

  dropVars <- intersect(columnsToDrop, names(out))

  out[, setdiff(names(out), dropVars), drop = FALSE]
}

# Purpose: Prepare all longitudinal outlier-filtered dataset versions.
# Arguments: Clean data, time/user settings and optional outlier results.
# Returns: Named list of model-ready data frames.
prepareLmeDataVersions <- function(data, timeVar = "", timeLevels = NULL, userVar = "",
                                   outliers = NULL, removeDuplicates = TRUE, duplicateCols = NULL,
                                   rareLevelMin = 10, applyRangeFilters = TRUE, rangeRules = NULL,
                                   rangeAction = "setNA", columnsToDrop = NULL) {
  methods <- unname(analysisOutlierChoices())
  out <- lapply(methods, function(method) {
    prepareLmeData(
      data,
      timeVar = timeVar,
      timeLevels = timeLevels,
      userVar = userVar,
      outlierMethod = method,
      outliers = outliers,
      removeDuplicates = removeDuplicates,
      duplicateCols = duplicateCols,
      rareLevelMin = rareLevelMin,
      applyRangeFilters = applyRangeFilters,
      rangeRules = rangeRules,
      rangeAction = rangeAction,
      columnsToDrop = columnsToDrop
    )
  })
  names(out) <- methods
  out
}
