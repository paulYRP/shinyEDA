# Purpose: Summarise missing values by variable.
# Arguments: Data frame.
# Returns: Data frame with missing counts and proportions.
missingSummary <- function(data) {
  n <- nrow(data)
  data.frame(
    variable = names(data),
    nMissing = vapply(data, function(x) sum(is.na(x)), integer(1)),
    propMissing = vapply(data, function(x) mean(is.na(x)), numeric(1)),
    rows = n,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# Purpose: Summarise categorical variables.
# Arguments: Data frame containing categorical columns.
# Returns: Long-format frequency table.
categoricalTable <- function(data) {
  if (ncol(data) == 0) {
    return(data.frame())
  }

  out <- lapply(names(data), function(v) {
    x <- data[[v]]
    if (is.character(x) || is.factor(x)) {
      x <- x[!is.na(x) & nzchar(trimws(as.character(x)))]
    } else {
      x <- x[!is.na(x)]
    }
    tab <- table(x)
    if (length(tab) == 0) {
      return(data.frame(
        variable = character(),
        n_levels = integer(),
        level = character(),
        count = integer(),
        proportion = numeric(),
        stringsAsFactors = FALSE
      ))
    }
    prop <- prop.table(tab)
    data.frame(
      variable = v,
      n_levels = length(tab),
      level = names(tab),
      count = as.integer(tab),
      proportion = as.numeric(prop),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}

# Purpose: Summarise integer-like variables.
# Arguments: Data frame containing integer-like variables.
# Returns: Data frame of basic discrete summaries.
discreteTable <- function(data) {
  if (ncol(data) == 0) {
    return(data.frame())
  }

  out <- data.frame(
    variable = names(data),
    min = vapply(data, function(x) min(x, na.rm = TRUE), numeric(1)),
    max = vapply(data, function(x) max(x, na.rm = TRUE), numeric(1)),
    mean = vapply(data, function(x) mean(x, na.rm = TRUE), numeric(1)),
    variance = vapply(data, function(x) var(x, na.rm = TRUE), numeric(1)),
    row.names = NULL
  )

  out$variance_mean <- ifelse(out$mean == 0, NA_real_, out$variance / out$mean)
  out[-1] <- lapply(out[-1], round, 2)
  out
}

# Purpose: Summarise continuous numeric distributions.
# Arguments: Data frame containing numeric variables.
# Returns: Data frame of quantiles and distribution diagnostics.
distributionTable <- function(data) {
  if (ncol(data) == 0) {
    return(data.frame())
  }

  out <- lapply(names(data), function(v) {
    x <- data[[v]]
    x <- x[is.finite(x)]
    if (length(x) == 0) {
      return(data.frame(variable = v))
    }

    meanVal <- mean(x)
    sdVal <- sd(x)
    q <- quantile(x, probs = c(0, .01, .05, .25, .5, .75, .95, .99, 1), names = FALSE)
    skewVal <- if (is.na(sdVal) || sdVal == 0) NA_real_ else mean((x - meanVal)^3) / sdVal^3

    data.frame(
      variable = v,
      n = length(x),
      min = q[1],
      p01 = q[2],
      p05 = q[3],
      q1 = q[4],
      median = q[5],
      mean = meanVal,
      q3 = q[6],
      p95 = q[7],
      p99 = q[8],
      max = q[9],
      sd = sdVal,
      iqr = IQR(x),
      range = diff(range(x)),
      mean_median_diff = meanVal - median(x),
      skewness = skewVal,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, out)
  numCols <- vapply(out, is.numeric, logical(1))
  out[numCols] <- lapply(out[numCols], round, 3)
  out
}

# Purpose: Summarise a binary variable by a target level.
# Arguments: Vector, target level and confidence level.
# Returns: One-row data frame with count and add-two adjusted CI.
binarySummary <- function(x, level, confLevel = 0.95) {
  n <- sum(!is.na(x))
  if (n == 0) {
    return(data.frame(
      level = level,
      count = 0,
      otherCount = 0,
      estimate = NA_real_,
      loCI = NA_real_,
      upCI = NA_real_
    ))
  }

  z <- stats::qnorm(1 - (1 - confLevel) / 2)
  count <- sum(x == level, na.rm = TRUE)
  adjN <- n + z^2
  adjProp <- (count + z^2 / 2) / adjN
  se <- sqrt(adjProp * (1 - adjProp) / adjN)

  data.frame(
    level = level,
    count = count,
    otherCount = n - count,
    estimate = adjProp,
    loCI = max(0, adjProp - z * se),
    upCI = min(1, adjProp + z * se)
  )
}

# Purpose: Summarise a binary variable across groups.
# Arguments: Data frame, binary variable, group variable, target level.
# Returns: Data frame with one row per group.
binaryByGroup <- function(data, binaryVar, groupVar, level, confLevel = 0.95) {
  groups <- unique(data[[groupVar]])
  groups <- groups[!is.na(groups)]

  out <- lapply(groups, function(g) {
    subsetData <- data[data[[groupVar]] == g, , drop = FALSE]
    tmp <- binarySummary(subsetData[[binaryVar]], level, confLevel)
    tmp$groupVar <- groupVar
    tmp$groupLevel <- as.character(g)
    tmp
  })

  out <- do.call(rbind, out)
  out[, c("groupVar", "groupLevel", "level", "count", "otherCount", "estimate", "loCI", "upCI")]
}

# Purpose: Compute three-sigma limits.
# Arguments: Numeric vector and SD multiplier.
# Returns: Named vector with lower and upper limits.
threeSigmaLimits <- function(x, t = 3) {
  c(down = mean(x, na.rm = TRUE) - t * sd(x, na.rm = TRUE),
    up = mean(x, na.rm = TRUE) + t * sd(x, na.rm = TRUE))
}

# Purpose: Compute Hampel limits.
# Arguments: Numeric vector and MAD multiplier.
# Returns: Named vector with lower and upper limits.
hampelLimits <- function(x, t = 3) {
  c(down = median(x, na.rm = TRUE) - t * mad(x, na.rm = TRUE),
    up = median(x, na.rm = TRUE) + t * mad(x, na.rm = TRUE))
}

# Purpose: Compute boxplot-rule limits.
# Arguments: Numeric vector and IQR multiplier.
# Returns: Named vector with lower and upper limits.
boxplotLimits <- function(x, t = 1.5) {
  q <- stats::quantile(x, probs = c(.25, .75), na.rm = TRUE, names = FALSE)
  c(down = q[1] - t * (q[2] - q[1]),
    up = q[2] + t * (q[2] - q[1]))
}

# Purpose: Summarise outliers for one variable and method.
# Arguments: Numeric vector, method name, lower limit and upper limit.
# Returns: List with summary and detailed rows.
outlierForMethod <- function(x, method, down, up) {
  idx <- which(x < down | x > up)
  minNomValues <- x[which(x >= down)]
  maxNomValues <- x[which(x <= up)]
  minNom <- if (length(minNomValues) == 0) NA_real_ else min(minNomValues)
  maxNom <- if (length(maxNomValues) == 0) NA_real_ else max(maxNomValues)
  nMiss <- sum(is.na(x))

  if (length(idx) == 0) {
    details <- data.frame(
      index = integer(),
      method = character(),
      nMiss = integer(),
      value = numeric(),
      type = character(),
      lowLim = numeric(),
      upLim = numeric(),
      minNom = numeric(),
      maxNom = numeric()
    )
  } else {
    details <- data.frame(
      index = idx,
      method = method,
      nMiss = nMiss,
      value = x[idx],
      type = ifelse(x[idx] < down, "L", "U"),
      lowLim = down,
      upLim = up,
      minNom = minNom,
      maxNom = maxNom
    )
  }

  summary <- data.frame(
    method = method,
    n = length(x),
    nMiss = nMiss,
    nOut = length(idx),
    lowLim = down,
    upLim = up,
    minNom = minNom,
    maxNom = maxNom
  )

  list(summary = summary, details = details)
}

# Purpose: Detect outliers in numeric variables.
# Arguments: Data frame and threshold multipliers.
# Returns: List with summary table and detail table.
detectOutliers <- function(data, t3 = 3, tH = 3, tb = 1.5) {
  numericVars <- getNumericVars(data)
  if (length(numericVars) == 0) {
    return(list(summary = data.frame(), detail = data.frame()))
  }

  summaries <- list()
  details <- list()

  for (v in numericVars) {
    x <- data[[v]]
    methods <- list(
      threeSigma = threeSigmaLimits(x, t3),
      hampel = hampelLimits(x, tH),
      boxplotRule = boxplotLimits(x, tb)
    )

    for (m in names(methods)) {
      res <- outlierForMethod(x, m, methods[[m]][["down"]], methods[[m]][["up"]])
      res$summary$variable <- v
      if (nrow(res$details) == 0) {
        res$details$variable <- character()
      } else {
        res$details$variable <- v
      }
      summaries[[paste(v, m)]] <- res$summary
      details[[paste(v, m)]] <- res$details
    }
  }

  summary <- do.call(rbind, summaries)
  detail <- do.call(rbind, details)
  rownames(summary) <- NULL
  rownames(detail) <- NULL
  if (nrow(detail) > 0) {
    methodOrder <- c("threeSigma", "hampel", "boxplotRule")
    detail <- detail[order(detail$variable, detail$index, match(detail$method, methodOrder)), , drop = FALSE]
    rownames(detail) <- NULL
  }

  list(summary = summary, detail = detail)
}

# Purpose: Build a variable dictionary table.
# Arguments: Data frame.
# Returns: Data frame with variable names and basic classes.
buildDictionary <- function(data) {
  data.frame(
    variable = names(data),
    type = vapply(data, function(x) paste(class(x), collapse = ", "), character(1)),
    nMissing = vapply(data, function(x) sum(is.na(x)), integer(1)),
    nUnique = vapply(data, function(x) length(unique(x)), integer(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# Purpose: Return built-in variable descriptions for common uploaded datasets.
# Arguments: None.
# Returns: Named character vector of descriptions.
defaultDescriptions <- function() {
  c(
    index = "Row index",
    user_id = "Unique identifier for each user",
    timepoint = "Number of timepoints",
    answered_at = "Date of the response",
    verified = "Verification status",
    gender = "Sex",
    birthdate = "Birth date",
    Age = "Age generated from birthdate",
    biotype = "Biotype classification",
    healthtype = "Health type classification",
    weight = "Weight",
    height = "Height",
    Waist = "Waist circumference",
    LBM = "Lean body mass",
    RiskStress = "Stress risk description",
    RiskStressVal = "Stress risk score",
    RiskDiabetes2 = "Type 2 diabetes risk description",
    RiskDiabetes2Val = "Type 2 diabetes risk score",
    RiskCardiovascular = "Cardiovascular risk description",
    RiskCardiovascularVal = "Cardiovascular risk score",
    q1 = "Over the past 7 days, I found it hard to wind down",
    q2 = "Over the past 7 days I was aware of dryness of my mouth",
    q3 = "Over the past 7 days I couldn\u2019t seem to experience any positive feeling at all",
    q4 = "Over the past 7 days I experienced breathing difficulty",
    q5 = "Over the past 7 days I found it difficult to work up the initiative to do things",
    q6 = "Over the past 7 days I tended to over-react to situations",
    q7 = "Over the past 7 days I experienced trembling",
    q8 = "Over the past 7 days I felt that I was using a lot of nervous energy",
    q9 = "Over the past 7 days I was worried about situations in which I might panic and make a fool of myself",
    q10 = "Over the past 7 days I felt that I had nothing to look forward to",
    q11 = "Over the past 7 days I found myself getting agitated",
    q12 = "Over the past 7 days I found it difficult to relax",
    q13 = "Over the past 7 days I felt down-hearted and blue",
    q14 = "Over the past 7 days I was intolerant of anything that kept me from getting on with what I was doing",
    q15 = "Over the past 7 days I felt I was close to panic",
    q16 = "Over the past 7 days I was unable to become enthusiastic about anything",
    q17 = "Over the past 7 days I felt I wasn\u2019t worth much as a person",
    q18 = "Over the past 7 days I felt that I was rather touchy",
    q19 = "Over the past 7 days I was aware of the action of my heart in the absence of physical exertion",
    q20 = "Over the past 7 days I felt scared without any good reason",
    q21 = "Over the past 7 days I felt that life was meaningless"
  )
}

# Purpose: Return built-in feature groups for common uploaded datasets.
# Arguments: None.
# Returns: Named character vector of feature groups.
defaultFeatures <- function() {
  c(
    index = "Metadata",
    user_id = "Metadata",
    timepoint = "Metadata",
    answered_at = "Metadata",
    verified = "Metadata",
    gender = "Demographics",
    birthdate = "Demographics",
    Age = "Demographics",
    biotype = "Health profile",
    healthtype = "Health profile",
    weight = "Anthropometry",
    height = "Anthropometry",
    Waist = "Anthropometry",
    LBM = "Anthropometry",
    RiskStress = "Risk assessment",
    RiskStressVal = "Risk assessment",
    RiskDiabetes2 = "Risk assessment",
    RiskDiabetes2Val = "Risk assessment",
    RiskCardiovascular = "Risk assessment",
    RiskCardiovascularVal = "Risk assessment",
    q1 = "DASS-21",
    q2 = "DASS-21",
    q3 = "DASS-21",
    q4 = "DASS-21",
    q5 = "DASS-21",
    q6 = "DASS-21",
    q7 = "DASS-21",
    q8 = "DASS-21",
    q9 = "DASS-21",
    q10 = "DASS-21",
    q11 = "DASS-21",
    q12 = "DASS-21",
    q13 = "DASS-21",
    q14 = "DASS-21",
    q15 = "DASS-21",
    q16 = "DASS-21",
    q17 = "DASS-21",
    q18 = "DASS-21",
    q19 = "DASS-21",
    q20 = "DASS-21",
    q21 = "DASS-21"
  )
}

# Purpose: Resolve descriptions from defaults plus optional uploaded metadata.
# Arguments: Optional uploaded dictionary metadata.
# Returns: Named character vector of descriptions.
resolveDescriptions <- function(metadata = NULL) {
  descriptions <- defaultDescriptions()
  if (!is.null(metadata) && length(metadata$descriptions) > 0) {
    newNames <- setdiff(names(metadata$descriptions), names(descriptions))
    descriptions[newNames] <- metadata$descriptions[newNames]
  }
  descriptions
}

# Purpose: Resolve feature groups from defaults plus optional uploaded metadata.
# Arguments: Optional uploaded dictionary metadata.
# Returns: Named character vector of feature groups.
resolveFeatures <- function(metadata = NULL) {
  features <- defaultFeatures()
  if (!is.null(metadata) && length(metadata$features) > 0) {
    newNames <- setdiff(names(metadata$features), names(features))
    features[newNames] <- metadata$features[newNames]
  }

  descriptionNames <- character(0)
  if (!is.null(metadata) && length(metadata$descriptions) > 0) {
    descriptionNames <- names(metadata$descriptions)
  }
  questionNames <- descriptionNames[grepl("^q[A-Za-z]*[0-9]+$", descriptionNames)]
  questionNames <- setdiff(questionNames, names(features))
  if (length(questionNames) > 0) {
    features[questionNames] <- "Questionnaire"
  }
  features
}

# Purpose: Summarise variables using dictionary metadata.
# Arguments: Data frame plus optional uploaded dictionary metadata.
# Returns: Data frame matching the variable-summary export intent.
variableDictionarySummary <- function(data, metadata = NULL) {
  descriptions <- resolveDescriptions(metadata)
  features <- resolveFeatures(metadata)
  varNames <- names(data)
  n <- nrow(data)

  out <- lapply(varNames, function(v) {
    x <- data[[v]]
    xtab <- table(x, useNA = "ifany")
    topIndex <- which.max(as.numeric(xtab))
    isBlank <- rep(FALSE, length(x))
    if (is.character(x) || is.factor(x)) {
      isBlank <- trimws(as.character(x)) == ""
    }
    missCount <- sum(is.na(x) | isBlank, na.rm = TRUE)

    data.frame(
      variable = v,
      description = ifelse(is.na(unname(descriptions[v])), "", unname(descriptions[v])),
      feature = ifelse(is.na(unname(features[v])), "", unname(features[v])),
      type = paste(class(x), collapse = ", "),
      levels = length(xtab),
      topLevel = names(xtab)[topIndex],
      topCount = as.integer(xtab[topIndex]),
      topFrac = round(as.integer(xtab[topIndex]) / n, 3),
      missCount = missCount,
      missFrac = round(missCount / n, 3),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}

# Purpose: Build a generic binary summary table for export.
# Arguments: Clean data and confidence level.
# Returns: Data frame with binary estimates by grouping variables.
exportBinaryTable <- function(data, confLevel = 0.95) {
  catVars <- getCategoricalVars(data)
  if (length(catVars) < 2) {
    return(data.frame())
  }

  levelCounts <- vapply(catVars, function(v) length(unique(stats::na.omit(data[[v]]))), integer(1))
  binaryVars <- names(levelCounts[levelCounts == 2])
  if (length(binaryVars) == 0) {
    return(data.frame())
  }

  binaryScore <- vapply(binaryVars, function(v) {
    levels <- tolower(as.character(unique(stats::na.omit(data[[v]]))))
    as.integer(all(c("female", "male") %in% levels))
  }, integer(1))
  binaryVar <- binaryVars[order(-binaryScore)][1]

  excludedGroups <- c(binaryVar, names(data)[grepl("^q[A-Za-z]*[0-9]+$", names(data))], names(data)[grepl("^Risk", names(data))])
  groupVars <- setdiff(names(levelCounts[levelCounts > 2 & levelCounts <= 100]), excludedGroups)
  preferredGroups <- intersect(c("healthtype", "biotype"), groupVars)
  groupVars <- unique(c(preferredGroups, setdiff(groupVars, preferredGroups)))
  if (length(binaryVars) == 0 || length(groupVars) == 0) {
    return(data.frame())
  }

  out <- list()
  levels <- sort(unique(as.character(stats::na.omit(data[[binaryVar]]))))
  if (length(levels) != 2) {
    return(data.frame())
  }

  targetLevel <- levels[1]
  otherLevel <- levels[2]
  targetCol <- paste0(gsub("[^A-Za-z0-9]+", "", tolower(targetLevel)), "Count")
  otherCol <- paste0(gsub("[^A-Za-z0-9]+", "", tolower(otherLevel)), "Count")

  for (groupVar in groupVars) {
    tmp <- binaryByGroup(data, binaryVar, groupVar, targetLevel, confLevel)
    if (nrow(tmp) == 0) {
      next
    }
    names(tmp)[names(tmp) == "count"] <- targetCol
    names(tmp)[names(tmp) == "otherCount"] <- otherCol
    tmp <- tmp[, c("groupVar", "groupLevel", "level", targetCol, otherCol, "estimate", "loCI", "upCI")]
    out[[groupVar]] <- tmp
  }

  if (length(out) == 0) {
    return(data.frame())
  }

  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

# Purpose: Format detailed outlier rows for the export workbook.
# Arguments: Outlier result list.
# Returns: Data frame with one row per detected outlier.
exportOutlierTable <- function(outliers) {
  if (is.null(outliers) || is.null(outliers$detail) || nrow(outliers$detail) == 0) {
    return(data.frame())
  }

  detail <- outliers$detail
  detail[, intersect(c("index", "variable", "method", "nMiss", "value", "lowLim", "upLim", "minNom", "maxNom"), names(detail)), drop = FALSE]
}

# Purpose: Describe exported workbook columns.
# Arguments: Clean data, export tables and optional uploaded dictionary metadata.
# Returns: Data frame with dataset, column and definition.
exportColumnDictionary <- function(data, tables, metadata = NULL) {
  dataDefs <- variableDictionarySummary(data, metadata)
  dataCols <- data.frame(
    dataset = "data",
    column = dataDefs$variable,
    definition = dataDefs$description,
    stringsAsFactors = FALSE
  )

  definitions <- list(
    summary = c(
      variable = "Name of the variable",
      description = "Definition of the column in the data",
      feature = "Definition of the group assigned by the author",
      type = "Data type",
      levels = "Number of distinct observed values",
      topLevel = "Value with highest frequency",
      topCount = "Frequency of topLevel",
      topFrac = "Fraction of records represented by topCount",
      missCount = "Number of missing values",
      missFrac = "Fraction of records represented by missCount"
    ),
    binary = c(
      groupVar = "Name of the grouping variable",
      groupLevel = "Level of the grouping variable",
      level = "Target level of the binary variable",
      femaleCount = "Number of Female records in the group",
      maleCount = "Number of Male records in the group",
      estimate = "Estimated proportion for the target level in the group",
      loCI = "Lower confidence limit for estimate",
      upCI = "Upper confidence limit for estimate"
    ),
    categorical = c(
      n_levels = "Number of observed levels",
      level = "Observed level of the variable",
      count = "Frequency of the level",
      proportion = "Fraction of records represented by the level"
    ),
    discrete = c(
      min = "Minimum observed value",
      max = "Maximum observed value",
      mean = "Arithmetic mean",
      variance = "Sample variance",
      variance_mean = "Ratio of variance to mean"
    ),
    numeric = c(
      n = "Number of non-missing observations",
      min = "Minimum observed value",
      p01 = "1st percentile",
      p05 = "5th percentile",
      q1 = "25th percentile",
      median = "50th percentile",
      mean = "Arithmetic mean",
      q3 = "75th percentile",
      p95 = "95th percentile",
      p99 = "99th percentile",
      max = "Maximum observed value",
      sd = "Sample standard deviation",
      iqr = "Interquartile range",
      range = "Difference between maximum and minimum",
      mean_median_diff = "Difference between mean and median",
      skewness = "Sample skewness"
    ),
    outlier = c(
      index = "Original row index in the data frame",
      variable = "Name of the variable where the outlier was detected",
      method = "Name of the outlier detection rule",
      nMiss = "Number of missing observations in the variable",
      value = "Detected outlying value",
      lowLim = "Lower outlier detection limit",
      upLim = "Upper outlier detection limit",
      minNom = "Minimum non-outlying value",
      maxNom = "Maximum non-outlying value"
    )
  )

  out <- list(data = dataCols)
  for (sheet in intersect(names(definitions), names(tables))) {
    if (sheet == "discrete") {
      next
    }
    defs <- definitions[[sheet]]
    tableCols <- names(tables[[sheet]])
    if (sheet %in% c("categorical", "numeric")) {
      tableCols <- setdiff(tableCols, "variable")
    }
    cols <- tableCols[tableCols %in% names(defs) | grepl("Count$", tableCols)]
    if (length(cols) == 0) {
      next
    }
      colDefs <- ifelse(
      cols %in% names(defs),
      unname(defs[cols]),
      "Count for the corresponding binary level"
    )
    out[[sheet]] <- data.frame(
      dataset = sheet,
      column = cols,
      definition = colDefs,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out
}

# Purpose: Build the key output tables for export.
# Arguments: Clean data and optional outlier result list.
# Returns: Named list of data frames.
buildEdaExportTables <- function(data, outliers = NULL, metadata = NULL) {
  catVars <- setdiff(getCategoricalVars(data), c("RiskStress", "RiskDiabetes2", "RiskCardiovascular"))
  excludedVars <- c("index", "user_id", "timepoint", "answered_at", "birthdate")
  intVars <- setdiff(getIntegerVars(data), excludedVars)
  numVars <- setdiff(getContinuousVars(data), excludedVars)

  tables <- list(
    summary = variableDictionarySummary(data, metadata),
    binary = exportBinaryTable(data),
    categorical = categoricalTable(data[catVars]),
    discrete = discreteTable(data[intVars]),
    numeric = distributionTable(data[numVars]),
    outlier = exportOutlierTable(outliers)
  )

  tables$columns <- exportColumnDictionary(data, tables, metadata)

  tables
}
