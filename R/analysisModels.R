# Purpose: Return labels keyed by dataset-version value.
# Arguments: None.
# Returns: Named character vector.
analysisVersionLabels <- function() {
  choices <- analysisOutlierChoices()
  labels <- names(choices)
  names(labels) <- unname(choices)
  labels
}

# Purpose: Return numeric variables suitable for multivariable diagnostics.
# Arguments: Data frame.
# Returns: Character vector of non-constant numeric variables.
defaultDiagnosticNumericVars <- function(data) {
  numericVars <- setdiff(getNumericVars(data), c("index", "user_id", "timepoint"))
  if (length(numericVars) == 0) {
    return(character(0))
  }

  numericVars[vapply(data[numericVars], function(x) {
    isTRUE(stats::sd(as.numeric(x), na.rm = TRUE) > 0)
  }, logical(1))]
}

# Purpose: Fit PCA on one reference dataset version and score selected versions.
# Arguments: Named list of data frames, numeric variables, reference version and compared versions.
# Returns: List with scores, loadings and variance explained.
fitPcaComparison <- function(dataVersions, variables, referenceVersion = "none", comparedVersions = NULL) {
  if (is.null(dataVersions) || length(dataVersions) == 0) {
    return(NULL)
  }

  if (is.null(comparedVersions) || length(comparedVersions) == 0) {
    comparedVersions <- names(dataVersions)
  }
  comparedVersions <- intersect(comparedVersions, names(dataVersions))
  if (!referenceVersion %in% names(dataVersions)) {
    referenceVersion <- comparedVersions[1]
  }
  if (is.na(referenceVersion) || length(comparedVersions) == 0) {
    return(NULL)
  }

  commonVars <- Reduce(intersect, lapply(dataVersions[c(referenceVersion, comparedVersions)], names))
  variables <- intersect(variables, commonVars)
  variables <- intersect(variables, defaultDiagnosticNumericVars(dataVersions[[referenceVersion]]))
  if (length(variables) < 2) {
    return(NULL)
  }

  xRef <- as.data.frame(lapply(dataVersions[[referenceVersion]][variables], as.numeric))
  completeRef <- stats::complete.cases(xRef)
  if (sum(completeRef) < 3) {
    return(NULL)
  }

  pca <- tryCatch(
    stats::prcomp(xRef[completeRef, , drop = FALSE], center = TRUE, scale. = TRUE),
    error = function(e) NULL
  )
  if (is.null(pca) || ncol(pca$rotation) < 2) {
    return(NULL)
  }

  labels <- analysisVersionLabels()
  scoreRows <- lapply(comparedVersions, function(version) {
    dat <- dataVersions[[version]]
    x <- as.data.frame(lapply(dat[variables], as.numeric))
    completeRows <- stats::complete.cases(x)
    if (sum(completeRows) == 0) {
      return(NULL)
    }

    scores <- stats::predict(pca, newdata = x[completeRows, , drop = FALSE])[, 1:2, drop = FALSE]
    rowId <- if ("index" %in% names(dat)) dat$index[completeRows] else which(completeRows)
    data.frame(
      version = version,
      dataset = if (version %in% names(labels)) labels[[version]] else version,
      rowId = rowId,
      as.data.frame(scores),
      row.names = NULL
    )
  })

  scores <- do.call(rbind, scoreRows)
  if (is.null(scores) || nrow(scores) == 0) {
    return(NULL)
  }
  scores$dataset <- factor(scores$dataset, levels = unname(labels[comparedVersions]))

  loadings <- as.data.frame(pca$rotation[, 1:2, drop = FALSE])
  loadings$variable <- row.names(loadings)
  row.names(loadings) <- NULL
  loadings$absPC1 <- abs(loadings$PC1)
  loadings$absPC2 <- abs(loadings$PC2)
  loadings <- loadings[order(-loadings$absPC1, -loadings$absPC2), c("variable", "PC1", "PC2", "absPC1", "absPC2")]

  variance <- round(100 * summary(pca)$importance[2, 1:2], 1)
  names(variance) <- c("PC1", "PC2")

  list(scores = scores, loadings = loadings, variance = variance, variables = variables)
}

# Purpose: Build long-format density data across selected dataset versions.
# Arguments: Named list of data frames, variables, compared versions, log-transform flag and preview limit.
# Returns: Data frame for density plotting.
buildDensityComparisonData <- function(dataVersions, variables, comparedVersions = NULL,
                                       logTransform = FALSE, maxVars = NULL) {
  if (is.null(dataVersions) || length(dataVersions) == 0) {
    return(data.frame())
  }

  if (is.null(comparedVersions) || length(comparedVersions) == 0) {
    comparedVersions <- names(dataVersions)
  }
  comparedVersions <- intersect(comparedVersions, names(dataVersions))
  if (length(comparedVersions) == 0) {
    return(data.frame())
  }

  commonVars <- Reduce(intersect, lapply(dataVersions[comparedVersions], defaultDiagnosticNumericVars))
  variables <- intersect(variables, commonVars)
  if (!is.null(maxVars) && length(variables) > maxVars) {
    variables <- head(variables, maxVars)
  }
  if (length(variables) == 0) {
    return(data.frame())
  }

  labels <- analysisVersionLabels()
  out <- lapply(comparedVersions, function(version) {
    dat <- dataVersions[[version]]
    tmp <- dat[variables]
    tmp$dataset <- if (version %in% names(labels)) labels[[version]] else version
    tmp$version <- version
    tmp
  })

  plotData <- do.call(rbind, out)
  plotData$dataset <- factor(plotData$dataset, levels = unname(labels[comparedVersions]))
  plotData <- tidyr::pivot_longer(plotData, dplyr::all_of(variables), names_to = "variable", values_to = "value")
  plotData$value <- suppressWarnings(as.numeric(plotData$value))
  if (isTRUE(logTransform)) {
    plotData$value <- ifelse(plotData$value > -1, log1p(plotData$value), NA_real_)
  }

  plotData
}

# Purpose: Build a long-format correlation table.
# Arguments: Data frame and correlation method.
# Returns: Data frame with variable pairs, sample sizes and correlations.
correlationTable <- function(data, method = "spearman") {
  numericVars <- setdiff(getNumericVars(data), c("index", "user_id", "timepoint"))
  numericVars <- numericVars[vapply(data[numericVars], function(x) isTRUE(stats::sd(x, na.rm = TRUE) > 0), logical(1))]
  if (length(numericVars) < 2) {
    return(data.frame())
  }

  pairs <- utils::combn(numericVars, 2, simplify = FALSE)
  out <- lapply(pairs, function(pair) {
    x <- data[[pair[1]]]
    y <- data[[pair[2]]]
    cc <- stats::complete.cases(x, y)
    n <- sum(cc)
    r <- if (n >= 3) suppressWarnings(stats::cor(x[cc], y[cc], method = method)) else NA_real_
    data.frame(var1 = pair[1], var2 = pair[2], n = n, r = round(r, 3), stringsAsFactors = FALSE)
  })

  do.call(rbind, out)
}

# Purpose: Build a correlation matrix for plotting.
# Arguments: Data frame, correlation method and optional maximum variable count.
# Returns: Matrix of pairwise correlations.
correlationMatrix <- function(data, method = "spearman", maxVars = NULL) {
  numericVars <- setdiff(getNumericVars(data), c("index", "user_id", "timepoint"))
  numericVars <- numericVars[vapply(data[numericVars], function(x) isTRUE(stats::sd(x, na.rm = TRUE) > 0), logical(1))]
  if (length(numericVars) < 2) {
    return(NULL)
  }
  if (!is.null(maxVars) && length(numericVars) > maxVars) {
    missingScore <- vapply(data[numericVars], function(x) mean(is.na(x)), numeric(1))
    varianceScore <- vapply(data[numericVars], function(x) stats::var(x, na.rm = TRUE), numeric(1))
    numericVars <- numericVars[order(missingScore, -varianceScore)]
    numericVars <- head(numericVars, maxVars)
  }

  round(stats::cor(data[numericVars], method = method, use = "pairwise.complete.obs"), 2)
}

# Purpose: Compare linear, quadratic and cubic polynomial lm models.
# Arguments: Data frame, outcome variable and numeric predictors.
# Returns: Data frame of adjusted R2, AIC and BIC.
fitPolyLmTable <- function(data, outcome, predictors) {
  predictors <- setdiff(intersect(predictors, getNumericVars(data)), outcome)
  if (!outcome %in% names(data) || length(predictors) == 0) {
    return(data.frame())
  }

  out <- lapply(predictors, function(v) {
    dat <- data.frame(y = data[[v]], x = data[[outcome]]) |> stats::na.omit()
    if (nrow(dat) < 8 || stats::sd(dat$x) == 0 || stats::sd(dat$y) == 0) {
      return(data.frame(
        variable = v,
        n = nrow(dat),
        linearAdjR2 = NA_real_,
        quadraticAdjR2 = NA_real_,
        cubicAdjR2 = NA_real_,
        linearAIC = NA_real_,
        quadraticAIC = NA_real_,
        cubicAIC = NA_real_,
        linearBIC = NA_real_,
        quadraticBIC = NA_real_,
        cubicBIC = NA_real_,
        row.names = NULL
      ))
    }

    m1 <- stats::lm(y ~ x, data = dat)
    m2 <- stats::lm(y ~ poly(x, 2, raw = TRUE), data = dat)
    m3 <- stats::lm(y ~ poly(x, 3, raw = TRUE), data = dat)

    data.frame(
      variable = v,
      n = nrow(dat),
      linearAdjR2 = summary(m1)$adj.r.squared,
      quadraticAdjR2 = summary(m2)$adj.r.squared,
      cubicAdjR2 = summary(m3)$adj.r.squared,
      linearAIC = AIC(m1),
      quadraticAIC = AIC(m2),
      cubicAIC = AIC(m3),
      linearBIC = BIC(m1),
      quadraticBIC = BIC(m2),
      cubicBIC = BIC(m3),
      row.names = NULL
    )
  })

  result <- do.call(rbind, out)
  numCols <- vapply(result, is.numeric, logical(1))
  result[numCols] <- lapply(result[numCols], round, 3)
  result
}

# Purpose: Compare log-log and untransformed lm fits.
# Arguments: Data frame, outcome and predictor variables.
# Returns: Data frame with model metrics.
compareLogLm <- function(data, outcome, predictor) {
  if (!all(c(outcome, predictor) %in% names(data))) {
    return(data.frame())
  }

  dat <- data[, c(outcome, predictor), drop = FALSE]
  names(dat) <- c("y", "x")
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  if (nrow(dat) < 5) {
    return(data.frame())
  }

  mRaw <- tryCatch(stats::lm(y ~ x, data = dat), error = function(e) NULL)
  logDat <- dat[dat$y > 0 & dat$x > 0, , drop = FALSE]
  mLog <- tryCatch(stats::lm(log(y) ~ log(x), data = logDat), error = function(e) NULL)

  data.frame(
    model = c("untransformed", "log-log"),
    n = c(if (is.null(mRaw)) NA_integer_ else nrow(dat), if (is.null(mLog)) NA_integer_ else nrow(logDat)),
    adjR2 = c(if (is.null(mRaw)) NA_real_ else summary(mRaw)$adj.r.squared,
              if (is.null(mLog)) NA_real_ else summary(mLog)$adj.r.squared),
    AIC = c(if (is.null(mRaw)) NA_real_ else AIC(mRaw), if (is.null(mLog)) NA_real_ else AIC(mLog)),
    BIC = c(if (is.null(mRaw)) NA_real_ else BIC(mRaw), if (is.null(mLog)) NA_real_ else BIC(mLog))
  )
}

# Purpose: Return GLM family choices supported by the formula builder.
# Arguments: None.
# Returns: Named character vector for selectInput choices.
glmFamilyChoices <- function() {
  c("Gaussian" = "gaussian", "Binomial" = "binomial", "Poisson" = "poisson", "Gamma" = "Gamma", "Inverse Gaussian" = "inverse.gaussian")
}

# Purpose: Create a GLM family object from a simple family name.
# Arguments: Family name selected in the UI.
# Returns: A stats family object.
makeGlmFamily <- function(familyName) {
  switch(
    familyName,
    binomial = stats::binomial(),
    poisson = stats::poisson(),
    Gamma = stats::Gamma(),
    inverse.gaussian = stats::inverse.gaussian(),
    stats::gaussian()
  )
}

# Purpose: Return outcome choices for generalized model builders.
# Arguments: Data frame.
# Returns: Character vector of candidate outcome variables.
modelOutcomeChoices <- function(data) {
  setdiff(names(data), c("index", "user_id"))
}

# Purpose: Return three default model outcomes.
# Arguments: Data frame.
# Returns: Character vector of up to three outcomes.
defaultModelOutcomes <- function(data) {
  riskVars <- intersect(defaultRiskVars(), names(data))
  candidates <- unique(c(riskVars, setdiff(modelOutcomeChoices(data), c(riskVars, "timepoint"))))
  head(candidates[!is.na(candidates) & nzchar(candidates)], 3)
}

# Purpose: Return a three-item outcome vector from user inputs and defaults.
# Arguments: Outcome inputs and data frame.
# Returns: Character vector of three outcomes where available.
selectedModelOutcomes <- function(outcomes, data) {
  defaults <- defaultModelOutcomes(data)
  outcomes <- as.character(outcomes)
  outcomes <- outcomes[!is.na(outcomes) & nzchar(outcomes) & outcomes %in% names(data)]
  out <- outcomes
  if (length(out) < 3) {
    out <- c(out, setdiff(defaults, out))
  }
  if (length(out) < 3) {
    out <- c(out, rep("", 3 - length(out)))
  }
  head(out, 3)
}

# Purpose: Check whether a variable can be used with a selected GLM/GLMM family.
# Arguments: Data frame, outcome and family name.
# Returns: NULL when valid, otherwise an error message.
validateOutcomeFamily <- function(data, outcome, familyName) {
  outcome <- scalarText(outcome)
  familyName <- scalarText(familyName, "gaussian")
  if (!hasScalarText(outcome) || !outcome %in% names(data)) {
    return("Outcome is not available in the selected dataset.")
  }

  x <- data[[outcome]]
  xComplete <- x[!is.na(x)]
  if (length(xComplete) < 3) {
    return("Outcome has fewer than three non-missing observations.")
  }

  if (identical(familyName, "gaussian")) {
    if (!is.numeric(x)) {
      return("Gaussian models require a numeric outcome.")
    }
    return(NULL)
  }

  if (identical(familyName, "binomial")) {
    if (is.logical(x)) {
      return(NULL)
    }
    if (is.numeric(x)) {
      vals <- unique(xComplete)
      if (all(vals %in% c(0, 1))) {
        return(NULL)
      }
      return("Binomial models require 0/1 numeric values or a two-level categorical outcome.")
    }
    if (length(unique(as.character(xComplete))) == 2) {
      return(NULL)
    }
    return("Binomial models require a two-level categorical outcome.")
  }

  if (identical(familyName, "poisson")) {
    if (!is.numeric(x)) {
      return("Poisson models require a numeric count outcome.")
    }
    vals <- xComplete
    if (any(vals < 0) || any(vals != floor(vals))) {
      return("Poisson models require non-negative integer counts.")
    }
    return(NULL)
  }

  if (familyName %in% c("Gamma", "inverse.gaussian")) {
    if (!is.numeric(x)) {
      return("Gamma and inverse Gaussian models require a numeric outcome.")
    }
    if (any(xComplete <= 0)) {
      return("Gamma and inverse Gaussian models require positive outcome values.")
    }
    return(NULL)
  }

  NULL
}

# Purpose: Coerce temporary outcome values where a family requires it.
# Arguments: Data frame, outcome variable and family name.
# Returns: Data frame with temporary model-compatible outcome type.
prepareOutcomeFamilyData <- function(data, outcome, familyName) {
  if (identical(familyName, "binomial") && outcome %in% names(data) &&
      !is.numeric(data[[outcome]]) && !is.logical(data[[outcome]])) {
    data[[outcome]] <- factor(data[[outcome]])
  }

  data
}

# Purpose: Fit one GLM from formula-builder settings.
# Arguments: Data frame, outcome, predictors, family, NA handling, transforms and interactions.
# Returns: List containing model, formula, status and metadata.
fitSingleGlmModel <- function(data, outcome, predictors, familyName = "gaussian", naAction = "na.omit",
                              logVars = character(), logMethod = "log", interactionBase = "",
                              interactionVars = character(), modelName = "Model") {
  outcome <- scalarText(outcome)
  familyName <- scalarText(familyName, "gaussian")
  predictors <- setdiff(intersect(predictors, names(data)), outcome)
  if (!hasScalarText(outcome) || length(predictors) == 0) {
    return(list(name = modelName, outcome = outcome, family = familyName, model = NULL, formula = NA_character_, error = "Select an outcome and at least one predictor."))
  }

  familyError <- validateOutcomeFamily(data, outcome, familyName)
  if (!is.null(familyError)) {
    return(list(name = modelName, outcome = outcome, family = familyName, model = NULL, formula = NA_character_, error = familyError))
  }

  logVars <- intersect(logVars, predictors)
  modelVars <- unique(c(outcome, predictors, logVars))
  dat <- data[, modelVars, drop = FALSE]
  dat <- prepareLogModelData(dat, logVars, logMethod)
  dat <- prepareOutcomeFamilyData(dat, outcome, familyName)
  dat <- makeNaAction(naAction)(dat)
  if (nrow(dat) < 8) {
    return(list(name = modelName, outcome = outcome, family = familyName, model = NULL, formula = NA_character_, error = "Model requires at least eight complete rows."))
  }

  rhsTerms <- buildModelRhsTerms(predictors, logVars, logMethod, interactionBase, interactionVars)
  form <- stats::as.formula(paste(modelTerm(outcome), "~", paste(rhsTerms, collapse = " + ")))
  familyObj <- makeGlmFamily(familyName)
  mod <- tryCatch(
    stats::glm(form, data = dat, family = familyObj, na.action = makeNaAction(naAction)),
    error = function(e) e
  )
  if (inherits(mod, "error")) {
    return(list(name = modelName, outcome = outcome, family = familyName, model = NULL, formula = paste(deparse(form), collapse = " "), error = conditionMessage(mod)))
  }

  list(name = modelName, outcome = outcome, family = familyName, model = mod, formula = paste(deparse(stats::formula(mod)), collapse = " "), error = "")
}

# Purpose: Fit up to three GLM models from formula-builder settings.
# Arguments: Data frame, outcomes, families, predictors and shared model settings.
# Returns: List of model result objects.
fitGlmOutcomeModels <- function(data, outcomes, families, predictors, naAction = "na.omit",
                                logVars = character(), logMethod = "log", interactionBase = "",
                                interactionVars = character()) {
  outcomes <- selectedModelOutcomes(outcomes, data)
  families <- as.character(families)
  if (length(families) < 3) {
    families <- c(families, rep("gaussian", 3 - length(families)))
  }
  families <- head(families, 3)

  stats::setNames(lapply(seq_along(outcomes), function(i) {
    fitSingleGlmModel(
      data,
      outcomes[i],
      predictors,
      families[i],
      naAction,
      logVars,
      logMethod,
      interactionBase,
      interactionVars,
      paste0("Model ", i)
    )
  }), paste0("Model ", seq_along(outcomes)))
}

# Purpose: Summarise GLM outcome models for AIC inspection.
# Arguments: List returned by fitGlmOutcomeModels().
# Returns: Data frame with model metrics and status.
glmOutcomeModelSummary <- function(fits) {
  if (is.null(fits) || length(fits) == 0) {
    return(data.frame())
  }

  out <- lapply(fits, function(fit) {
    mod <- fit$model
    data.frame(
      model = fit$name,
      outcome = fit$outcome,
      family = fit$family,
      n = if (is.null(mod)) NA_integer_ else stats::nobs(mod),
      AIC = if (is.null(mod)) NA_real_ else AIC(mod),
      formula = if (is.null(fit$formula)) NA_character_ else fit$formula,
      status = if (is.null(fit$error) || !nzchar(fit$error)) "ok" else fit$error,
      row.names = NULL
    )
  })

  result <- do.call(rbind, out)
  numCols <- vapply(result, is.numeric, logical(1))
  result[numCols] <- lapply(result[numCols], round, 3)
  result
}

# Purpose: Return NA-action choices supported by model builders.
# Arguments: None.
# Returns: Named character vector for selectInput choices.
naActionChoices <- function() {
  c("Omit missing" = "na.omit", "Exclude missing" = "na.exclude")
}

# Purpose: Convert NA-action text to a base R function.
# Arguments: NA-action name selected in the UI.
# Returns: A function suitable for model na.action.
makeNaAction <- function(naAction) {
  if (identical(naAction, "na.exclude")) {
    return(stats::na.exclude)
  }

  stats::na.omit
}

# Purpose: Return log-transform choices used by model formula builders.
# Arguments: None.
# Returns: Named character vector for selectInput choices.
logMethodChoices <- function() {
  c(
    "log(x)" = "log",
    "log1p(x)" = "log1p",
    "sign(x) * log1p(abs(x))" = "signedLog1p"
  )
}

# Purpose: Build a formula-safe variable term.
# Arguments: Variable name.
# Returns: Character scalar usable in an R formula.
modelTerm <- function(var) {
  var <- scalarText(var)
  if (!hasScalarText(var)) {
    return("")
  }

  reservedWords <- c("if", "else", "repeat", "while", "function", "for", "in", "next", "break",
                     "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_",
                     "NA_complex_", "NA_character_")
  if (identical(make.names(var), var) && !var %in% reservedWords) {
    return(var)
  }

  paste0("`", gsub("`", "\\\\`", var), "`")
}

# Purpose: Build raw or transformed formula terms.
# Arguments: Variables, selected log variables and log method.
# Returns: Character vector of formula terms.
buildFormulaTerms <- function(vars, logVars = character(), logMethod = "log") {
  vars <- vars[!is.na(vars) & nzchar(vars)]
  logVars <- intersect(logVars, vars)
  vapply(vars, function(var) {
    term <- modelTerm(var)
    if (!var %in% logVars) {
      return(term)
    }

    switch(
      logMethod,
      log1p = paste0("I(log1p(", term, "))"),
      signedLog1p = paste0("I(sign(", term, ") * log1p(abs(", term, ")))"),
      paste0("I(log(", term, "))")
    )
  }, character(1), USE.NAMES = FALSE)
}

# Purpose: Build RHS terms with optional interaction terms.
# Arguments: Predictors, log variables, log method, interaction base and variables.
# Returns: Character vector of RHS formula terms.
buildModelRhsTerms <- function(predictors, logVars = character(), logMethod = "log",
                               interactionBase = "", interactionVars = character()) {
  predictors <- predictors[!is.na(predictors) & nzchar(predictors)]
  predictors <- unique(predictors)
  if (length(predictors) == 0) {
    return("1")
  }

  termMap <- stats::setNames(buildFormulaTerms(predictors, logVars, logMethod), predictors)
  interactionBase <- scalarText(interactionBase)
  interactionVars <- setdiff(intersect(interactionVars, predictors), interactionBase)
  if (hasScalarText(interactionBase) && interactionBase %in% predictors && length(interactionVars) > 0) {
    interactionTerms <- paste0(termMap[[interactionBase]], " * ", unname(termMap[interactionVars]))
    mainVars <- setdiff(predictors, c(interactionBase, interactionVars))
    return(unique(c(interactionTerms, unname(termMap[mainVars]))))
  }

  unname(termMap)
}

# Purpose: Set values incompatible with selected log methods to NA in a temporary data copy.
# Arguments: Data frame, variables to transform and log method.
# Returns: Data frame with invalid transform inputs set to NA.
prepareLogModelData <- function(data, logVars = character(), logMethod = "log") {
  logVars <- intersect(logVars, names(data))
  if (length(logVars) == 0) {
    return(data)
  }

  out <- data
  for (var in logVars) {
    x <- suppressWarnings(as.numeric(out[[var]]))
    invalid <- !is.finite(x)
    if (identical(logMethod, "log")) {
      invalid <- invalid | (!is.na(x) & x <= 0)
    } else if (identical(logMethod, "log1p")) {
      invalid <- invalid | (!is.na(x) & x <= -1)
    }
    out[[var]][invalid] <- NA
  }

  out
}

# Purpose: Fit backward and forward AIC-selected lm models.
# Arguments: Data frame, outcome, predictor variables and variables to drop.
# Returns: List with selected models and metrics.
fitLmStepAic <- function(data, outcome, predictors) {
  if (!requireNamespace("MASS", quietly = TRUE) || !outcome %in% names(data)) {
    return(NULL)
  }

  predictors <- setdiff(intersect(predictors, names(data)), outcome)
  if (length(predictors) == 0) {
    return(NULL)
  }

  dat <- data[, c(outcome, predictors), drop = FALSE] |> stats::na.omit()
  if (nrow(dat) < 8) {
    return(NULL)
  }

  fullForm <- stats::reformulate(predictors, response = outcome)
  nullForm <- stats::reformulate("1", response = outcome)
  fullMod <- tryCatch(stats::lm(fullForm, data = dat), error = function(e) NULL)
  nullMod <- tryCatch(stats::lm(nullForm, data = dat), error = function(e) NULL)
  if (is.null(fullMod) || is.null(nullMod)) {
    return(NULL)
  }

  backward <- tryCatch(MASS::stepAIC(fullMod, direction = "backward", trace = FALSE), error = function(e) NULL)
  forward <- tryCatch(MASS::stepAIC(nullMod, scope = stats::formula(fullMod), direction = "forward", trace = FALSE), error = function(e) NULL)

  list(data = dat, full = fullMod, backward = backward, forward = forward)
}

# Purpose: Fit backward and forward AIC-selected GLM models.
# Arguments: Data frame, outcome, predictor variables, family, NA handling and optional transforms/interactions.
# Returns: List with selected models and metrics.
fitGlmStepAic <- function(data, outcome, predictors, familyName = "gaussian", naAction = "na.omit",
                          logVars = character(), logMethod = "log", interactionBase = "",
                          interactionVars = character()) {
  if (!requireNamespace("MASS", quietly = TRUE) || !outcome %in% names(data)) {
    return(NULL)
  }

  predictors <- setdiff(intersect(predictors, names(data)), outcome)
  if (length(predictors) == 0) {
    return(NULL)
  }

  logVars <- intersect(logVars, c(outcome, predictors))
  dat <- data[, c(outcome, predictors), drop = FALSE]
  dat <- prepareLogModelData(dat, logVars, logMethod)
  dat <- makeNaAction(naAction)(dat)
  if (nrow(dat) < 8) {
    return(NULL)
  }

  rhsTerms <- buildModelRhsTerms(predictors, logVars, logMethod, interactionBase, interactionVars)
  responseTerm <- buildFormulaTerms(outcome, logVars, logMethod)
  fullForm <- stats::as.formula(paste(responseTerm, "~", paste(rhsTerms, collapse = " + ")))
  nullForm <- stats::as.formula(paste(responseTerm, "~ 1"))
  familyObj <- makeGlmFamily(familyName)
  fullMod <- tryCatch(stats::glm(fullForm, data = dat, family = familyObj, na.action = makeNaAction(naAction)), error = function(e) NULL)
  nullMod <- tryCatch(stats::glm(nullForm, data = dat, family = familyObj, na.action = makeNaAction(naAction)), error = function(e) NULL)
  if (is.null(fullMod) || is.null(nullMod)) {
    return(NULL)
  }

  backward <- tryCatch(MASS::stepAIC(fullMod, direction = "backward", trace = FALSE), error = function(e) NULL)
  forward <- tryCatch(MASS::stepAIC(nullMod, scope = stats::formula(fullMod), direction = "forward", trace = FALSE), error = function(e) NULL)

  list(data = dat, full = fullMod, backward = backward, forward = forward)
}

# Purpose: Summarise selected lm models.
# Arguments: Model-selection result list.
# Returns: Data frame with formulas, AIC, BIC and adjusted R2.
lmSelectionSummary <- function(selection) {
  if (is.null(selection)) {
    return(data.frame())
  }

  mods <- list(full = selection$full, backward = selection$backward, forward = selection$forward)
  out <- lapply(names(mods), function(nm) {
    mod <- mods[[nm]]
    if (is.null(mod)) {
      return(data.frame(model = nm, formula = NA_character_, AIC = NA_real_, BIC = NA_real_, adjR2 = NA_real_))
    }
    data.frame(
      model = nm,
      formula = paste(deparse(stats::formula(mod)), collapse = " "),
      AIC = AIC(mod),
      BIC = BIC(mod),
      adjR2 = summary(mod)$adj.r.squared,
      row.names = NULL
    )
  })

  result <- do.call(rbind, out)
  numCols <- vapply(result, is.numeric, logical(1))
  result[numCols] <- lapply(result[numCols], round, 3)
  result
}

# Purpose: Summarise selected GLM models.
# Arguments: Model-selection result list.
# Returns: Data frame with formulas and AIC values.
glmSelectionSummary <- function(selection) {
  if (is.null(selection)) {
    return(data.frame())
  }

  mods <- list(full = selection$full, backward = selection$backward, forward = selection$forward)
  out <- lapply(names(mods), function(nm) {
    mod <- mods[[nm]]
    if (is.null(mod)) {
      return(data.frame(model = nm, formula = NA_character_, AIC = NA_real_))
    }
    data.frame(
      model = nm,
      formula = paste(deparse(stats::formula(mod)), collapse = " "),
      AIC = AIC(mod),
      row.names = NULL
    )
  })

  result <- do.call(rbind, out)
  numCols <- vapply(result, is.numeric, logical(1))
  result[numCols] <- lapply(result[numCols], round, 3)
  result
}

# Purpose: Return coefficient table for a fitted model.
# Arguments: lm or lmer model.
# Returns: Data frame of model coefficients.
modelCoefficientTable <- function(mod) {
  if (is.null(mod)) {
    return(data.frame())
  }

  coefs <- as.data.frame(summary(mod)$coefficients)
  coefs$term <- row.names(coefs)
  row.names(coefs) <- NULL
  coefs[, c("term", setdiff(names(coefs), "term")), drop = FALSE]
}

# Purpose: Get marginal and conditional R2 for an lmer model.
# Arguments: Fitted lmer model.
# Returns: Named numeric vector.
getLmeR2 <- function(mod) {
  if (is.null(mod) || !requireNamespace("performance", quietly = TRUE)) {
    return(c(marginal = NA_real_, conditional = NA_real_))
  }

  r2 <- tryCatch(
    {
      r2Value <- NULL
      invisible(utils::capture.output(
        r2Value <- suppressWarnings(suppressMessages(as.data.frame(performance::r2_nakagawa(mod)))),
        type = "message"
      ))
      r2Value
    },
    error = function(e) data.frame(R2_marginal = NA_real_, R2_conditional = NA_real_)
  )

  c(marginal = r2$R2_marginal[1], conditional = r2$R2_conditional[1])
}

# Purpose: Fit an lmer model with lmerTest when available.
# Arguments: Formula and data frame.
# Returns: Fitted mixed-effects model or NULL.
fitLmerModel <- function(formula, data, naAction = "na.omit") {
  if (requireNamespace("lmerTest", quietly = TRUE)) {
    return(tryCatch(lmerTest::lmer(formula, data = data, REML = FALSE, na.action = makeNaAction(naAction)), error = function(e) NULL))
  }
  if (requireNamespace("lme4", quietly = TRUE)) {
    return(tryCatch(lme4::lmer(formula, data = data, REML = FALSE, na.action = makeNaAction(naAction)), error = function(e) NULL))
  }

  NULL
}

# Purpose: Fit a generalized mixed model with lmer for Gaussian or glmer otherwise.
# Arguments: Formula, data frame, family name and NA handling.
# Returns: Fitted mixed model or error object.
fitGeneralMixedModel <- function(formula, data, familyName = "gaussian", naAction = "na.omit") {
  familyName <- scalarText(familyName, "gaussian")
  if (identical(familyName, "gaussian")) {
    if (requireNamespace("lmerTest", quietly = TRUE)) {
      return(tryCatch(
        lmerTest::lmer(formula, data = data, REML = FALSE, na.action = makeNaAction(naAction)),
        error = function(e) e
      ))
    }
    if (requireNamespace("lme4", quietly = TRUE)) {
      return(tryCatch(
        lme4::lmer(formula, data = data, REML = FALSE, na.action = makeNaAction(naAction)),
        error = function(e) e
      ))
    }
    return(simpleError("Install lme4 or lmerTest to fit Gaussian mixed models."))
  }

  if (!requireNamespace("lme4", quietly = TRUE)) {
    return(simpleError("Install lme4 to fit generalized mixed models."))
  }

  tryCatch(
    lme4::glmer(formula, data = data, family = makeGlmFamily(familyName), na.action = makeNaAction(naAction)),
    error = function(e) e
  )
}

# Purpose: Fit one generalized mixed model from formula-builder settings.
# Arguments: Data frame, outcome, predictors, family, group/time variables and shared settings.
# Returns: List containing model, formula, status and metadata.
fitSingleMixedOutcomeModel <- function(data, outcome, predictors, familyName = "gaussian",
                                       groupVar = "", timeVar = "", timeInteraction = "",
                                       randomSlopes = character(), naAction = "na.omit",
                                       logVars = character(), logMethod = "log",
                                       modelName = "Model") {
  outcome <- scalarText(outcome)
  familyName <- scalarText(familyName, "gaussian")
  groupVar <- scalarText(groupVar)
  timeVar <- scalarText(timeVar)
  timeInteraction <- scalarText(timeInteraction)
  if (!hasScalarText(groupVar) || !groupVar %in% names(data)) {
    return(list(name = modelName, outcome = outcome, family = familyName, model = NULL, formula = NA_character_, error = "Select a valid grouping variable."))
  }
  if (!hasScalarText(outcome) || !outcome %in% names(data)) {
    return(list(name = modelName, outcome = outcome, family = familyName, model = NULL, formula = NA_character_, error = "Select a valid outcome."))
  }

  familyError <- validateOutcomeFamily(data, outcome, familyName)
  if (!is.null(familyError)) {
    return(list(name = modelName, outcome = outcome, family = familyName, model = NULL, formula = NA_character_, error = familyError))
  }

  predictors <- setdiff(intersect(predictors, names(data)), c(outcome, groupVar, timeVar))
  randomSlopes <- setdiff(intersect(randomSlopes, names(data)), c(outcome, groupVar))
  logVars <- intersect(logVars, predictors)
  fixedTerms <- buildFormulaTerms(predictors, logVars, logMethod)
  if (hasScalarText(timeVar) && timeVar %in% names(data)) {
    timeTerm <- modelTerm(timeVar)
    if (hasScalarText(timeInteraction) && timeInteraction %in% predictors) {
      timeInteractionTerm <- buildFormulaTerms(timeInteraction, logVars, logMethod)
      fixedTerms <- c(paste0(timeTerm, " * ", timeInteractionTerm), buildFormulaTerms(setdiff(predictors, timeInteraction), logVars, logMethod))
    } else {
      fixedTerms <- c(timeTerm, fixedTerms)
    }
  }
  fixedTerms <- fixedTerms[!is.na(fixedTerms) & nzchar(fixedTerms)]
  rhs <- c(if (length(fixedTerms) == 0) "1" else fixedTerms, buildRandomEffectTerm(groupVar, randomSlopes))
  form <- stats::as.formula(paste(modelTerm(outcome), "~", paste(rhs, collapse = " + ")))

  modelVars <- unique(c(outcome, predictors, groupVar, timeVar, timeInteraction, randomSlopes, logVars))
  modelVars <- modelVars[!is.na(modelVars) & nzchar(modelVars) & modelVars %in% names(data)]
  dat <- data[, modelVars, drop = FALSE]
  dat <- prepareLogModelData(dat, logVars, logMethod)
  dat <- prepareOutcomeFamilyData(dat, outcome, familyName)
  dat <- makeNaAction(naAction)(dat)
  if (nrow(dat) < 10 || length(unique(dat[[groupVar]])) < 2) {
    return(list(name = modelName, outcome = outcome, family = familyName, model = NULL, formula = paste(deparse(form), collapse = " "), error = "Model requires at least ten complete rows and two groups."))
  }

  mod <- fitGeneralMixedModel(form, dat, familyName, naAction)
  if (inherits(mod, "error")) {
    return(list(name = modelName, outcome = outcome, family = familyName, model = NULL, formula = paste(deparse(form), collapse = " "), error = conditionMessage(mod)))
  }

  list(name = modelName, outcome = outcome, family = familyName, model = mod, formula = paste(deparse(stats::formula(mod)), collapse = " "), error = "")
}

# Purpose: Fit up to three generalized mixed models from formula-builder settings.
# Arguments: Data frame, outcomes, families and shared mixed-model settings.
# Returns: List of model result objects.
fitMixedOutcomeModels <- function(data, outcomes, families, predictors, groupVar, timeVar = "",
                                  timeInteraction = "", randomSlopes = character(),
                                  naAction = "na.omit", logVars = character(), logMethod = "log") {
  outcomes <- selectedModelOutcomes(outcomes, data)
  families <- as.character(families)
  if (length(families) < 3) {
    families <- c(families, rep("gaussian", 3 - length(families)))
  }
  families <- head(families, 3)

  stats::setNames(lapply(seq_along(outcomes), function(i) {
    fitSingleMixedOutcomeModel(
      data,
      outcomes[i],
      predictors,
      families[i],
      groupVar,
      timeVar,
      timeInteraction,
      randomSlopes,
      naAction,
      logVars,
      logMethod,
      paste0("Model ", i)
    )
  }), paste0("Model ", seq_along(outcomes)))
}

# Purpose: Summarise generalized mixed models for AIC and R2 inspection.
# Arguments: List returned by fitMixedOutcomeModels().
# Returns: Data frame with model metrics and status.
mixedOutcomeModelSummary <- function(fits) {
  if (is.null(fits) || length(fits) == 0) {
    return(data.frame())
  }

  out <- lapply(fits, function(fit) {
    mod <- fit$model
    r2 <- getLmeR2(mod)
    data.frame(
      model = fit$name,
      outcome = fit$outcome,
      family = fit$family,
      n = if (is.null(mod)) NA_integer_ else stats::nobs(mod),
      AIC = if (is.null(mod)) NA_real_ else AIC(mod),
      marginalR2 = r2["marginal"],
      conditionalR2 = r2["conditional"],
      formula = if (is.null(fit$formula)) NA_character_ else fit$formula,
      status = if (is.null(fit$error) || !nzchar(fit$error)) "ok" else fit$error,
      row.names = NULL
    )
  })

  result <- do.call(rbind, out)
  numCols <- vapply(result, is.numeric, logical(1))
  result[numCols] <- lapply(result[numCols], round, 3)
  result
}

# Purpose: Compare linear, quadratic and cubic polynomial lmer models.
# Arguments: Data frame, outcome, predictors, user id and time variables.
# Returns: Data frame with AIC, BIC and Nakagawa R2.
fitPolyLmeTable <- function(data, outcome, predictors, userVar, timeVar) {
  outcome <- scalarText(outcome)
  userVar <- scalarText(userVar)
  timeVar <- scalarText(timeVar)
  predictors <- setdiff(intersect(predictors, getNumericVars(data)), outcome)
  requiredVars <- c(outcome, userVar, timeVar)
  if (!all(vapply(requiredVars, hasScalarText, logical(1))) ||
      !all(c(outcome, userVar, timeVar) %in% names(data)) ||
      length(predictors) == 0) {
    return(data.frame())
  }

  out <- lapply(predictors, function(v) {
    dat <- data.frame(
      y = data[[v]],
      x = data[[outcome]],
      user_id = data[[userVar]],
      timepoint = data[[timeVar]]
    ) |> stats::na.omit()

    if (nrow(dat) < 10 || length(unique(dat$user_id)) < 2 || length(unique(dat$x)) < 4) {
      return(data.frame(
        variable = v,
        n = nrow(dat),
        linearMarginalR2 = NA_real_,
        quadraticMarginalR2 = NA_real_,
        cubicMarginalR2 = NA_real_,
        linearConditionalR2 = NA_real_,
        quadraticConditionalR2 = NA_real_,
        cubicConditionalR2 = NA_real_,
        linearAIC = NA_real_,
        quadraticAIC = NA_real_,
        cubicAIC = NA_real_,
        linearBIC = NA_real_,
        quadraticBIC = NA_real_,
        cubicBIC = NA_real_,
        row.names = NULL
      ))
    }

    m1 <- fitLmerModel(y ~ x + timepoint + (1 | user_id), dat)
    m2 <- fitLmerModel(y ~ poly(x, 2, raw = TRUE) + timepoint + (1 | user_id), dat)
    m3 <- fitLmerModel(y ~ poly(x, 3, raw = TRUE) + timepoint + (1 | user_id), dat)
    r21 <- getLmeR2(m1)
    r22 <- getLmeR2(m2)
    r23 <- getLmeR2(m3)

    data.frame(
      variable = v,
      n = nrow(dat),
      linearMarginalR2 = r21["marginal"],
      quadraticMarginalR2 = r22["marginal"],
      cubicMarginalR2 = r23["marginal"],
      linearConditionalR2 = r21["conditional"],
      quadraticConditionalR2 = r22["conditional"],
      cubicConditionalR2 = r23["conditional"],
      linearAIC = if (is.null(m1)) NA_real_ else AIC(m1),
      quadraticAIC = if (is.null(m2)) NA_real_ else AIC(m2),
      cubicAIC = if (is.null(m3)) NA_real_ else AIC(m3),
      linearBIC = if (is.null(m1)) NA_real_ else BIC(m1),
      quadraticBIC = if (is.null(m2)) NA_real_ else BIC(m2),
      cubicBIC = if (is.null(m3)) NA_real_ else BIC(m3),
      row.names = NULL
    )
  })

  result <- do.call(rbind, out)
  numCols <- vapply(result, is.numeric, logical(1))
  result[numCols] <- lapply(result[numCols], round, 3)
  result
}

# Purpose: Compare log-log and untransformed lmer fits.
# Arguments: Data frame, outcome, predictor, user id and time variables.
# Returns: Data frame with model metrics.
compareLogLme <- function(data, outcome, predictor, userVar, timeVar, naAction = "na.omit") {
  outcome <- scalarText(outcome)
  predictor <- scalarText(predictor)
  userVar <- scalarText(userVar)
  timeVar <- scalarText(timeVar)
  requiredVars <- c(outcome, predictor, userVar, timeVar)
  if (!all(vapply(requiredVars, hasScalarText, logical(1))) ||
      !all(c(outcome, predictor, userVar, timeVar) %in% names(data))) {
    return(data.frame())
  }

  dat <- data.frame(y = data[[outcome]], x = data[[predictor]], user_id = data[[userVar]], timepoint = data[[timeVar]])
  dat <- makeNaAction(naAction)(dat)
  if (nrow(dat) < 10 || length(unique(dat$user_id)) < 2) {
    return(data.frame())
  }

  rawMod <- fitLmerModel(y ~ x + timepoint + (1 | user_id), dat, naAction)
  logDat <- dat[dat$y > 0 & dat$x > 0, , drop = FALSE]
  logMod <- fitLmerModel(log(y) ~ log(x) + timepoint + (1 | user_id), logDat, naAction)
  r2Raw <- getLmeR2(rawMod)
  r2Log <- getLmeR2(logMod)

  data.frame(
    model = c("untransformed", "log-log"),
    n = c(if (is.null(rawMod)) NA_integer_ else nrow(dat), if (is.null(logMod)) NA_integer_ else nrow(logDat)),
    marginalR2 = c(r2Raw["marginal"], r2Log["marginal"]),
    conditionalR2 = c(r2Raw["conditional"], r2Log["conditional"]),
    AIC = c(if (is.null(rawMod)) NA_real_ else AIC(rawMod), if (is.null(logMod)) NA_real_ else AIC(logMod)),
    BIC = c(if (is.null(rawMod)) NA_real_ else BIC(rawMod), if (is.null(logMod)) NA_real_ else BIC(logMod))
  )
}

# Purpose: Fit LME interaction models by focal predictor.
# Arguments: Data frame, outcome, focal predictors, covariates, user id and time variables.
# Returns: List of models and summary table.
# Purpose: Build an lmer random-effects term.
# Arguments: Grouping variable and optional random slopes.
# Returns: Character string for the random-effects part of a formula.
buildRandomEffectTerm <- function(groupVar, randomSlopes = character()) {
  groupVar <- scalarText(groupVar)
  if (!hasScalarText(groupVar)) {
    return("")
  }

  randomSlopes <- randomSlopes[!is.na(randomSlopes) & nzchar(randomSlopes)]
  if (length(randomSlopes) == 0) {
    return(paste0("(1 | ", modelTerm(groupVar), ")"))
  }

  paste0("(1 + ", paste(buildFormulaTerms(randomSlopes), collapse = " + "), " | ", modelTerm(groupVar), ")")
}

# Purpose: Fit LME interaction models by focal predictor.
# Arguments: Data frame, outcome, focal predictors, covariates, grouping/time variables, NA handling and transforms.
# Returns: List of models and summary table.
fitLmeInteractionModels <- function(data, outcome, focalPredictors, covariates, userVar, timeVar,
                                    randomSlopes = character(), naAction = "na.omit",
                                    logVars = character(), logMethod = "log") {
  outcome <- scalarText(outcome)
  userVar <- scalarText(userVar)
  timeVar <- scalarText(timeVar)
  focalPredictors <- focalPredictors[!is.na(focalPredictors) & nzchar(focalPredictors)]
  requiredVars <- c(outcome, userVar, timeVar)
  if (!all(vapply(requiredVars, hasScalarText, logical(1))) ||
      !all(c(outcome, userVar, timeVar) %in% names(data)) ||
      length(focalPredictors) == 0) {
    return(list(models = list(), summary = data.frame()))
  }

  focalPredictors <- intersect(focalPredictors, names(data))
  covariates <- setdiff(intersect(covariates, names(data)), c(outcome, focalPredictors, userVar, timeVar))
  randomSlopes <- setdiff(intersect(randomSlopes, names(data)), c(outcome, userVar))
  logVars <- intersect(logVars, c(outcome, focalPredictors, covariates))
  modelVars <- unique(c(outcome, focalPredictors, covariates, randomSlopes, userVar, timeVar, logVars))
  dat <- prepareLogModelData(data[, modelVars, drop = FALSE], logVars, logMethod)
  dat <- makeNaAction(naAction)(dat)
  if (nrow(dat) < 10 || length(unique(dat[[userVar]])) < 2) {
    return(list(models = list(), summary = data.frame()))
  }

  models <- lapply(focalPredictors, function(focal) {
    timeTerm <- modelTerm(timeVar)
    focalTerm <- buildFormulaTerms(focal, logVars, logMethod)
    rhs <- c(
      paste0(timeTerm, " * ", focalTerm),
      buildFormulaTerms(setdiff(focalPredictors, focal), logVars, logMethod),
      buildFormulaTerms(covariates, logVars, logMethod),
      buildRandomEffectTerm(userVar, randomSlopes)
    )
    rhs <- rhs[!is.na(rhs) & nzchar(rhs)]
    responseTerm <- buildFormulaTerms(outcome, logVars, logMethod)
    form <- stats::as.formula(paste(responseTerm, "~", paste(rhs, collapse = " + ")))
    fitLmerModel(form, dat, naAction)
  })
  names(models) <- focalPredictors

  summaryRows <- lapply(names(models), function(nm) {
    mod <- models[[nm]]
    r2 <- getLmeR2(mod)
    data.frame(
      interaction = nm,
      n = if (is.null(mod)) NA_integer_ else stats::nobs(mod),
      AIC = if (is.null(mod)) NA_real_ else AIC(mod),
      marginalR2 = r2["marginal"],
      conditionalR2 = r2["conditional"],
      formula = if (is.null(mod)) NA_character_ else paste(deparse(stats::formula(mod)), collapse = " "),
      row.names = NULL
    )
  })

  summary <- do.call(rbind, summaryRows)
  numCols <- vapply(summary, is.numeric, logical(1))
  summary[numCols] <- lapply(summary[numCols], round, 3)
  list(models = models, summary = summary)
}
