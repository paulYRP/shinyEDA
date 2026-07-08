# Purpose: Plot a correlation heatmap.
# Arguments: Correlation matrix.
# Returns: ggplot object.
plotCorrelationMatrix <- function(corMat) {
  shiny::validate(shiny::need(!is.null(corMat), "At least two numeric variables are required."))
  plotData <- as.data.frame(as.table(corMat))
  names(plotData) <- c("var1", "var2", "r")

  ggplot2::ggplot(plotData, ggplot2::aes(var1, var2, fill = r)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", r)), size = 3) +
    ggplot2::scale_fill_gradient2(low = "#3b6fb6", mid = "white", high = "#b64242", limits = c(-1, 1), na.value = "grey90") +
    ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL, fill = "r") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

# Purpose: Plot PCA scores across outlier-filtered dataset versions.
# Arguments: PCA comparison result from fitPcaComparison().
# Returns: ggplot object.
plotPcaComparison <- function(pcaResult) {
  shiny::validate(shiny::need(!is.null(pcaResult), "Select at least two numeric variables with enough complete rows for PCA."))
  shiny::validate(shiny::need(nrow(pcaResult$scores) > 0, "No PCA scores are available."))

  ggplot2::ggplot(pcaResult$scores, ggplot2::aes(PC1, PC2)) +
    ggplot2::geom_point(alpha = 0.7, na.rm = TRUE) +
    ggplot2::facet_wrap(~ dataset, ncol = min(4, length(unique(pcaResult$scores$dataset)))) +
    ggplot2::labs(
      title = "PCA removing outliers",
      subtitle = "PCA is fitted on the selected reference version and projected onto selected versions.",
      x = paste0("PC1 (", pcaResult$variance[["PC1"]], "%)"),
      y = paste0("PC2 (", pcaResult$variance[["PC2"]], "%)")
    ) +
    ggplot2::theme_bw()
}

# Purpose: Plot density comparison across outlier-filtered dataset versions.
# Arguments: Long-format density data and whether log1p was used.
# Returns: ggplot object.
plotDensityComparison <- function(plotData, logTransform = FALSE) {
  shiny::validate(shiny::need(nrow(plotData) > 0, "Select numeric variables with enough non-missing values."))

  ggplot2::ggplot(plotData, ggplot2::aes(value)) +
    ggplot2::geom_density(na.rm = TRUE) +
    ggplot2::facet_grid(variable ~ dataset, scales = "free") +
    ggplot2::labs(
      title = if (isTRUE(logTransform)) {
        "Log-density comparison before and after removing outliers"
      } else {
        "Density comparison before and after removing outliers"
      },
      x = if (isTRUE(logTransform)) "log1p(Value)" else "Value",
      y = "Density"
    ) +
    ggplot2::theme_bw()
}

# Purpose: Plot polynomial relationships for one outcome against numeric predictors.
# Arguments: Data frame, outcome, predictors, polynomial degree, optional colour variable and maximum predictors.
# Returns: ggplot object.
plotPolyRelationships <- function(data, outcome, predictors, degree = 2, colourVar = NULL, maxPredictors = NULL) {
  colourVar <- scalarText(colourVar)
  if (!hasScalarText(colourVar) || !colourVar %in% names(data)) {
    colourVar <- NULL
  }
  predictors <- setdiff(intersect(predictors, getNumericVars(data)), outcome)
  shiny::validate(shiny::need(outcome %in% names(data), "Select an outcome."))
  shiny::validate(shiny::need(length(predictors) > 0, "Select at least one numeric predictor."))
  if (!is.null(maxPredictors) && length(predictors) > maxPredictors) {
    predictors <- head(predictors, maxPredictors)
  }

  plotData <- data[, c(outcome, predictors, colourVar), drop = FALSE] |>
    tidyr::pivot_longer(dplyr::all_of(predictors), names_to = "variable", values_to = "value")

  aesArgs <- ggplot2::aes(.data[[outcome]], value)
  if (!is.null(colourVar) && colourVar %in% names(plotData)) {
    aesArgs <- ggplot2::aes(.data[[outcome]], value, colour = as.factor(.data[[colourVar]]))
  }

  ggplot2::ggplot(plotData, aesArgs) +
    ggplot2::geom_point(alpha = 0.45, na.rm = TRUE) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ poly(x, degree, raw = TRUE), se = TRUE, na.rm = TRUE) +
    ggplot2::facet_wrap(~ variable, scales = "free_y") +
    ggplot2::labs(
      title = paste(ifelse(degree == 2, "Quadratic", "Cubic"), "relationships with", outcome),
      x = outcome,
      y = "Variable value",
      colour = colourVar
    ) +
    ggplot2::theme_minimal()
}

# Purpose: Plot longitudinal trajectories for selected outcomes.
# Arguments: Data frame, user id variable, time variable and outcomes.
# Returns: ggplot object.
plotOutcomeTrajectories <- function(data, userVar, timeVar, outcomes) {
  userVar <- scalarText(userVar)
  timeVar <- scalarText(timeVar)
  outcomes <- intersect(outcomes, names(data))
  shiny::validate(shiny::need(hasScalarText(userVar) && hasScalarText(timeVar) && all(c(userVar, timeVar) %in% names(data)), "Select user and time variables."))
  shiny::validate(shiny::need(length(outcomes) > 0, "Select at least one outcome."))

  plotData <- data[, c(userVar, timeVar, outcomes), drop = FALSE] |>
    tidyr::pivot_longer(dplyr::all_of(outcomes), names_to = "variable", values_to = "value")

  hasMultipleTimes <- length(unique(stats::na.omit(as.character(plotData[[timeVar]])))) > 1
  p <- ggplot2::ggplot(plotData, ggplot2::aes(.data[[timeVar]], value, group = .data[[userVar]])) +
    ggplot2::geom_point(alpha = 0.25, na.rm = TRUE) +
    ggplot2::stat_summary(ggplot2::aes(group = 1), fun = mean, geom = "point", size = 2.4, colour = "red", na.rm = TRUE) +
    ggplot2::facet_wrap(~ variable, scales = "free_y") +
    ggplot2::labs(title = "Outcome trajectories across timepoints", x = timeVar, y = "Value") +
    ggplot2::theme_minimal()

  if (isTRUE(hasMultipleTimes)) {
    p <- p +
      ggplot2::geom_line(alpha = 0.12, na.rm = TRUE) +
      ggplot2::stat_summary(ggplot2::aes(group = 1), fun = mean, geom = "line", linewidth = 1.1, colour = "red", na.rm = TRUE)
  }

  p
}
