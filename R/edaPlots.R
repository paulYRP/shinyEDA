# Purpose: Plot missing values by variable.
# Arguments: Missing summary table.
# Returns: ggplot object.
plotMissingSummary <- function(missingTable) {
  ggplot2::ggplot(missingTable, ggplot2::aes(reorder(variable, nMissing), nMissing)) +
    ggplot2::geom_col(fill = "#4b6cb7") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Missing values", title = "Missing values by variable") +
    ggplot2::theme_minimal()
}

# Purpose: Plot a continuous variable distribution.
# Arguments: Data frame, variable name, log-transform flag.
# Returns: ggplot object.
plotNumericDistribution <- function(data, variable, logTransform = FALSE) {
  x <- data[[variable]]
  plotData <- data.frame(value = as.numeric(x))
  if (logTransform) {
    plotData <- plotData[plotData$value > 0, , drop = FALSE]
    plotData$value <- log(plotData$value)
  }

  ggplot2::ggplot(plotData, ggplot2::aes(value)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 30,
                            fill = "#8fb3ff", colour = "white") +
    ggplot2::geom_density(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::labs(
      x = if (logTransform) paste0("log(", variable, ")") else variable,
      y = "Density",
      title = paste("Distribution of", variable)
    ) +
    ggplot2::theme_minimal()
}

# Purpose: Plot categorical proportions.
# Arguments: Data frame and categorical variable.
# Returns: ggplot object.
plotCategoricalProportion <- function(data, variable) {
  plotData <- data |>
    dplyr::count(.data[[variable]], name = "n") |>
    dplyr::mutate(prop = n / sum(n))

  ggplot2::ggplot(plotData, ggplot2::aes(reorder(as.character(.data[[variable]]), prop), prop)) +
    ggplot2::geom_col(fill = "#6aa84f") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = variable, y = "Proportion", title = paste("Proportional distribution:", variable)) +
    ggplot2::theme_minimal()
}

# Purpose: Plot question response proportions.
# Arguments: Data frame and question variable names.
# Returns: ggplot object.
plotQuestionResponses <- function(data, questionVars) {
  vars <- intersect(questionVars, names(data))
  shiny::validate(shiny::need(length(vars) > 0, "No question-style columns were found."))

  plotData <- data |>
    tidyr::pivot_longer(dplyr::all_of(vars), names_to = "Question", values_to = "Response") |>
    dplyr::mutate(Question = factor(Question, levels = vars))

  ggplot2::ggplot(plotData, ggplot2::aes(Question, fill = as.factor(Response))) +
    ggplot2::geom_bar(position = "fill") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(title = "Response distribution across questions", y = "Percentage", fill = "Response") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

# Purpose: Plot binary proportions with confidence intervals.
# Arguments: Binary summary table.
# Returns: ggplot object.
plotBinaryCi <- function(binaryTable) {
  ggplot2::ggplot(binaryTable, ggplot2::aes(reorder(groupLevel, estimate), estimate)) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = loCI, ymax = upCI), width = 0.15) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::labs(x = NULL, y = "Estimated probability", title = "Binary estimate with confidence interval") +
    ggplot2::theme_minimal()
}

# Purpose: Plot a discrete variable frequency distribution.
# Arguments: Data frame and variable name.
# Returns: ggplot object.
plotDiscreteFrequency <- function(data, variable) {
  plotData <- data |>
    dplyr::count(.data[[variable]], name = "n")

  ggplot2::ggplot(plotData, ggplot2::aes(.data[[variable]], n)) +
    ggplot2::geom_segment(ggplot2::aes(xend = .data[[variable]], y = 0, yend = n), linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(x = variable, y = "Frequency", title = paste("Frequency:", variable)) +
    ggplot2::theme_minimal()
}

# Purpose: Plot one numeric variable by one grouping variable.
# Arguments: Data frame, numeric variable, grouping variable.
# Returns: ggplot object.
plotBoxByGroup <- function(data, numericVar, groupVar) {
  ggplot2::ggplot(data, ggplot2::aes(as.factor(.data[[groupVar]]), .data[[numericVar]])) +
    ggplot2::geom_boxplot(fill = "#f6b26b", na.rm = TRUE) +
    ggplot2::labs(x = groupVar, y = numericVar, title = paste(numericVar, "by", groupVar)) +
    ggplot2::theme_minimal()
}

# Purpose: Plot relationship between two numeric variables.
# Arguments: Data frame, x variable, y variable, optional colour/group variable.
# Returns: ggplot object.
plotScatter <- function(data, xVar, yVar, colourVar = NULL, logScale = FALSE) {
  p <- ggplot2::ggplot(data, ggplot2::aes(.data[[xVar]], .data[[yVar]]))
  if (!is.null(colourVar) && colourVar %in% names(data)) {
    p <- ggplot2::ggplot(data, ggplot2::aes(.data[[xVar]], .data[[yVar]], colour = as.factor(.data[[colourVar]])))
  }

  p <- p +
    ggplot2::geom_point(alpha = 0.65, na.rm = TRUE) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, na.rm = TRUE) +
    ggplot2::labs(x = xVar, y = yVar, colour = colourVar, title = paste(yVar, "vs", xVar)) +
    ggplot2::theme_minimal()

  if (logScale) {
    p <- p + ggplot2::scale_x_log10() + ggplot2::scale_y_log10()
  }

  p
}

# Purpose: Plot outlier counts by method.
# Arguments: Outlier summary table.
# Returns: ggplot object.
plotOutlierSummary <- function(outlierSummaryTable) {
  ggplot2::ggplot(outlierSummaryTable, ggplot2::aes(variable, nOut, fill = method)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Number of outliers", title = "Outliers by variable and method") +
    ggplot2::theme_minimal()
}
