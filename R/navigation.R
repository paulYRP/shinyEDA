# Purpose: Define the app navigation.
# Arguments: None.
# Returns: Data frame with section, subsection, label and key.
getEdaNav <- function() {
  data.frame(
    section = c(
      "Home",
      "Setup",
      "Exploration", "Exploration", "Exploration", "Exploration",
      "Characterising variables", "Characterising variables",
      "Characterising variables", "Characterising variables",
      "Outliers detection", "Inliers detection",
      "GLM analysis", "GLM analysis", "GLM analysis", "GLM analysis",
      "GLM analysis", "GLM analysis",
      "LME analysis", "LME analysis", "LME analysis", "LME analysis",
      "LME analysis", "LME analysis",
      "Dictionary"
    ),
    subsection = c(
      "Home",
      "Setup",
      "Overview", "Numeric variables", "Categorical variables", "Conditioning Variable",
      "Binary", "Categorical variables", "Discrete variables", "Continuous variables",
      "Outliers detection", "Inliers detection",
      "Prepare data", "PCA and loadings", "Density inspection",
      "Correlation", "Polynomial", "Generalized Linear Models",
      "Prepare data", "PCA and loadings", "Density inspection",
      "Correlation", "Polynomial", "Generalized Linear Mixed Effects Models",
      "Dictionary"
    ),
    label = c(
      "Home",
      "Setup",
      "Overview", "Numeric variables", "Categorical variables", "Conditioning Variable",
      "Binary", "Categorical variables", "Discrete variables", "Continuous variables",
      "Outliers detection", "Inliers detection",
      "Prepare data", "PCA and loadings", "Density inspection",
      "Correlation", "Polynomial", "Generalized Linear Models",
      "Prepare data", "PCA and loadings", "Density inspection",
      "Correlation", "Polynomial", "Generalized Linear Mixed Effects Models",
      "Dictionary"
    ),
    key = c(
      "home",
      "setup",
      "explorationOverview", "numericVariables", "explorationCategorical",
      "conditioningVariable", "binaryVariables", "characterCategorical",
      "discreteVariables", "continuousVariables", "outliersDetection",
      "inliersDetection",
      "glmPrepare", "glmPca", "glmDensity", "glmCorrelation", "glmPolynomial", "glmRegression",
      "lmePrepare", "lmePca", "lmeDensity", "lmeCorrelation", "lmePolynomial", "lmeModels",
      "dictionary"
    ),
    stringsAsFactors = FALSE
  )
}

# Purpose: Filter navigation items by search text.
# Arguments: nav data frame; searchText typed by user.
# Returns: Filtered navigation data frame.
filterNav <- function(nav, searchText) {
  if (is.null(searchText) || !nzchar(trimws(searchText))) {
    return(nav)
  }

  q <- tolower(trimws(searchText))
  keep <- grepl(q, tolower(nav$section)) | grepl(q, tolower(nav$subsection))
  nav[keep, , drop = FALSE]
}
