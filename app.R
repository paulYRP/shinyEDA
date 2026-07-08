# Purpose: Source all R files from one app folder.
# Arguments: Relative directory path.
# Returns: Invisibly returns sourced file paths.
sourceAppDir <- function(path) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  files <- sort(files)
  invisible(lapply(files, source, local = FALSE))
}

sourceAppDir("R")
loadAppPackages()
sourceAppDir(file.path("R", "modules"))

# Purpose: Return UI for the currently selected navigation section.
# Arguments: Navigation key.
# Returns: Shiny UI for one section only.
sectionUi <- function(key) {
  switch(
    key,
    home = homeUi("home"),
    setup = dataUi("data"),
    explorationOverview = overviewUi("overview"),
    numericVariables = numericUi("numeric"),
    explorationCategorical = categoricalUi("explorationCategorical"),
    conditioningVariable = conditioningUi("conditioning"),
    binaryVariables = binaryUi("binary"),
    characterCategorical = categoricalUi("characterCategorical"),
    discreteVariables = discreteUi("discrete"),
    continuousVariables = continuousUi("continuous"),
    outliersDetection = outliersUi("outliers"),
    inliersDetection = inliersUi("inliers"),
    glmPrepare = glmPrepareUi("glmPrepare"),
    glmPca = analysisPcaUi("glmPca", "GLM PCA and loadings"),
    glmDensity = analysisDensityUi("glmDensity", "GLM density inspection"),
    glmCorrelation = glmCorrelationUi("glmCorrelation"),
    glmPolynomial = glmPolynomialUi("glmPolynomial"),
    glmRegression = glmRegressionUi("glmRegression"),
    lmePrepare = lmePrepareUi("lmePrepare"),
    lmePca = analysisPcaUi("lmePca", "LME PCA and loadings"),
    lmeDensity = analysisDensityUi("lmeDensity", "LME density inspection"),
    lmeCorrelation = lmeCorrelationUi("lmeCorrelation"),
    lmePolynomial = lmePolynomialUi("lmePolynomial"),
    lmeModels = lmeModelsUi("lmeModels"),
    dictionary = dictionaryUi("dictionary"),
    homeUi("home")
  )
}

# Purpose: Render the top-right display mode control.
# Arguments: None.
# Returns: Shiny UI.
topBarUi <- function() {
  shiny::div(
    class = "app-topbar",
    shiny::div(
      class = "top-theme-control",
      bslib::input_dark_mode(id = "themeMode", mode = "light")
    )
  )
}

ui <- bslib::page_fillable(
  title = "shinyEDA",
  theme = appTheme("light"),
  shiny::tags$head(
    shiny::includeCSS(file.path("www", "styles.css")),
    shiny::includeScript(file.path("www", "app.js"))
  ),
  shiny::div(
    class = "app-shell",
    shiny::tags$aside(class = "app-sidebar", sidebarUi("sidebar", getEdaNav())),
    shiny::tags$main(class = "app-main", topBarUi(), shiny::uiOutput("activePage"))
  )
)

server <- function(input, output, session) {
  nav <- getEdaNav()
  selected <- sidebarServer("sidebar", nav)
  moduleLoaded <- new.env(parent = emptyenv())
  appState <- new.env(parent = emptyenv())

  shiny::observeEvent(input$themeMode, {
    mode <- input$themeMode
    if (is.logical(mode)) {
      mode <- if (isTRUE(mode)) "dark" else "light"
    }

    if (is.null(mode) || length(mode) == 0) {
      mode <- "light"
    }

    mode <- tolower(mode[[1]])
    if (!mode %in% c("light", "dark")) {
      mode <- "light"
    }

    if (is.function(session$setCurrentTheme)) {
      session$setCurrentTheme(appTheme(mode))
    }
  }, ignoreInit = FALSE)

  dataState <- dataServer("data", selected)

  # Purpose: Check whether a module server has already been registered.
  # Arguments: Module key.
  # Returns: TRUE when the module was already loaded.
  isLoaded <- function(key) {
    isTRUE(moduleLoaded[[key]])
  }

  # Purpose: Mark a module server as registered.
  # Arguments: Module key.
  # Returns: Invisibly returns NULL.
  markLoaded <- function(key) {
    moduleLoaded[[key]] <- TRUE
    invisible(NULL)
  }

  # Purpose: Register outlier detection only when a section needs it.
  # Arguments: None.
  # Returns: Outlier-state reactive list.
  ensureOutliers <- function() {
    if (!isLoaded("outliersDetection")) {
      appState$outliers <- outliersServer("outliers", dataState$cleanData, dataState$activeFileName)
      markLoaded("outliersDetection")
    }
    appState$outliers
  }

  # Purpose: Register GLM preparation and its outlier dependency.
  # Arguments: None.
  # Returns: GLM preparation state.
  ensureGlmPrepare <- function() {
    if (!isLoaded("glmPrepare")) {
      appState$glm <- glmPrepareServer("glmPrepare", dataState$cleanData, ensureOutliers())
      markLoaded("glmPrepare")
    }
    appState$glm
  }

  # Purpose: Register LME preparation and its outlier dependency.
  # Arguments: None.
  # Returns: LME preparation state.
  ensureLmePrepare <- function() {
    if (!isLoaded("lmePrepare")) {
      appState$lme <- lmePrepareServer("lmePrepare", dataState$cleanData, ensureOutliers())
      markLoaded("lmePrepare")
    }
    appState$lme
  }

  # Purpose: Lazily register server logic for the selected section.
  # Arguments: Navigation key.
  # Returns: Invisibly returns NULL.
  ensureSectionServer <- function(key) {
    if (isLoaded(key)) {
      return(invisible(NULL))
    }

    switch(
      key,
      home = homeServer("home"),
      setup = NULL,
      explorationOverview = overviewServer("overview", dataState$cleanData, dataState$abnormalTable, dataState$activeFileName),
      numericVariables = numericServer("numeric", dataState$cleanData, dataState$activeFileName),
      explorationCategorical = categoricalServer("explorationCategorical", dataState$cleanData, dataState$activeFileName),
      conditioningVariable = conditioningServer("conditioning", dataState$cleanData, dataState$activeFileName),
      binaryVariables = binaryServer("binary", dataState$cleanData, dataState$activeFileName),
      characterCategorical = categoricalServer("characterCategorical", dataState$cleanData, dataState$activeFileName),
      discreteVariables = discreteServer("discrete", dataState$cleanData, dataState$activeFileName),
      continuousVariables = continuousServer("continuous", dataState$cleanData, dataState$activeFileName),
      outliersDetection = ensureOutliers(),
      inliersDetection = inliersServer("inliers", dataState$cleanData, dataState$activeFileName),
      glmPrepare = ensureGlmPrepare(),
      glmPca = analysisPcaServer("glmPca", ensureGlmPrepare()$modelDataVersions, dataState$activeFileName),
      glmDensity = analysisDensityServer("glmDensity", ensureGlmPrepare()$modelDataVersions, dataState$activeFileName),
      glmCorrelation = glmCorrelationServer("glmCorrelation", ensureGlmPrepare()$modelData),
      glmPolynomial = glmPolynomialServer("glmPolynomial", ensureGlmPrepare()$modelData),
      glmRegression = glmRegressionServer("glmRegression", ensureGlmPrepare()$modelDataVersions),
      lmePrepare = ensureLmePrepare(),
      lmePca = analysisPcaServer("lmePca", ensureLmePrepare()$modelDataVersions, dataState$activeFileName),
      lmeDensity = analysisDensityServer("lmeDensity", ensureLmePrepare()$modelDataVersions, dataState$activeFileName),
      lmeCorrelation = lmeCorrelationServer("lmeCorrelation", ensureLmePrepare()$modelData, ensureLmePrepare()$timeVar),
      lmePolynomial = lmePolynomialServer("lmePolynomial", ensureLmePrepare()$modelData, ensureLmePrepare()$userVar, ensureLmePrepare()$timeVar),
      lmeModels = lmeModelsServer("lmeModels", ensureLmePrepare()$modelDataVersions, ensureLmePrepare()$userVar, ensureLmePrepare()$timeVar),
      dictionary = dictionaryServer("dictionary", dataState$cleanData, dataState$params, ensureOutliers(), dataState$activeFileName, dataState$dictionaryMetadata),
      homeServer("home")
    )

    markLoaded(key)
    invisible(NULL)
  }

  output$activePage <- shiny::renderUI({
    key <- selected()
    ensureSectionServer(key)
    sectionUi(key)
  })

  output$selectedKey <- shiny::renderText({
    selected()
  })
  shiny::outputOptions(output, "selectedKey", suspendWhenHidden = FALSE)
}

shiny::shinyApp(ui, server)
