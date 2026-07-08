# Purpose: UI for PCA score plots and loading tables.
# Arguments: Module id and title.
# Returns: Shiny UI.
analysisPcaUi <- function(id, title = "PCA and loadings") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3(title),
    controlCard(
      dropdownInput(ns("referenceVersion"), "Reference Version", choices = analysisOutlierChoices(), selected = "none"),
      checkboxDropdownInput(ns("comparedVersions"), "Compared Versions", choices = analysisOutlierChoices(), placeholder = "Select versions"),
      checkboxDropdownInput(ns("numericVars"), "Numeric Variables", choices = NULL, placeholder = "Select variables"),
      dropdownInput(ns("imageFormat"), "Plot Format", choices = c("png", "jpg", "tiff"))
    ),
    expandablePlotCard(
      "PCA score plot",
      ns("pcaPlot"),
      ns("expandPca"),
      height = 520,
      shiny::downloadButton(ns("downloadPlot"), "Download PCA plot")
    ),
    bslib::card(
      bslib::card_header("Loadings table"),
      DT::DTOutput(ns("loadingsTable")),
      shiny::downloadButton(ns("downloadLoadings"), "Download loadings")
    )
  )
}

# Purpose: Server for PCA score plots and loading tables.
# Arguments: Module id, prepared dataset versions and source file-name reactive.
# Returns: None.
analysisPcaServer <- function(id, modelDataVersions, fileName = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(modelDataVersions(), {
      versions <- modelDataVersions()
      choices <- analysisOutlierChoices()
      choices <- choices[unname(choices) %in% names(versions)]
      if (length(choices) == 0) {
        return()
      }
      selectedVersions <- unname(choices)
      selectedReference <- if ("none" %in% selectedVersions) "none" else selectedVersions[1]
      vars <- defaultDiagnosticNumericVars(versions[[selectedReference]])

      shiny::updateSelectInput(session, "referenceVersion", choices = choices, selected = selectedReference)
      shiny::updateCheckboxGroupInput(session, "comparedVersions", choices = choices, selected = selectedVersions)
      shiny::updateCheckboxGroupInput(session, "numericVars", choices = vars, selected = vars)
    })

    pcaResult <- shiny::reactive({
      versions <- modelDataVersions()
      shiny::req(versions)
      versionNames <- names(versions)
      shiny::req(length(versionNames) > 0)
      referenceVersion <- scalarText(input$referenceVersion, if ("none" %in% versionNames) "none" else versionNames[1])
      comparedVersions <- input$comparedVersions
      if (is.null(comparedVersions) || length(comparedVersions) == 0) {
        comparedVersions <- versionNames
      }
      numericVars <- input$numericVars
      if (is.null(numericVars) || length(numericVars) == 0 || !referenceVersion %in% versionNames) {
        referenceForVars <- if (referenceVersion %in% versionNames) referenceVersion else versionNames[1]
        numericVars <- defaultDiagnosticNumericVars(versions[[referenceForVars]])
      }
      fitPcaComparison(
        versions,
        numericVars,
        referenceVersion,
        comparedVersions
      )
    })

    output$pcaPlot <- shiny::renderPlot({
      plotPcaComparison(pcaResult())
    }, res = appPlotResolution())

    observeExpandedPlot(
      input,
      output,
      session,
      "expandPca",
      "pcaPlotFull",
      "PCA score plot",
      function() plotPcaComparison(pcaResult()),
      height = "820px"
    )

    output$loadingsTable <- DT::renderDT({
      shiny::validate(shiny::need(!is.null(pcaResult()), "PCA requires at least two numeric variables with enough complete rows."))
      tab <- pcaResult()$loadings
      numCols <- vapply(tab, is.numeric, logical(1))
      tab[numCols] <- lapply(tab[numCols], round, 4)
      DT::datatable(tab, options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE)
    })

    output$downloadPlot <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "PCA", input$imageFormat),
      content = function(file) savePlotFile(plotPcaComparison(pcaResult()), file, input$imageFormat, width = 10, height = 7)
    )

    output$downloadLoadings <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "PCALOADINGS", "csv"),
      content = function(file) utils::write.csv(pcaResult()$loadings, file, row.names = FALSE)
    )
  })
}

# Purpose: UI for density inspection across dataset versions.
# Arguments: Module id and title.
# Returns: Shiny UI.
analysisDensityUi <- function(id, title = "Density inspection") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3(title),
    controlCard(
      checkboxDropdownInput(ns("comparedVersions"), "Compared Versions", choices = analysisOutlierChoices(), placeholder = "Select versions"),
      checkboxDropdownInput(ns("numericVars"), "Numeric Variables", choices = NULL, placeholder = "Select variables"),
      dropdownInput(ns("imageFormat"), "Plot Format", choices = c("png", "jpg", "tiff"))
    ),
    expandablePlotCard(
      "Density comparison",
      ns("densityPlot"),
      ns("expandDensity"),
      height = 620,
      shiny::downloadButton(ns("downloadDensity"), "Download density plot")
    ),
    expandablePlotCard(
      "Log-density comparison",
      ns("logDensityPlot"),
      ns("expandLogDensity"),
      height = 620,
      shiny::downloadButton(ns("downloadLogDensity"), "Download log-density plot")
    )
  )
}

# Purpose: Server for density inspection across dataset versions.
# Arguments: Module id, prepared dataset versions and source file-name reactive.
# Returns: None.
analysisDensityServer <- function(id, modelDataVersions, fileName = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(modelDataVersions(), {
      versions <- modelDataVersions()
      choices <- analysisOutlierChoices()
      choices <- choices[unname(choices) %in% names(versions)]
      if (length(choices) == 0) {
        return()
      }
      selectedVersions <- unname(choices)
      referenceVersion <- if ("none" %in% selectedVersions) "none" else selectedVersions[1]
      vars <- defaultDiagnosticNumericVars(versions[[referenceVersion]])

      shiny::updateCheckboxGroupInput(session, "comparedVersions", choices = choices, selected = selectedVersions)
      shiny::updateCheckboxGroupInput(session, "numericVars", choices = vars, selected = vars)
    })

    densityPreview <- shiny::reactive({
      versions <- modelDataVersions()
      shiny::req(versions)
      comparedVersions <- input$comparedVersions
      if (is.null(comparedVersions) || length(comparedVersions) == 0) {
        comparedVersions <- names(versions)
      }
      numericVars <- input$numericVars
      if (is.null(numericVars) || length(numericVars) == 0) {
        referenceVersion <- if ("none" %in% names(versions)) "none" else names(versions)[1]
        numericVars <- defaultDiagnosticNumericVars(versions[[referenceVersion]])
      }
      buildDensityComparisonData(versions, numericVars, comparedVersions, maxVars = appPreviewFacetLimit())
    })

    densityFull <- shiny::reactive({
      versions <- modelDataVersions()
      shiny::req(versions)
      comparedVersions <- input$comparedVersions
      if (is.null(comparedVersions) || length(comparedVersions) == 0) {
        comparedVersions <- names(versions)
      }
      numericVars <- input$numericVars
      if (is.null(numericVars) || length(numericVars) == 0) {
        referenceVersion <- if ("none" %in% names(versions)) "none" else names(versions)[1]
        numericVars <- defaultDiagnosticNumericVars(versions[[referenceVersion]])
      }
      buildDensityComparisonData(versions, numericVars, comparedVersions)
    })

    logDensityPreview <- shiny::reactive({
      versions <- modelDataVersions()
      shiny::req(versions)
      comparedVersions <- input$comparedVersions
      if (is.null(comparedVersions) || length(comparedVersions) == 0) {
        comparedVersions <- names(versions)
      }
      numericVars <- input$numericVars
      if (is.null(numericVars) || length(numericVars) == 0) {
        referenceVersion <- if ("none" %in% names(versions)) "none" else names(versions)[1]
        numericVars <- defaultDiagnosticNumericVars(versions[[referenceVersion]])
      }
      buildDensityComparisonData(versions, numericVars, comparedVersions, logTransform = TRUE, maxVars = appPreviewFacetLimit())
    })

    logDensityFull <- shiny::reactive({
      versions <- modelDataVersions()
      shiny::req(versions)
      comparedVersions <- input$comparedVersions
      if (is.null(comparedVersions) || length(comparedVersions) == 0) {
        comparedVersions <- names(versions)
      }
      numericVars <- input$numericVars
      if (is.null(numericVars) || length(numericVars) == 0) {
        referenceVersion <- if ("none" %in% names(versions)) "none" else names(versions)[1]
        numericVars <- defaultDiagnosticNumericVars(versions[[referenceVersion]])
      }
      buildDensityComparisonData(versions, numericVars, comparedVersions, logTransform = TRUE)
    })

    output$densityPlot <- shiny::renderPlot({
      plotDensityComparison(densityPreview())
    }, res = appPlotResolution())

    observeExpandedPlot(
      input,
      output,
      session,
      "expandDensity",
      "densityPlotFull",
      "Density comparison",
      function() plotDensityComparison(densityFull()),
      height = function() paste0(max(760, min(2200, length(unique(densityFull()$variable)) * 180)), "px")
    )

    output$logDensityPlot <- shiny::renderPlot({
      plotDensityComparison(logDensityPreview(), logTransform = TRUE)
    }, res = appPlotResolution())

    observeExpandedPlot(
      input,
      output,
      session,
      "expandLogDensity",
      "logDensityPlotFull",
      "Log-density comparison",
      function() plotDensityComparison(logDensityFull(), logTransform = TRUE),
      height = function() paste0(max(760, min(2200, length(unique(logDensityFull()$variable)) * 180)), "px")
    )

    output$downloadDensity <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "DENSITY", input$imageFormat),
      content = function(file) savePlotFile(plotDensityComparison(densityFull()), file, input$imageFormat, width = 10, height = 12)
    )

    output$downloadLogDensity <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "LOGDENSITY", input$imageFormat),
      content = function(file) savePlotFile(plotDensityComparison(logDensityFull(), logTransform = TRUE), file, input$imageFormat, width = 10, height = 12)
    )
  })
}
