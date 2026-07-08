# Purpose: UI for longitudinal polynomial checks.
# Arguments: Module id.
# Returns: Shiny UI.
lmePolynomialUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("LME polynomial"),
    controlCard(
      dropdownInput(ns("outcome"), "Outcome Variable", choices = NULL),
      checkboxDropdownInput(ns("predictors"), "Numeric Predictors", choices = NULL, placeholder = "Select predictors"),
      dropdownInput(ns("degree"), "Polynomial Degree", choices = c("Quadratic" = 2, "Cubic" = 3), selected = 2)
    ),
    expandablePlotCard(
      "Polynomial plot by timepoint",
      ns("polyPlot"),
      ns("expandPoly"),
      height = 560
    ),
    bslib::card(
      bslib::card_header("LME polynomial comparison"),
      DT::DTOutput(ns("polyTable"))
    )
  )
}

# Purpose: Server for longitudinal polynomial checks.
# Arguments: Module id, prepared data and user/time variable reactives.
# Returns: None.
lmePolynomialServer <- function(id, modelData, userVar, timeVar) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(modelData(), {
      dat <- modelData()
      numericVars <- setdiff(getNumericVars(dat), c("index", "user_id", "timepoint"))
      outcomes <- intersect(defaultAnalysisOutcomes(dat), numericVars)
      selectedOutcome <- if (length(outcomes) > 0) {
        outcomes[1]
      } else if (length(numericVars) > 0) {
        numericVars[1]
      } else {
        ""
      }
      selectedPredictors <- setdiff(numericVars, selectedOutcome)
      shiny::updateSelectInput(session, "outcome", choices = numericVars, selected = selectedOutcome)
      shiny::updateCheckboxGroupInput(session, "predictors", choices = numericVars, selected = selectedPredictors)
    })

    selectedSettings <- shiny::reactive({
      dat <- modelData()
      numericVars <- setdiff(getNumericVars(dat), c("index", "user_id", "timepoint"))
      outcomes <- intersect(defaultAnalysisOutcomes(dat), numericVars)
      outcome <- scalarText(input$outcome, if (length(outcomes) > 0) outcomes[1] else if (length(numericVars) > 0) numericVars[1] else "")
      predictors <- input$predictors
      if (is.null(predictors) || length(predictors) == 0) {
        predictors <- setdiff(numericVars, outcome)
      }
      degree <- suppressWarnings(as.integer(scalarText(input$degree, "2")))
      if (is.na(degree)) {
        degree <- 2L
      }
      list(
        outcome = outcome,
        predictors = predictors,
        degree = degree,
        userVar = scalarText(userVar(), if ("user_id" %in% names(dat)) "user_id" else ""),
        timeVar = scalarText(timeVar(), if ("timepoint" %in% names(dat)) "timepoint" else "")
      )
    })

    output$polyPlot <- shiny::renderPlot({
      shiny::req(modelData())
      settings <- selectedSettings()
      plotPolyRelationships(modelData(), settings$outcome, settings$predictors, settings$degree, colourVar = settings$timeVar, maxPredictors = appPreviewFacetLimit())
    }, res = appPlotResolution())

    observeExpandedPlot(
      input,
      output,
      session,
      "expandPoly",
      "polyPlotFull",
      "Polynomial plot by timepoint",
      function() {
        shiny::req(modelData())
        settings <- selectedSettings()
        plotPolyRelationships(modelData(), settings$outcome, settings$predictors, settings$degree, colourVar = settings$timeVar)
      },
      height = function() {
        nVars <- length(selectedSettings()$predictors)
        paste0(max(760, min(1800, ceiling(nVars / 2) * 360)), "px")
      }
    )

    output$polyTable <- DT::renderDT({
      shiny::req(modelData())
      settings <- selectedSettings()
      shiny::validate(shiny::need(requireNamespace("lme4", quietly = TRUE) || requireNamespace("lmerTest", quietly = TRUE), "Install lme4 or lmerTest to fit LME polynomial models."))
      DT::datatable(fitPolyLmeTable(modelData(), settings$outcome, settings$predictors, settings$userVar, settings$timeVar), options = list(scrollX = TRUE, pageLength = 15))
    })
  })
}
