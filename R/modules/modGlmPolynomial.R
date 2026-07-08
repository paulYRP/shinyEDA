# Purpose: UI for GLM polynomial checks.
# Arguments: Module id.
# Returns: Shiny UI.
glmPolynomialUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("GLM polynomial"),
    controlCard(
      dropdownInput(ns("outcome"), "Outcome Variable", choices = NULL),
      checkboxDropdownInput(ns("predictors"), "Numeric Predictors", choices = NULL, placeholder = "Select predictors"),
      dropdownInput(ns("degree"), "Polynomial Degree", choices = c("Quadratic" = 2, "Cubic" = 3), selected = 2)
    ),
    expandablePlotCard(
      "Polynomial plot",
      ns("polyPlot"),
      ns("expandPoly"),
      height = 560
    ),
    bslib::card(
      bslib::card_header("Linear, quadratic and cubic comparison"),
      DT::DTOutput(ns("polyTable"))
    )
  )
}

# Purpose: Server for GLM polynomial checks.
# Arguments: Module id and prepared model data reactive.
# Returns: None.
glmPolynomialServer <- function(id, modelData) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(modelData(), {
      dat <- modelData()
      numericVars <- setdiff(getNumericVars(dat), c("index", "user_id", "timepoint"))
      outcomes <- defaultAnalysisOutcomes(dat)
      selectedOutcome <- if (length(outcomes) > 0) outcomes[1] else numericVars[1]
      selectedPredictors <- setdiff(numericVars, selectedOutcome)
      shiny::updateSelectInput(session, "outcome", choices = numericVars, selected = selectedOutcome)
      shiny::updateCheckboxGroupInput(session, "predictors", choices = numericVars, selected = selectedPredictors)
    })

    output$polyPlot <- shiny::renderPlot({
      shiny::req(modelData(), input$outcome, input$predictors)
      plotPolyRelationships(modelData(), input$outcome, input$predictors, as.integer(input$degree), maxPredictors = appPreviewFacetLimit())
    }, res = appPlotResolution())

    observeExpandedPlot(
      input,
      output,
      session,
      "expandPoly",
      "polyPlotFull",
      "Polynomial plot",
      function() {
        shiny::req(modelData(), input$outcome, input$predictors)
        plotPolyRelationships(modelData(), input$outcome, input$predictors, as.integer(input$degree))
      },
      height = function() {
        nVars <- length(input$predictors)
        paste0(max(760, min(1800, ceiling(nVars / 2) * 360)), "px")
      }
    )

    output$polyTable <- DT::renderDT({
      shiny::req(modelData(), input$outcome, input$predictors)
      DT::datatable(fitPolyLmTable(modelData(), input$outcome, input$predictors), options = list(scrollX = TRUE, pageLength = 15))
    })
  })
}
