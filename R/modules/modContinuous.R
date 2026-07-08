# Purpose: UI for continuous variable characterisation.
# Arguments: Module id.
# Returns: Shiny UI.
continuousUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Continuous variables"),
    controlCard(
      dropdownInput(ns("continuousVar"), "Continuous Variable", choices = NULL),
      shiny::checkboxInput(ns("logTransform"), "Use Log Transform", value = FALSE),
      dropdownInput(ns("imageFormat"), "Plot Format", choices = c("png", "jpg", "tiff")),
      shiny::downloadButton(ns("downloadPlot"), "Download plot", class = "control-action")
    ),
    expandablePlotCard(
      "Distribution diagnostics",
      ns("distPlot"),
      ns("expandDist"),
      height = 420,
      DT::DTOutput(ns("distTable")),
      shiny::downloadButton(ns("downloadTable"), "Download numeric table")
    )
  )
}

# Purpose: Server for continuous variable characterisation.
# Arguments: Module id and clean data reactive.
# Returns: None.
continuousServer <- function(id, cleanData, fileName = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    continuousVars <- shiny::reactive({
      shiny::req(cleanData())
      setdiff(getContinuousVars(cleanData()), c("index", "user_id", "timepoint"))
    })

    shiny::observeEvent(continuousVars(), {
      shiny::updateSelectInput(session, "continuousVar", choices = continuousVars())
    })

    distTable <- shiny::reactive({
      shiny::req(cleanData())
      distributionTable(cleanData()[continuousVars()])
    })

    distPlot <- shiny::reactive({
      shiny::req(cleanData(), input$continuousVar)
      plotNumericDistribution(cleanData(), input$continuousVar, input$logTransform)
    })

    output$distPlot <- shiny::renderPlot({
      shiny::validate(shiny::need(length(continuousVars()) > 0, "No continuous variables were found."))
      distPlot()
    }, res = appPlotResolution())

    observeExpandedPlot(
      input,
      output,
      session,
      "expandDist",
      "distPlotFull",
      "Distribution diagnostics",
      function() {
        shiny::validate(shiny::need(length(continuousVars()) > 0, "No continuous variables were found."))
        distPlot()
      }
    )

    output$distTable <- DT::renderDT({
      DT::datatable(distTable(), options = list(scrollX = TRUE, pageLength = 10))
    })

    output$downloadTable <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "CONTINUOUS", "csv"),
      content = function(file) utils::write.csv(distTable(), file, row.names = FALSE)
    )

    output$downloadPlot <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "CONTINUOUS", input$imageFormat),
      content = function(file) savePlotFile(distPlot(), file, input$imageFormat)
    )
  })
}
