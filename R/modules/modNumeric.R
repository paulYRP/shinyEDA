# Purpose: UI for numeric-variable exploration.
# Arguments: Module id.
# Returns: Shiny UI.
numericUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Numeric variables"),
    controlCard(
      dropdownInput(ns("numericVar"), "Numeric Variable", choices = NULL),
      shiny::checkboxInput(ns("logTransform"), "Use Log Transform", value = FALSE),
      dropdownInput(ns("imageFormat"), "Plot Format", choices = c("png", "jpg", "tiff")),
      shiny::downloadButton(ns("downloadPlot"), "Download plot", class = "control-action")
    ),
    expandablePlotCard(
      "Distribution",
      ns("distPlot"),
      ns("expandDist"),
      height = 420,
      DT::DTOutput(ns("distTable")),
      shiny::downloadButton(ns("downloadTable"), "Download distribution table")
    )
  )
}

# Purpose: Server for numeric-variable exploration.
# Arguments: Module id and clean data reactive.
# Returns: None.
numericServer <- function(id, cleanData, fileName = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    numericVars <- shiny::reactive({
      shiny::req(cleanData())
      getNumericVars(cleanData())
    })

    shiny::observeEvent(numericVars(), {
      shiny::updateSelectInput(session, "numericVar", choices = numericVars())
    })

    distTable <- shiny::reactive({
      shiny::req(cleanData())
      distributionTable(cleanData()[numericVars()])
    })

    currentPlot <- shiny::reactive({
      shiny::req(cleanData(), input$numericVar)
      plotNumericDistribution(cleanData(), input$numericVar, input$logTransform)
    })

    output$distPlot <- shiny::renderPlot({
      shiny::validate(shiny::need(length(numericVars()) > 0, "No numeric variables available."))
      currentPlot()
    }, res = appPlotResolution())

    observeExpandedPlot(
      input,
      output,
      session,
      "expandDist",
      "distPlotFull",
      "Distribution",
      function() {
        shiny::validate(shiny::need(length(numericVars()) > 0, "No numeric variables available."))
        currentPlot()
      }
    )

    output$distTable <- DT::renderDT({
      DT::datatable(distTable(), options = list(scrollX = TRUE, pageLength = 10))
    })

    output$downloadTable <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "NUMERIC", "csv"),
      content = function(file) utils::write.csv(distTable(), file, row.names = FALSE)
    )

    output$downloadPlot <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "NUMERIC", input$imageFormat),
      content = function(file) savePlotFile(currentPlot(), file, input$imageFormat)
    )
  })
}
