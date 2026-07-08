# Purpose: UI for binary variable characterisation.
# Arguments: Module id.
# Returns: Shiny UI.
binaryUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Binary"),
    controlCard(
      dropdownInput(ns("binaryVar"), "Binary Variable", choices = NULL),
      dropdownInput(ns("targetLevel"), "Target Level", choices = NULL),
      dropdownInput(ns("groupVar"), "Group Variable", choices = NULL),
      shiny::numericInput(ns("confLevel"), "Confidence Level", value = 0.95, min = 0.5, max = 0.999, step = 0.01),
      dropdownInput(ns("imageFormat"), "Plot Format", choices = c("png", "jpg", "tiff")),
      shiny::downloadButton(ns("downloadPlot"), "Download plot", class = "control-action")
    ),
    expandablePlotCard(
      "Binary summary",
      ns("binaryPlot"),
      ns("expandBinary"),
      height = 420,
      DT::DTOutput(ns("binaryTable")),
      shiny::downloadButton(ns("downloadTable"), "Download binary table")
    )
  )
}

# Purpose: Server for binary variable characterisation.
# Arguments: Module id and clean data reactive.
# Returns: None.
binaryServer <- function(id, cleanData, fileName = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    catVars <- shiny::reactive({
      shiny::req(cleanData())
      getCategoricalVars(cleanData())
    })

    shiny::observeEvent(cleanData(), {
      vars <- catVars()
      selectedBinary <- if ("gender" %in% vars) "gender" else vars[1]
      shiny::updateSelectInput(session, "binaryVar", choices = vars, selected = selectedBinary)
      shiny::updateSelectInput(session, "groupVar", choices = vars, selected = if ("healthtype" %in% vars) "healthtype" else vars[min(2, length(vars))])
    })

    shiny::observeEvent(input$binaryVar, {
      shiny::req(cleanData(), input$binaryVar)
      levels <- sort(unique(as.character(cleanData()[[input$binaryVar]])))
      levels <- levels[!is.na(levels)]
      shiny::updateSelectInput(session, "targetLevel", choices = levels, selected = if ("Female" %in% levels) "Female" else levels[1])
    })

    binaryTableRx <- shiny::reactive({
      shiny::req(cleanData(), input$binaryVar, input$targetLevel, input$groupVar)
      shiny::validate(shiny::need(input$binaryVar != input$groupVar, "Choose different binary and grouping variables."))
      binaryByGroup(cleanData(), input$binaryVar, input$groupVar, input$targetLevel, input$confLevel)
    })

    binaryPlot <- shiny::reactive({
      plotBinaryCi(binaryTableRx())
    })

    output$binaryPlot <- shiny::renderPlot({
      shiny::validate(shiny::need(length(catVars()) >= 2, "At least two categorical variables are required."))
      binaryPlot()
    }, res = appPlotResolution())

    observeExpandedPlot(
      input,
      output,
      session,
      "expandBinary",
      "binaryPlotFull",
      "Binary summary",
      function() {
        shiny::validate(shiny::need(length(catVars()) >= 2, "At least two categorical variables are required."))
        binaryPlot()
      }
    )

    output$binaryTable <- DT::renderDT({
      DT::datatable(binaryTableRx(), options = list(scrollX = TRUE, pageLength = 12))
    })

    output$downloadTable <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "BINARY", "csv"),
      content = function(file) utils::write.csv(binaryTableRx(), file, row.names = FALSE)
    )

    output$downloadPlot <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "BINARY", input$imageFormat),
      content = function(file) savePlotFile(binaryPlot(), file, input$imageFormat)
    )
  })
}
