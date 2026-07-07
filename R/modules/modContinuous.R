# Purpose: UI for continuous variable characterisation.
# Arguments: Module id.
# Returns: Shiny UI.
continuousUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Continuous variables"),
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        bslib::card_header("Controls"),
        shiny::selectInput(ns("continuousVar"), "Continuous variable", choices = NULL),
        shiny::checkboxInput(ns("logTransform"), "Use log transformation", value = FALSE),
        shiny::selectInput(ns("imageFormat"), "Plot download format", choices = c("png", "jpg", "tiff")),
        shiny::downloadButton(ns("downloadPlot"), "Download plot")
      ),
      bslib::card(
        bslib::card_header("Distribution diagnostics"),
        shiny::plotOutput(ns("distPlot"), height = 420),
        DT::DTOutput(ns("distTable")),
        shiny::downloadButton(ns("downloadTable"), "Download numeric table")
      )
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
    })

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
