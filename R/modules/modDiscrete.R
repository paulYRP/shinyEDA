# Purpose: UI for discrete variable characterisation.
# Arguments: Module id.
# Returns: Shiny UI.
discreteUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Discrete variables"),
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        bslib::card_header("Controls"),
        shiny::selectInput(ns("discreteVar"), "Discrete variable", choices = NULL),
        shiny::selectInput(ns("imageFormat"), "Plot download format", choices = c("png", "jpg", "tiff")),
        shiny::downloadButton(ns("downloadPlot"), "Download plot")
      ),
      bslib::card(
        bslib::card_header("Frequency and summary"),
        shiny::plotOutput(ns("freqPlot"), height = 420),
        DT::DTOutput(ns("discreteTable")),
        shiny::downloadButton(ns("downloadTable"), "Download discrete table")
      )
    )
  )
}

# Purpose: Server for discrete variable characterisation.
# Arguments: Module id and clean data reactive.
# Returns: None.
discreteServer <- function(id, cleanData, fileName = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    discreteVars <- shiny::reactive({
      shiny::req(cleanData())
      setdiff(getIntegerVars(cleanData()), c("index", "user_id", "timepoint"))
    })

    shiny::observeEvent(discreteVars(), {
      shiny::updateSelectInput(session, "discreteVar", choices = discreteVars())
    })

    discreteTableRx <- shiny::reactive({
      shiny::req(cleanData())
      discreteTable(cleanData()[discreteVars()])
    })

    freqPlot <- shiny::reactive({
      shiny::req(cleanData(), input$discreteVar)
      plotDiscreteFrequency(cleanData(), input$discreteVar)
    })

    output$freqPlot <- shiny::renderPlot({
      shiny::validate(shiny::need(length(discreteVars()) > 0, "No discrete variables were found."))
      freqPlot()
    })

    output$discreteTable <- DT::renderDT({
      DT::datatable(discreteTableRx(), options = list(scrollX = TRUE, pageLength = 10))
    })

    output$downloadTable <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "DISCRETE", "csv"),
      content = function(file) utils::write.csv(discreteTableRx(), file, row.names = FALSE)
    )

    output$downloadPlot <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "DISCRETE", input$imageFormat),
      content = function(file) savePlotFile(freqPlot(), file, input$imageFormat)
    )
  })
}
