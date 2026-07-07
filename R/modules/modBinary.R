# Purpose: UI for binary variable characterisation.
# Arguments: Module id.
# Returns: Shiny UI.
binaryUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Binary"),
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        bslib::card_header("Controls"),
        shiny::selectInput(ns("binaryVar"), "Binary variable", choices = NULL),
        shiny::selectInput(ns("targetLevel"), "Target level", choices = NULL),
        shiny::selectInput(ns("groupVar"), "Group variable", choices = NULL),
        shiny::numericInput(ns("confLevel"), "Confidence level", value = 0.95, min = 0.5, max = 0.999, step = 0.01),
        shiny::selectInput(ns("imageFormat"), "Plot download format", choices = c("png", "jpg", "tiff")),
        shiny::downloadButton(ns("downloadPlot"), "Download plot")
      ),
      bslib::card(
        bslib::card_header("Binary summary"),
        shiny::plotOutput(ns("binaryPlot"), height = 420),
        DT::DTOutput(ns("binaryTable")),
        shiny::downloadButton(ns("downloadTable"), "Download binary table")
      )
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
    })

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
