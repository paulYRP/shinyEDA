# Purpose: UI for inlier-detection discussion.
# Arguments: Module id.
# Returns: Shiny UI.
inliersUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Inliers detection"),
    bslib::card(
      bslib::card_header("Concept"),
      shiny::p("There is no single mathematical detection procedure for inliers."),
      shiny::p("As a practical diagnostic, this page counts repeated values and flags unusually frequent values in a selected variable.")
    ),
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        bslib::card_header("Controls"),
        shiny::selectInput(ns("variable"), "Variable", choices = NULL),
        shiny::numericInput(ns("topN"), "Show top repeated values", value = 20, min = 5, max = 100, step = 5)
      ),
      bslib::card(
        bslib::card_header("Repeated values"),
        DT::DTOutput(ns("repeatTable")),
        shiny::downloadButton(ns("downloadTable"), "Download repeated-value table")
      )
    )
  )
}

# Purpose: Server for repeated-value inlier diagnostics.
# Arguments: Module id and clean data reactive.
# Returns: None.
inliersServer <- function(id, cleanData, fileName = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(cleanData(), {
      shiny::updateSelectInput(session, "variable", choices = names(cleanData()))
    })

    repeatTable <- shiny::reactive({
      shiny::req(cleanData(), input$variable)
      tab <- sort(table(cleanData()[[input$variable]], useNA = "ifany"), decreasing = TRUE)
      out <- data.frame(
        value = names(tab),
        count = as.integer(tab),
        proportion = as.numeric(tab) / nrow(cleanData()),
        stringsAsFactors = FALSE
      )
      head(out, input$topN)
    })

    output$repeatTable <- DT::renderDT({
      DT::datatable(repeatTable(), options = list(scrollX = TRUE, pageLength = 10))
    })

    output$downloadTable <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "INLIERS", "csv"),
      content = function(file) utils::write.csv(repeatTable(), file, row.names = FALSE)
    )
  })
}
