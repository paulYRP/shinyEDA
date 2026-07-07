# Purpose: UI for exploration overview.
# Arguments: Module id.
# Returns: Shiny UI.
overviewUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Overview"),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("Variable structure"), DT::DTOutput(ns("varTable"))),
      bslib::card(bslib::card_header("Abnormal values before cleaning"), DT::DTOutput(ns("abnormalTable")))
    ),
    bslib::card(
      bslib::card_header("Missing values"),
      shiny::plotOutput(ns("missingPlot"), height = 420),
      DT::DTOutput(ns("missingTable")),
      shiny::downloadButton(ns("downloadMissing"), "Download missing summary")
    )
  )
}

# Purpose: Server for exploration overview.
# Arguments: Module id, clean data reactive, abnormal table reactive.
# Returns: None.
overviewServer <- function(id, cleanData, abnormalTable, fileName = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    varSummary <- shiny::reactive({
      shiny::req(cleanData())
      getVariableSummary(cleanData())
    })

    missingTable <- shiny::reactive({
      shiny::req(cleanData())
      missingSummary(cleanData())
    })

    output$varTable <- DT::renderDT({
      DT::datatable(varSummary(), options = list(scrollX = TRUE, pageLength = 12))
    })

    output$abnormalTable <- DT::renderDT({
      DT::datatable(abnormalTable(), options = list(scrollX = TRUE, pageLength = 8))
    })

    output$missingPlot <- shiny::renderPlot({
      shiny::validate(shiny::need(nrow(missingTable()) > 0, "Upload data to inspect missingness."))
      plotMissingSummary(missingTable())
    })

    output$missingTable <- DT::renderDT({
      DT::datatable(missingTable(), options = list(scrollX = TRUE, pageLength = 12))
    })

    output$downloadMissing <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "MISSING", "csv"),
      content = function(file) utils::write.csv(missingTable(), file, row.names = FALSE)
    )
  })
}
