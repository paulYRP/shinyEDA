# Purpose: UI for dictionary and full analysis export.
# Arguments: Module id.
# Returns: Shiny UI.
dictionaryUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Dictionary"),
    shiny::p("Review variable metadata and download reproducible output tables."),
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header("Variable dictionary"),
        DT::DTOutput(ns("dictionaryTable")),
        shiny::hr(),
        shiny::downloadButton(ns("downloadDictionary"), "Download dictionary CSV")
      ),
      bslib::card(
        bslib::card_header("Selected parameters"),
        DT::DTOutput(ns("parameterTable")),
        shiny::hr(),
        shiny::downloadButton(ns("downloadLog"), "Download parameter log")
      )
    ),
    bslib::card(
      bslib::card_header("Complete workbook"),
      shiny::p("Workbook includes summary, binary, categorical, discrete, numeric, outlier and column-definition sheets when available."),
      shiny::downloadButton(ns("downloadWorkbook"), "Download workbook")
    )
  )
}

# Purpose: Server for dictionary and full analysis export.
# Arguments: Module id, clean data reactive, parameter-log reactive, optional outlier, file-name and dictionary metadata reactives.
# Returns: No return value.
dictionaryServer <- function(id, cleanData, params, outliers = NULL, fileName = NULL, dictionaryMetadata = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    dictionaryTable <- shiny::reactive({
      shiny::req(cleanData())
      metadata <- if (is.null(dictionaryMetadata)) NULL else dictionaryMetadata()
      variableDictionarySummary(cleanData(), metadata)
    })

    exportTables <- shiny::reactive({
      shiny::req(cleanData())
      outlierValue <- NULL
      if (!is.null(outliers)) {
        outlierValue <- outliers()
      }
      metadata <- if (is.null(dictionaryMetadata)) NULL else dictionaryMetadata()
      buildEdaExportTables(cleanData(), outlierValue, metadata)
    })

    output$dictionaryTable <- DT::renderDT({
      DT::datatable(dictionaryTable(), options = list(scrollX = TRUE, pageLength = 15))
    })

    output$parameterTable <- DT::renderDT({
      shiny::req(params())
      DT::datatable(params(), options = list(dom = "t", scrollX = TRUE))
    })

    output$downloadDictionary <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "DICT", "csv"),
      content = function(file) utils::write.csv(dictionaryTable(), file, row.names = FALSE)
    )

    output$downloadLog <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "LOG", "csv"),
      content = function(file) utils::write.csv(params(), file, row.names = FALSE)
    )

    output$downloadWorkbook <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "DICT", "xlsx"),
      content = function(file) writeWorkbookTables(exportTables(), file)
    )
  })
}
