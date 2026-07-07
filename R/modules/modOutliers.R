# Purpose: UI for outlier detection.
# Arguments: Module id.
# Returns: Shiny UI.
outliersUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Outliers detection"),
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        bslib::card_header("Controls"),
        shiny::numericInput(ns("t3"), "Three-sigma multiplier", value = 3, min = 0.5, step = 0.1),
        shiny::numericInput(ns("tH"), "Hampel MAD multiplier", value = 3, min = 0.5, step = 0.1),
        shiny::numericInput(ns("tb"), "Boxplot IQR multiplier", value = 1.5, min = 0.1, step = 0.1),
        shiny::selectInput(ns("imageFormat"), "Plot download format", choices = c("png", "jpg", "tiff")),
        shiny::downloadButton(ns("downloadPlot"), "Download plot")
      ),
      bslib::card(
        bslib::card_header("Outlier counts"),
        shiny::plotOutput(ns("outlierPlot"), height = 460),
        DT::DTOutput(ns("summaryTable")),
        shiny::downloadButton(ns("downloadSummary"), "Download summary")
      )
    ),
    bslib::card(
      bslib::card_header("Detailed outlier rows"),
      DT::DTOutput(ns("detailTable")),
      shiny::downloadButton(ns("downloadDetail"), "Download details")
    )
  )
}

# Purpose: Server for outlier detection.
# Arguments: Module id and clean data reactive.
# Returns: Reactive list with outlier summary/detail tables.
outliersServer <- function(id, cleanData, fileName = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    outliers <- shiny::reactive({
      shiny::req(cleanData())
      excludeVars <- intersect(c("index", "user_id", "timepoint", "answered_at", "birthdate"), names(cleanData()))
      data <- cleanData()[setdiff(names(cleanData()), excludeVars)]
      shiny::withProgress(message = "Detecting outliers", value = 0.5, {
        detectOutliers(data, input$t3, input$tH, input$tb)
      })
    }) |>
      shiny::bindCache(cleanData(), input$t3, input$tH, input$tb)

    outlierPlot <- shiny::reactive({
      plotOutlierSummary(outliers()$summary)
    })

    output$outlierPlot <- shiny::renderPlot({
      shiny::validate(shiny::need(nrow(outliers()$summary) > 0, "No numeric variables available for outlier detection."))
      outlierPlot()
    })

    output$summaryTable <- DT::renderDT({
      DT::datatable(outliers()$summary, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$detailTable <- DT::renderDT({
      DT::datatable(outliers()$detail, options = list(scrollX = TRUE, pageLength = 10))
    })

    output$downloadSummary <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "OUTLIERSUMMARY", "csv"),
      content = function(file) utils::write.csv(outliers()$summary, file, row.names = FALSE)
    )

    output$downloadDetail <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "OUTLIER", "csv"),
      content = function(file) utils::write.csv(outliers()$detail, file, row.names = FALSE)
    )

    output$downloadPlot <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "OUTLIER", input$imageFormat),
      content = function(file) savePlotFile(outlierPlot(), file, input$imageFormat)
    )

    outliers
  })
}
