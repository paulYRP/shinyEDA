# Purpose: UI for outlier detection.
# Arguments: Module id.
# Returns: Shiny UI.
outliersUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Outliers detection"),
    controlCard(
      shiny::numericInput(ns("t3"), "Three-Sigma Multiplier", value = 3, min = 0.5, step = 0.1),
      shiny::numericInput(ns("tH"), "Hampel MAD Multiplier", value = 3, min = 0.5, step = 0.1),
      shiny::numericInput(ns("tb"), "Boxplot IQR Multiplier", value = 1.5, min = 0.1, step = 0.1),
      dropdownInput(ns("imageFormat"), "Plot Format", choices = c("png", "jpg", "tiff")),
      shiny::downloadButton(ns("downloadPlot"), "Download plot", class = "control-action")
    ),
    expandablePlotCard(
      "Outlier counts",
      ns("outlierPlot"),
      ns("expandOutlier"),
      height = 460,
      DT::DTOutput(ns("summaryTable")),
      shiny::downloadButton(ns("downloadSummary"), "Download summary")
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
      t3 <- if (is.null(input$t3) || is.na(input$t3)) 3 else input$t3
      tH <- if (is.null(input$tH) || is.na(input$tH)) 3 else input$tH
      tb <- if (is.null(input$tb) || is.na(input$tb)) 1.5 else input$tb
      shiny::withProgress(message = "Detecting outliers", value = 0.5, {
        detectOutliers(data, t3, tH, tb)
      })
    })

    outlierPlot <- shiny::reactive({
      plotOutlierSummary(outliers()$summary, appPreviewVariableLimit())
    })

    outlierPlotFull <- shiny::reactive({
      plotOutlierSummary(outliers()$summary)
    })

    output$outlierPlot <- shiny::renderPlot({
      shiny::validate(shiny::need(nrow(outliers()$summary) > 0, "No numeric variables available for outlier detection."))
      outlierPlot()
    }, res = appPlotResolution())

    observeExpandedPlot(
      input,
      output,
      session,
      "expandOutlier",
      "outlierPlotFull",
      "Outlier counts",
      function() {
        shiny::validate(shiny::need(nrow(outliers()$summary) > 0, "No numeric variables available for outlier detection."))
        outlierPlotFull()
      },
      height = function() {
        nVars <- length(unique(outliers()$summary$variable))
        paste0(max(760, min(2200, nVars * 55)), "px")
      }
    )

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
      content = function(file) savePlotFile(outlierPlotFull(), file, input$imageFormat)
    )

    outliers
  })
}
