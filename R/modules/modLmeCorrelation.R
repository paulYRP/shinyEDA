# Purpose: UI for longitudinal correlations.
# Arguments: Module id.
# Returns: Shiny UI.
lmeCorrelationUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("LME correlation"),
    controlCard(
      dropdownInput(ns("method"), "Correlation Method", choices = c("spearman", "pearson", "kendall"), selected = "spearman"),
      dropdownInput(ns("timeLevel"), "Selected Timepoint", choices = NULL)
    ),
    expandablePlotCard(
      "Timepoint-specific heatmap",
      ns("corPlot"),
      ns("expandCor"),
      height = 560
    ),
    bslib::card(
      bslib::card_header("Timepoint-specific correlation table"),
      DT::DTOutput(ns("corTable"))
    )
  )
}

# Purpose: Server for longitudinal correlations.
# Arguments: Module id, prepared model data, user id and time variable reactives.
# Returns: None.
lmeCorrelationServer <- function(id, modelData, timeVar) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(modelData(), {
      dat <- modelData()
      tv <- scalarText(timeVar())
      if (!hasScalarText(tv) || !tv %in% names(dat)) {
        shiny::updateSelectInput(session, "timeLevel", choices = c("All rows" = ""), selected = "")
        return()
      }
      lev <- unique(as.character(dat[[tv]]))
      lev <- lev[!is.na(lev) & nzchar(lev)]
      if (length(lev) == 0) {
        shiny::updateSelectInput(session, "timeLevel", choices = c("All rows" = ""), selected = "")
        return()
      }
      shiny::updateSelectInput(session, "timeLevel", choices = stats::setNames(lev, lev), selected = lev[1])
    })

    timeData <- shiny::reactive({
      dat <- modelData()
      tv <- scalarText(timeVar())
      selectedLevel <- scalarText(input$timeLevel)
      if (hasScalarText(tv) && tv %in% names(dat) && hasScalarText(selectedLevel)) {
        dat <- dat[as.character(dat[[tv]]) == selectedLevel, , drop = FALSE]
      }
      dat
    })

    output$corPlot <- shiny::renderPlot({
      shiny::req(timeData())
      plotCorrelationMatrix(correlationMatrix(timeData(), input$method, appPreviewMatrixLimit()))
    }, res = appPlotResolution())

    observeExpandedPlot(
      input,
      output,
      session,
      "expandCor",
      "corPlotFull",
      "Timepoint-specific heatmap",
      function() {
        shiny::req(timeData())
        plotCorrelationMatrix(correlationMatrix(timeData(), input$method))
      },
      height = "900px"
    )

    output$corTable <- DT::renderDT({
      shiny::req(timeData())
      DT::datatable(correlationTable(timeData(), input$method), options = list(scrollX = TRUE, pageLength = 15))
    })
  })
}
