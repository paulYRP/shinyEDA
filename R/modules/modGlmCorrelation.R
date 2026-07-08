# Purpose: UI for GLM correlation analysis.
# Arguments: Module id.
# Returns: Shiny UI.
glmCorrelationUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("GLM correlation"),
    controlCard(
      dropdownInput(ns("method"), "Correlation Method", choices = c("spearman", "pearson", "kendall"), selected = "spearman")
    ),
    expandablePlotCard(
      "Correlation heatmap",
      ns("corPlot"),
      ns("expandCor"),
      height = 560
    ),
    bslib::card(
      bslib::card_header("Correlation table"),
      DT::DTOutput(ns("corTable"))
    )
  )
}

# Purpose: Server for GLM correlation analysis.
# Arguments: Module id and prepared model data reactive.
# Returns: None.
glmCorrelationServer <- function(id, modelData) {
  shiny::moduleServer(id, function(input, output, session) {
    output$corPlot <- shiny::renderPlot({
      shiny::req(modelData())
      plotCorrelationMatrix(correlationMatrix(modelData(), input$method, appPreviewMatrixLimit()))
    }, res = appPlotResolution())

    observeExpandedPlot(
      input,
      output,
      session,
      "expandCor",
      "corPlotFull",
      "Correlation heatmap",
      function() {
        shiny::req(modelData())
        plotCorrelationMatrix(correlationMatrix(modelData(), input$method))
      },
      height = "900px"
    )

    output$corTable <- DT::renderDT({
      shiny::req(modelData())
      DT::datatable(correlationTable(modelData(), input$method), options = list(scrollX = TRUE, pageLength = 15))
    })
  })
}
