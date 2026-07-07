# Purpose: UI for conditioning and tree diagnostics.
# Arguments: Module id.
# Returns: Shiny UI.
conditioningUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Conditioning Variable"),
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        bslib::card_header("Controls"),
        shiny::selectInput(ns("xVar"), "X variable", choices = NULL),
        shiny::selectInput(ns("yVar"), "Y variable", choices = NULL),
        shiny::selectInput(ns("groupVar"), "Conditioning/group variable", choices = NULL),
        shiny::checkboxInput(ns("logScale"), "Use log scales", value = FALSE),
        shiny::selectInput(ns("treeOutcome"), "Tree outcome", choices = NULL),
        shiny::selectInput(ns("treePredictors"), "Tree predictors", choices = NULL, multiple = TRUE),
        shiny::selectInput(ns("imageFormat"), "Plot download format", choices = c("png", "jpg", "tiff")),
        shiny::downloadButton(ns("downloadScatter"), "Download scatter plot")
      ),
      bslib::card(
        bslib::card_header("Conditional relationship"),
        shiny::plotOutput(ns("scatterPlot"), height = 420)
      )
    ),
    bslib::card(
      bslib::card_header("Regression tree"),
      shiny::plotOutput(ns("treePlot"), height = 520),
      shiny::downloadButton(ns("downloadTree"), "Download tree plot")
    )
  )
}

# Purpose: Server for conditioning and regression-tree diagnostics.
# Arguments: Module id and clean data reactive.
# Returns: None.
conditioningServer <- function(id, cleanData, fileName = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    numericVars <- shiny::reactive({
      shiny::req(cleanData())
      getNumericVars(cleanData())
    })

    catVars <- shiny::reactive({
      shiny::req(cleanData())
      getCategoricalVars(cleanData())
    })

    shiny::observeEvent(cleanData(), {
      nums <- numericVars()
      cats <- catVars()
      shiny::updateSelectInput(session, "xVar", choices = nums, selected = if ("RiskStressVal" %in% nums) "RiskStressVal" else nums[1])
      shiny::updateSelectInput(session, "yVar", choices = nums, selected = if ("LBM" %in% nums) "LBM" else nums[min(2, length(nums))])
      shiny::updateSelectInput(session, "groupVar", choices = c("None" = "", cats), selected = if ("gender" %in% cats) "gender" else "")
      shiny::updateSelectInput(session, "treeOutcome", choices = nums, selected = if ("RiskStressVal" %in% nums) "RiskStressVal" else nums[1])
      shiny::updateSelectInput(session, "treePredictors", choices = names(cleanData()), selected = intersect(c("gender", "height", "weight", "Waist", "LBM"), names(cleanData())))
    })

    scatterPlot <- shiny::reactive({
      shiny::req(cleanData(), input$xVar, input$yVar)
      groupVar <- if (nzchar(input$groupVar)) input$groupVar else NULL
      plotScatter(cleanData(), input$xVar, input$yVar, groupVar, input$logScale)
    })

    output$scatterPlot <- shiny::renderPlot({
      shiny::validate(shiny::need(length(numericVars()) >= 2, "At least two numeric variables are required."))
      scatterPlot()
    })

    output$treePlot <- shiny::renderPlot({
      shiny::req(cleanData(), input$treeOutcome, input$treePredictors)
      shiny::validate(shiny::need(length(input$treePredictors) > 0, "Select at least one tree predictor."))
      form <- stats::reformulate(input$treePredictors, response = input$treeOutcome)
      mod <- rpart::rpart(form, data = cleanData())
      plot(mod, uniform = TRUE, margin = 0.1)
      text(mod, use.n = TRUE, cex = 0.75)
    })

    output$downloadScatter <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "CONDITIONING", input$imageFormat),
      content = function(file) savePlotFile(scatterPlot(), file, input$imageFormat)
    )

    output$downloadTree <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "TREE", input$imageFormat),
      content = function(file) {
        grDevices::png(file, width = 1800, height = 1200, res = 180)
        on.exit(grDevices::dev.off(), add = TRUE)
        form <- stats::reformulate(input$treePredictors, response = input$treeOutcome)
        mod <- rpart::rpart(form, data = cleanData())
        plot(mod, uniform = TRUE, margin = 0.1)
        text(mod, use.n = TRUE, cex = 0.75)
      }
    )
  })
}
