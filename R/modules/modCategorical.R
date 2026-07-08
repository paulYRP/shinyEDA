# Purpose: UI for categorical exploration.
# Arguments: Module id.
# Returns: Shiny UI.
categoricalUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Categorical variables"),
    controlCard(
      dropdownInput(ns("catVar"), "Category Variable", choices = NULL),
      dropdownInput(ns("groupVar"), "Group Variable", choices = NULL),
      dropdownInput(ns("numericVar"), "Numeric Variable", choices = NULL),
      dropdownInput(ns("responseSet"), "Response Set", choices = c("Custom" = "custom")),
      checkboxDropdownInput(ns("responseVars"), "Response Columns", choices = NULL, placeholder = "Select columns"),
      dropdownInput(ns("imageFormat"), "Plot Format", choices = c("png", "jpg", "tiff")),
      shiny::downloadButton(ns("downloadPlot"), "Download plot", class = "control-action")
    ),
    expandablePlotCard(
      "Categorical distribution",
      ns("catPlot"),
      ns("expandCat"),
      height = 360,
      DT::DTOutput(ns("catTable")),
      shiny::downloadButton(ns("downloadTable"), "Download categorical table")
    ),
    expandablePlotCard(
      "Numeric variable by group",
      ns("boxPlot"),
      ns("expandBox"),
      height = 380
    ),
    expandablePlotCard(
      "Response-item distributions",
      ns("questionPlot"),
      ns("expandResponse"),
      height = 420,
      shiny::downloadButton(ns("downloadQuestionPlot"), "Download response plot")
    )
  )
}

# Purpose: Server for categorical exploration.
# Arguments: Module id and clean data reactive.
# Returns: None.
categoricalServer <- function(id, cleanData, fileName = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    catVars <- shiny::reactive({
      shiny::req(cleanData())
      setdiff(getCategoricalVars(cleanData()), c("RiskStress", "RiskDiabetes2", "RiskCardiovascular"))
    })

    numericVars <- shiny::reactive({
      shiny::req(cleanData())
      getNumericVars(cleanData())
    })

    shiny::observeEvent(cleanData(), {
      shiny::updateSelectInput(session, "catVar", choices = catVars())
      shiny::updateSelectInput(session, "groupVar", choices = catVars(), selected = if ("gender" %in% catVars()) "gender" else NULL)
      shiny::updateSelectInput(session, "numericVar", choices = numericVars(), selected = if ("weight" %in% numericVars()) "weight" else NULL)
      choices <- responseSetChoices(cleanData())
      selectedSet <- defaultResponseSet(cleanData())
      responseVars <- responseSetVars(cleanData(), selectedSet)
      shiny::updateSelectInput(session, "responseSet", choices = choices, selected = selectedSet)
      shiny::updateCheckboxGroupInput(session, "responseVars", choices = names(cleanData()), selected = responseVars)
    })

    shiny::observeEvent(input$responseSet, {
      shiny::req(cleanData())
      if (identical(input$responseSet, "custom")) {
        shiny::updateCheckboxGroupInput(session, "responseVars", choices = names(cleanData()))
        return()
      }

      responseVars <- responseSetVars(cleanData(), input$responseSet)
      shiny::updateCheckboxGroupInput(session, "responseVars", choices = names(cleanData()), selected = responseVars)
    })

    catTable <- shiny::reactive({
      shiny::req(cleanData())
      categoricalTable(cleanData()[catVars()])
    })

    currentPlot <- shiny::reactive({
      shiny::req(cleanData(), input$catVar)
      plotCategoricalProportion(cleanData(), input$catVar)
    })

    output$catPlot <- shiny::renderPlot({
      shiny::validate(shiny::need(length(catVars()) > 0, "No categorical variables available."))
      currentPlot()
    }, res = appPlotResolution())

    observeExpandedPlot(
      input,
      output,
      session,
      "expandCat",
      "catPlotFull",
      "Categorical distribution",
      function() {
        shiny::validate(shiny::need(length(catVars()) > 0, "No categorical variables available."))
        currentPlot()
      }
    )

    output$boxPlot <- shiny::renderPlot({
      shiny::req(input$numericVar, input$groupVar)
      plotBoxByGroup(cleanData(), input$numericVar, input$groupVar)
    }, res = appPlotResolution())

    observeExpandedPlot(
      input,
      output,
      session,
      "expandBox",
      "boxPlotFull",
      "Numeric variable by group",
      function() {
        shiny::req(input$numericVar, input$groupVar)
        plotBoxByGroup(cleanData(), input$numericVar, input$groupVar)
      }
    )

    output$questionPlot <- shiny::renderPlot({
      plotQuestionResponses(cleanData(), input$responseVars, appPreviewFacetLimit())
    }, res = appPlotResolution())

    observeExpandedPlot(
      input,
      output,
      session,
      "expandResponse",
      "questionPlotFull",
      "Response-item distributions",
      function() {
        plotQuestionResponses(cleanData(), input$responseVars)
      },
      height = function() {
        nVars <- length(intersect(input$responseVars, names(cleanData())))
        paste0(max(760, min(1800, nVars * 45)), "px")
      }
    )

    output$catTable <- DT::renderDT({
      DT::datatable(catTable(), options = list(scrollX = TRUE, pageLength = 10))
    })

    output$downloadTable <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "CATEGORICAL", "csv"),
      content = function(file) utils::write.csv(catTable(), file, row.names = FALSE)
    )

    output$downloadPlot <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "CATEGORICAL", input$imageFormat),
      content = function(file) savePlotFile(currentPlot(), file, input$imageFormat)
    )

    output$downloadQuestionPlot <- shiny::downloadHandler(
      filename = function() buildDownloadName(fileName(), "RESPONSES", input$imageFormat),
      content = function(file) savePlotFile(plotQuestionResponses(cleanData(), input$responseVars), file, input$imageFormat)
    )
  })
}
