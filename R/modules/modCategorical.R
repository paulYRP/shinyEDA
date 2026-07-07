# Purpose: UI for categorical exploration.
# Arguments: Module id.
# Returns: Shiny UI.
categoricalUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Categorical variables"),
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        bslib::card_header("Controls"),
        shiny::selectInput(ns("catVar"), "Categorical variable", choices = NULL),
        shiny::selectInput(ns("groupVar"), "Group for numeric boxplot", choices = NULL),
        shiny::selectInput(ns("numericVar"), "Numeric variable for boxplot", choices = NULL),
        shiny::selectizeInput(ns("questionVars"), "Question response variables", choices = NULL, multiple = TRUE),
        shiny::selectInput(ns("imageFormat"), "Plot download format", choices = c("png", "jpg", "tiff")),
        shiny::downloadButton(ns("downloadPlot"), "Download plot")
      ),
      bslib::card(
        bslib::card_header("Categorical distribution"),
        shiny::plotOutput(ns("catPlot"), height = 360),
        DT::DTOutput(ns("catTable")),
        shiny::downloadButton(ns("downloadTable"), "Download categorical table")
      )
    ),
    bslib::card(
      bslib::card_header("Numeric variable by group"),
      shiny::plotOutput(ns("boxPlot"), height = 380)
    ),
    bslib::card(
      bslib::card_header("Question responses"),
      shiny::plotOutput(ns("questionPlot"), height = 420),
      shiny::downloadButton(ns("downloadQuestionPlot"), "Download question plot")
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
      questionVars <- intersect(detectQuestionVars(cleanData()), names(cleanData()))
      shiny::updateSelectizeInput(session, "questionVars", choices = names(cleanData()), selected = questionVars, server = TRUE)
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
    })

    output$boxPlot <- shiny::renderPlot({
      shiny::req(input$numericVar, input$groupVar)
      plotBoxByGroup(cleanData(), input$numericVar, input$groupVar)
    })

    output$questionPlot <- shiny::renderPlot({
      plotQuestionResponses(cleanData(), input$questionVars)
    })

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
      filename = function() buildDownloadName(fileName(), "QUESTIONS", input$imageFormat),
      content = function(file) savePlotFile(plotQuestionResponses(cleanData(), input$questionVars), file, input$imageFormat)
    )
  })
}
