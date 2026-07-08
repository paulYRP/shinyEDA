# Purpose: UI for cross-sectional model-data preparation.
# Arguments: Module id.
# Returns: Shiny UI.
glmPrepareUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("GLM prepare data"),
    controlCard(
      dropdownInput(ns("timeVar"), "Time Variable", choices = NULL),
      dropdownInput(ns("timeLevel"), "Selected Timepoint", choices = NULL),
      dropdownInput(ns("userVar"), "User ID Variable", choices = NULL),
      dropdownInput(ns("outlierMethod"), "Dataset Version", choices = analysisOutlierChoices(), selected = "boxplotRule"),
      shiny::numericInput(ns("rareLevelMin"), "Rare Level Minimum", value = 10, min = 1, step = 1),
      checkboxDropdownInput(ns("duplicateCols"), "Duplicate Key Columns", choices = NULL, placeholder = "Select keys"),
      shiny::div(class = "control-check-after", shiny::checkboxInput(ns("removeDuplicates"), "Remove Duplicates", value = TRUE)),
      dropdownInput(ns("rangeAction"), "Range Rule Action", choices = c("Set to NA" = "setNA", "Remove rows" = "removeRows"), selected = "setNA"),
      shiny::div(
        class = "control-wide",
        shiny::textAreaInput(ns("rangeRules"), "Value Range Rules", value = "", rows = 3, placeholder = "BMI, 12, 70")
      ),
      shiny::div(class = "control-check-after", shiny::checkboxInput(ns("applyRangeFilters"), "Apply Range Rules", value = TRUE)),
      shiny::div(
        class = "control-wide",
        checkboxDropdownInput(ns("columnsToDrop"), "Columns Removed Before Modelling", choices = NULL, placeholder = "Select columns")
      )
    ),
    bslib::card(
      bslib::card_header("Prepared data"),
      DT::DTOutput(ns("summaryTable")),
      shiny::hr(),
      DT::DTOutput(ns("variableTable"))
    )
  )
}

# Purpose: Server for cross-sectional model-data preparation.
# Arguments: Module id, clean data reactive and optional outlier reactive.
# Returns: List containing prepared data and selected controls.
glmPrepareServer <- function(id, cleanData, outliers = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    baseData <- shiny::reactive({
      shiny::req(cleanData())
      addBmiValue(addD21Scores(cleanData()))
    })

    shiny::observeEvent(baseData(), {
      vars <- names(baseData())
      timeDefault <- if ("timepoint" %in% vars) "timepoint" else ""
      userDefault <- if ("user_id" %in% vars) "user_id" else ""
      duplicateDefault <- if ("user_id" %in% vars) "user_id" else character()
      dropDefault <- defaultModelDropColumns(baseData())
      shiny::updateSelectInput(session, "timeVar", choices = c("None" = "", vars), selected = timeDefault)
      shiny::updateSelectInput(session, "userVar", choices = c("None" = "", vars), selected = userDefault)
      shiny::updateCheckboxGroupInput(session, "duplicateCols", choices = vars, selected = duplicateDefault)
      shiny::updateTextAreaInput(session, "rangeRules", value = formatRangeRules(defaultRangeRules(baseData())))
      shiny::updateCheckboxGroupInput(session, "columnsToDrop", choices = vars, selected = dropDefault)
    })

    shiny::observeEvent(list(baseData(), input$timeVar), {
      dat <- baseData()
      if (is.null(input$timeVar) || !nzchar(input$timeVar) || !input$timeVar %in% names(dat)) {
        shiny::updateSelectInput(session, "timeLevel", choices = c("All rows" = ""), selected = "")
        return()
      }

      levels <- sort(unique(as.character(dat[[input$timeVar]])))
      levels <- levels[!is.na(levels) & nzchar(levels)]
      selected <- if ("1" %in% levels) "1" else levels[1]
      shiny::updateSelectInput(session, "timeLevel", choices = stats::setNames(levels, levels), selected = selected)
    }, ignoreInit = FALSE)

    modelDataVersions <- shiny::reactive({
      shiny::req(cleanData())
      dat <- cleanData()
      baseDat <- baseData()
      timeVar <- if (is.null(input$timeVar)) {
        if ("timepoint" %in% names(baseDat)) "timepoint" else ""
      } else {
        input$timeVar
      }
      timeLevel <- if (is.null(input$timeLevel)) {
        if (nzchar(timeVar) && timeVar %in% names(baseDat)) {
          levels <- sort(unique(as.character(baseDat[[timeVar]])))
          levels <- levels[!is.na(levels) & nzchar(levels)]
          if (length(levels) == 0) {
            ""
          } else if ("1" %in% levels) {
            "1"
          } else {
            levels[1]
          }
        } else {
          ""
        }
      } else {
        input$timeLevel
      }
      userVar <- if (is.null(input$userVar)) {
        if ("user_id" %in% names(baseDat)) "user_id" else ""
      } else {
        input$userVar
      }
      duplicateCols <- if (is.null(input$duplicateCols)) {
        if ("user_id" %in% names(baseDat)) "user_id" else character()
      } else {
        input$duplicateCols
      }
      rareLevelMin <- if (is.null(input$rareLevelMin) || is.na(input$rareLevelMin)) 10 else input$rareLevelMin
      removeDuplicates <- if (is.null(input$removeDuplicates)) TRUE else input$removeDuplicates
      applyRangeFilters <- if (is.null(input$applyRangeFilters)) TRUE else input$applyRangeFilters
      rangeAction <- if (is.null(input$rangeAction) || !nzchar(input$rangeAction)) "setNA" else input$rangeAction
      rangeRules <- if (is.null(input$rangeRules)) defaultRangeRules(baseDat) else parseRangeRules(input$rangeRules, dat)
      columnsToDrop <- if (is.null(input$columnsToDrop)) defaultModelDropColumns(baseDat) else input$columnsToDrop
      outlierValue <- if (is.null(outliers)) NULL else outliers()
      prepareGlmDataVersions(
        dat,
        timeVar = timeVar,
        timeLevel = timeLevel,
        userVar = userVar,
        outliers = outlierValue,
        removeDuplicates = removeDuplicates,
        duplicateCols = duplicateCols,
        rareLevelMin = rareLevelMin,
        applyRangeFilters = applyRangeFilters,
        rangeRules = rangeRules,
        rangeAction = rangeAction,
        columnsToDrop = columnsToDrop
      )
    })

    modelData <- shiny::reactive({
      versions <- modelDataVersions()
      method <- input$outlierMethod
      if (is.null(method) || !method %in% names(versions)) {
        method <- "boxplotRule"
      }
      versions[[method]]
    })

    output$summaryTable <- DT::renderDT({
      shiny::req(modelData())
      tab <- data.frame(
        metric = c("Rows", "Columns", "Numeric variables", "Categorical variables", "Outcomes detected"),
        value = c(
          nrow(modelData()),
          ncol(modelData()),
          length(getNumericVars(modelData())),
          length(getCategoricalVars(modelData())),
          paste(defaultAnalysisOutcomes(modelData()), collapse = ", ")
        )
      )
      DT::datatable(tab, options = list(dom = "t"), rownames = FALSE)
    })

    output$variableTable <- DT::renderDT({
      shiny::req(modelData())
      DT::datatable(getVariableSummary(modelData()), options = list(scrollX = TRUE, pageLength = 10))
    })

    list(
      modelData = modelData,
      modelDataVersions = modelDataVersions,
      timeVar = shiny::reactive(input$timeVar),
      userVar = shiny::reactive(input$userVar)
    )
  })
}
