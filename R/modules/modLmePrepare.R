# Purpose: UI for longitudinal model-data preparation.
# Arguments: Module id.
# Returns: Shiny UI.
lmePrepareUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("LME prepare data"),
    controlCard(
      dropdownInput(ns("timeVar"), "Time Variable", choices = NULL),
      checkboxDropdownInput(ns("timeLevels"), "Included Timepoints", choices = NULL, placeholder = "Select timepoints"),
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
      DT::DTOutput(ns("timeTable"))
    )
  )
}

# Purpose: Server for longitudinal model-data preparation.
# Arguments: Module id, clean data reactive and optional outlier reactive.
# Returns: List containing prepared data and selected controls.
lmePrepareServer <- function(id, cleanData, outliers = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    baseData <- shiny::reactive({
      shiny::req(cleanData())
      addBmiValue(addD21Scores(cleanData()))
    })

    shiny::observeEvent(baseData(), {
      vars <- names(baseData())
      timeDefault <- if ("timepoint" %in% vars) "timepoint" else ""
      userDefault <- if ("user_id" %in% vars) "user_id" else ""
      duplicateDefault <- intersect(c("user_id", "timepoint"), vars)
      dropDefault <- defaultModelDropColumns(baseData())
      shiny::updateSelectInput(session, "timeVar", choices = c("None" = "", vars), selected = timeDefault)
      shiny::updateSelectInput(session, "userVar", choices = c("None" = "", vars), selected = userDefault)
      shiny::updateCheckboxGroupInput(session, "duplicateCols", choices = vars, selected = duplicateDefault)
      shiny::updateTextAreaInput(session, "rangeRules", value = formatRangeRules(defaultRangeRules(baseData())))
      shiny::updateCheckboxGroupInput(session, "columnsToDrop", choices = vars, selected = dropDefault)
    })

    shiny::observeEvent(list(baseData(), input$timeVar), {
      dat <- baseData()
      selectedTimeVar <- scalarText(input$timeVar)
      if (!hasScalarText(selectedTimeVar) || !selectedTimeVar %in% names(dat)) {
        shiny::updateCheckboxGroupInput(session, "timeLevels", choices = character(), selected = character())
        return()
      }

      levels <- sort(unique(as.character(dat[[selectedTimeVar]])))
      levels <- levels[!is.na(levels) & nzchar(levels)]
      selected <- if (all(c("1", "2", "3") %in% levels)) c("1", "2", "3") else head(levels, 3)
      shiny::updateCheckboxGroupInput(session, "timeLevels", choices = levels, selected = selected)
    }, ignoreInit = FALSE)

    modelDataVersions <- shiny::reactive({
      shiny::req(cleanData())
      dat <- cleanData()
      baseDat <- baseData()
      timeVar <- if (!hasScalarText(input$timeVar)) {
        if ("timepoint" %in% names(baseDat)) "timepoint" else ""
      } else {
        scalarText(input$timeVar)
      }
      timeLevels <- if (is.null(input$timeLevels)) {
        if (hasScalarText(timeVar) && timeVar %in% names(baseDat)) {
          levels <- sort(unique(as.character(baseDat[[timeVar]])))
          levels <- levels[!is.na(levels) & nzchar(levels)]
          if (all(c("1", "2", "3") %in% levels)) c("1", "2", "3") else head(levels, 3)
        } else {
          character()
        }
      } else {
        input$timeLevels
      }
      userVar <- if (!hasScalarText(input$userVar)) {
        if ("user_id" %in% names(baseDat)) "user_id" else ""
      } else {
        scalarText(input$userVar)
      }
      duplicateCols <- if (is.null(input$duplicateCols)) {
        intersect(c("user_id", "timepoint"), names(baseDat))
      } else {
        input$duplicateCols
      }
      rareLevelMin <- if (is.null(input$rareLevelMin) || is.na(input$rareLevelMin)) 10 else input$rareLevelMin
      removeDuplicates <- if (is.null(input$removeDuplicates)) TRUE else input$removeDuplicates
      applyRangeFilters <- if (is.null(input$applyRangeFilters)) TRUE else input$applyRangeFilters
      rangeAction <- if (!hasScalarText(input$rangeAction)) "setNA" else scalarText(input$rangeAction)
      rangeRules <- if (is.null(input$rangeRules)) defaultRangeRules(baseDat) else parseRangeRules(input$rangeRules, dat)
      columnsToDrop <- if (is.null(input$columnsToDrop)) defaultModelDropColumns(baseDat) else input$columnsToDrop
      outlierValue <- if (is.null(outliers)) NULL else outliers()
      prepareLmeDataVersions(
        dat,
        timeVar = timeVar,
        timeLevels = timeLevels,
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
      selectedUserVar <- scalarText(input$userVar, if ("user_id" %in% names(modelData())) "user_id" else "")
      selectedTimeVar <- scalarText(input$timeVar, if ("timepoint" %in% names(modelData())) "timepoint" else "")
      tab <- data.frame(
        metric = c("Rows", "Columns", "Users", "Timepoints", "Outcomes detected"),
        value = c(
          nrow(modelData()),
          ncol(modelData()),
          if (hasScalarText(selectedUserVar) && selectedUserVar %in% names(modelData())) length(unique(modelData()[[selectedUserVar]])) else NA_integer_,
          if (hasScalarText(selectedTimeVar) && selectedTimeVar %in% names(modelData())) length(unique(modelData()[[selectedTimeVar]])) else NA_integer_,
          paste(defaultAnalysisOutcomes(modelData()), collapse = ", ")
        )
      )
      DT::datatable(tab, options = list(dom = "t"), rownames = FALSE)
    })

    output$timeTable <- DT::renderDT({
      shiny::req(modelData())
      selectedTimeVar <- scalarText(input$timeVar, if ("timepoint" %in% names(modelData())) "timepoint" else "")
      if (!hasScalarText(selectedTimeVar) || !selectedTimeVar %in% names(modelData())) {
        return(DT::datatable(data.frame()))
      }
      tab <- as.data.frame(table(modelData()[[selectedTimeVar]], useNA = "ifany"))
      names(tab) <- c("timepoint", "n")
      DT::datatable(tab, options = list(dom = "t"), rownames = FALSE)
    })

    list(
      modelData = modelData,
      modelDataVersions = modelDataVersions,
      timeVar = shiny::reactive({
        dat <- baseData()
        selectedTimeVar <- scalarText(input$timeVar)
        if (hasScalarText(selectedTimeVar) && selectedTimeVar %in% names(dat)) {
          return(selectedTimeVar)
        }
        if ("timepoint" %in% names(dat)) "timepoint" else ""
      }),
      userVar = shiny::reactive({
        dat <- baseData()
        selectedUserVar <- scalarText(input$userVar)
        if (hasScalarText(selectedUserVar) && selectedUserVar %in% names(dat)) {
          return(selectedUserVar)
        }
        if ("user_id" %in% names(dat)) "user_id" else ""
      })
    )
  })
}
