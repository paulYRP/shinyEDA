# Purpose: UI for dataset upload and cleaning controls.
# Arguments: Module id.
# Returns: Shiny UI.
dataUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Setup"),
    controlCard(
      shiny::fileInput(ns("dataFile"), "Upload Dataset", accept = c(".csv", ".txt", ".xlsx", ".xls")),
      dropdownInput(ns("exampleData"), "Example Dataset", choices = getExampleDatasets(), selected = "datasets::iris"),
      shiny::actionButton(ns("loadExample"), "Load example", class = "control-action"),
      shiny::uiOutput(ns("currentDataset")),
      dropdownInput(ns("sheetName"), "Excel Sheet", choices = c("First sheet" = "1"), selected = "1"),
      dropdownInput(ns("dictionarySheet"), "Dictionary Sheet", choices = c("None" = ""), selected = ""),
      checkboxDropdownInput(
        ns("abnormalValues"),
        "Values Converted to NA",
        choices = abnormalValueChoices(),
        selected = encodeAbnormalValues(defaultAbnormalValues()),
        placeholder = "Select NA tokens"
      ),
      shiny::textInput(ns("customAbnormalValues"), "Custom NA Values", value = "", placeholder = "missing, refused"),
      checkboxDropdownInput(ns("numericVars"), "Convert Numeric Vars", choices = NULL, placeholder = "Select variables"),
      checkboxDropdownInput(ns("integerVars"), "Convert Integer Vars", choices = NULL, placeholder = "Select variables"),
      checkboxDropdownInput(ns("factorVars"), "Convert Factor Vars", choices = NULL, placeholder = "Select variables"),
      dropdownInput(ns("birthdateVar"), "Birthdate Variable", choices = NULL),
      shiny::textInput(ns("dateOrigin"), "Date Origin", value = "1899-12-30"),
      shiny::textInput(ns("ageVar"), "Age Variable", value = "Age"),
      shiny::numericInput(ns("previewRows"), "Preview Rows", value = 50, min = 5, max = 500, step = 5)
    ),
    shiny::div(
      class = "privacy-note",
      "Privacy: uploaded data is kept only for the active Shiny session. Changing sections should not remove the active dataset; temporary files are deleted when the session ends or when another file is uploaded."
    ),
    bslib::card(
      bslib::card_header("Data preview"),
      shiny::div(
        class = "download-row",
        shiny::downloadButton(ns("downloadCsv"), "Download cleaned CSV"),
        shiny::downloadButton(ns("downloadXlsx"), "Download cleaned XLSX")
      ),
      DT::DTOutput(ns("preview"))
    )
  )
}

# Purpose: Server for dataset upload and cleaning.
# Arguments: Module id and optional active-section reactive.
# Returns: List of raw data, cleaned data, abnormal table and parameter log reactives.
dataServer <- function(id, activeSection = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    fileState <- shiny::reactiveVal(NULL)
    sheetChoicesState <- shiny::reactiveVal(c("First sheet" = "1"))
    dictionaryChoicesState <- shiny::reactiveVal(c("None" = ""))
    selectedSheetState <- shiny::reactiveVal("1")
    selectedDictionaryState <- shiny::reactiveVal("")
    sessionDir <- file.path(tempdir(), paste0("shinyEDA_", session$token))
    dir.create(sessionDir, recursive = TRUE, showWarnings = FALSE)

    session$onSessionEnded(function() {
      unlink(sessionDir, recursive = TRUE, force = TRUE)
    })

    shiny::observeEvent(input$loadExample, {
      unlink(list.files(sessionDir, full.names = TRUE), recursive = TRUE, force = TRUE)
      exampleKey <- input$exampleData
      exampleLabel <- names(getExampleDatasets())[match(exampleKey, getExampleDatasets())]
      if (is.na(exampleLabel)) {
        exampleLabel <- exampleKey
      }

      fileState(list(
        source = "example",
        exampleKey = exampleKey,
        name = paste0(gsub("[^A-Za-z0-9]+", "", exampleLabel), ".csv"),
        label = exampleLabel,
        size = NA_real_,
        ext = "example"
      ))

      sheetChoicesState(c("Not used for example data" = "1"))
      dictionaryChoicesState(c("None" = ""))
      selectedSheetState("1")
      selectedDictionaryState("")
      shiny::updateSelectInput(session, "sheetName", choices = sheetChoicesState(), selected = selectedSheetState())
      shiny::updateSelectInput(session, "dictionarySheet", choices = dictionaryChoicesState(), selected = selectedDictionaryState())
    })

    shiny::observeEvent(input$dataFile, {
      shiny::req(input$dataFile)
      ext <- tools::file_ext(input$dataFile$name)
      unlink(list.files(sessionDir, full.names = TRUE), recursive = TRUE, force = TRUE)
      savedPath <- file.path(sessionDir, paste0("uploaded.", tolower(ext)))
      file.copy(input$dataFile$datapath, savedPath, overwrite = TRUE)

      fileState(list(
        source = "upload",
        path = savedPath,
        name = input$dataFile$name,
        size = input$dataFile$size,
        ext = ext
      ))

      sheets <- getWorkbookSheets(savedPath, ext)
      selectedSheet <- chooseDefaultSheet(sheets)
      selectedDictionarySheet <- chooseDefaultDictionarySheet(sheets)

      if (tolower(ext) %in% c("xlsx", "xls") && length(sheets) > 0) {
        sheetChoicesState(stats::setNames(sheets, sheets))
        dictionaryChoices <- c("None" = "", stats::setNames(sheets, sheets))
        dictionaryChoicesState(dictionaryChoices)
        selectedSheetState(selectedSheet)
        selectedDictionaryState(selectedDictionarySheet)
        shiny::updateSelectInput(session, "sheetName", choices = sheetChoicesState(), selected = selectedSheetState())
        shiny::updateSelectInput(session, "dictionarySheet", choices = dictionaryChoicesState(), selected = selectedDictionaryState())
      } else {
        sheetChoicesState(c("Not used for CSV/TXT" = "1"))
        dictionaryChoicesState(c("None" = ""))
        selectedSheetState("1")
        selectedDictionaryState("")
        shiny::updateSelectInput(session, "sheetName", choices = sheetChoicesState(), selected = selectedSheetState())
        shiny::updateSelectInput(session, "dictionarySheet", choices = dictionaryChoicesState(), selected = selectedDictionaryState())
      }
    })

    shiny::observeEvent(input$sheetName, {
      choices <- unname(sheetChoicesState())
      if (hasScalarText(input$sheetName) && input$sheetName %in% choices) {
        selectedSheetState(input$sheetName)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$dictionarySheet, {
      choices <- unname(dictionaryChoicesState())
      if (!is.null(input$dictionarySheet) && input$dictionarySheet %in% choices) {
        selectedDictionaryState(input$dictionarySheet)
      }
    }, ignoreInit = TRUE)

    if (!is.null(activeSection)) {
      shiny::observeEvent(activeSection(), {
        if (!identical(activeSection(), "setup")) {
          return()
        }
        shiny::updateSelectInput(session, "sheetName", choices = sheetChoicesState(), selected = selectedSheetState())
        shiny::updateSelectInput(session, "dictionarySheet", choices = dictionaryChoicesState(), selected = selectedDictionaryState())
      }, ignoreInit = FALSE)
    }

    rawData <- shiny::reactive({
      activeFile <- fileState()
      shiny::req(activeFile)
      if (identical(activeFile$source, "example")) {
        return(loadExampleDataset(activeFile$exampleKey))
      }

      sheet <- input$sheetName
      if (is.null(sheet) || !sheet %in% unname(sheetChoicesState())) {
        sheet <- selectedSheetState()
      }
      if (is.null(sheet) || !nzchar(trimws(sheet))) {
        sheet <- 1
      } else if (grepl("^[0-9]+$", sheet)) {
        sheet <- as.integer(sheet)
      }
      readUploadedData(activeFile$path, activeFile$ext, sheet)
    })

    output$currentDataset <- shiny::renderUI({
      activeFile <- fileState()
      if (is.null(activeFile)) {
        return(shiny::div(class = "dataset-status dataset-empty", "No dataset uploaded."))
      }

      shiny::req(rawData())
      if (identical(activeFile$source, "example")) {
        return(shiny::div(
          class = "dataset-status",
          shiny::strong(activeFile$label),
          shiny::tags$span("Source: datasets package"),
          shiny::tags$span(paste0(nrow(rawData()), " rows x ", ncol(rawData()), " columns"))
        ))
      }

      shiny::div(
        class = "dataset-status",
        shiny::strong(activeFile$name),
        shiny::tags$span(paste0("Sheet: ", selectedSheetState())),
        shiny::tags$span(paste0(nrow(rawData()), " rows x ", ncol(rawData()), " columns"))
      )
    })

    shiny::observeEvent(rawData(), {
      vars <- names(rawData())
      likelyNumeric <- suggestNumericVars(rawData())
      likelyInteger <- suggestIntegerVars(rawData())
      likelyFactor <- suggestFactorVars(rawData())
      selectedAbnormal <- input$abnormalValues
      if (is.null(selectedAbnormal)) {
        selectedAbnormal <- encodeAbnormalValues(defaultAbnormalValues())
      }
      shiny::updateCheckboxGroupInput(session, "abnormalValues", choices = abnormalValueChoices(rawData()), selected = selectedAbnormal)
      shiny::updateCheckboxGroupInput(session, "numericVars", choices = vars, selected = likelyNumeric)
      shiny::updateCheckboxGroupInput(session, "integerVars", choices = vars, selected = likelyInteger)
      shiny::updateCheckboxGroupInput(session, "factorVars", choices = vars, selected = likelyFactor)
      birthdateDefault <- if ("birthdate" %in% vars) "birthdate" else ""
      shiny::updateSelectInput(session, "birthdateVar", choices = c("None" = "", vars), selected = birthdateDefault)
    })

    abnormalValues <- shiny::reactive({
      unique(c(
        decodeAbnormalValues(input$abnormalValues),
        parseAbnormalValues(input$customAbnormalValues)
      ))
    })

    abnormalTable <- shiny::reactive({
      shiny::req(rawData())
      countAbnormalValues(rawData(), abnormalValues())
    })

    dictionaryMetadata <- shiny::reactive({
      activeFile <- fileState()
      shiny::req(activeFile)
      if (identical(activeFile$source, "example")) {
        return(NULL)
      }

      dictionarySheet <- selectedDictionaryState()
      if (hasScalarText(input$dictionarySheet) && input$dictionarySheet %in% unname(dictionaryChoicesState())) {
        dictionarySheet <- input$dictionarySheet
      }
      readDictionaryMetadata(activeFile$path, activeFile$ext, dictionarySheet)
    })

    cleanData <- shiny::reactive({
      shiny::req(rawData())
      shiny::withProgress(message = "Preparing data", value = 0, {
        dat <- rawData()
        shiny::incProgress(0.25, detail = "Replacing abnormal values")
        dat <- replaceAbnormalValues(dat, abnormalValues())
        shiny::incProgress(0.25, detail = "Converting numeric variables")
        dat <- convertNumericVars(dat, input$numericVars)
        shiny::incProgress(0.15, detail = "Converting dates")
        dat <- convertDateVar(dat, input$birthdateVar, input$dateOrigin)
        shiny::incProgress(0.15, detail = "Creating age variable")
        dat <- addAgeFromBirthdate(dat, input$birthdateVar, input$ageVar)
        shiny::incProgress(0.10, detail = "Converting integer variables")
        dat <- convertIntegerVars(dat, input$integerVars)
        shiny::incProgress(0.10, detail = "Converting factor variables")
        dat <- convertFactorVars(dat, input$factorVars)
        dat
      })
    })

    params <- shiny::reactive({
      activeFile <- fileState()
      buildParameterLog(list(
        file = if (is.null(activeFile)) "" else activeFile$name,
        sheet = selectedSheetState(),
        dictionarySheet = selectedDictionaryState(),
        abnormalValues = abnormalValues(),
        customAbnormalValues = parseAbnormalValues(input$customAbnormalValues),
        numericVars = input$numericVars,
        integerVars = input$integerVars,
        factorVars = input$factorVars,
        birthdateVar = input$birthdateVar,
        dateOrigin = input$dateOrigin,
        ageVar = input$ageVar
      ))
    })

    output$preview <- DT::renderDT({
      shiny::req(cleanData())
      rows <- if (is.null(input$previewRows) || is.na(input$previewRows)) 50 else input$previewRows
      DT::datatable(head(cleanData(), rows), options = list(scrollX = TRUE, pageLength = 10))
    })

    output$downloadCsv <- shiny::downloadHandler(
      filename = function() buildDownloadName(activeFileName(), "CLEAN", "csv"),
      content = function(file) utils::write.csv(cleanData(), file, row.names = FALSE)
    )

    output$downloadXlsx <- shiny::downloadHandler(
      filename = function() buildDownloadName(activeFileName(), "CLEAN", "xlsx"),
      content = function(file) writeWorkbookTables(list(cleanedData = cleanData()), file)
    )

    activeFileName <- shiny::reactive({
      activeFile <- fileState()
      if (is.null(activeFile)) "" else activeFile$name
    })

    list(
      rawData = rawData,
      cleanData = cleanData,
      abnormalTable = abnormalTable,
      params = params,
      activeFileName = activeFileName,
      dictionaryMetadata = dictionaryMetadata
    )
  })
}
