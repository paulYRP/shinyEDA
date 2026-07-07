# Purpose: UI for dataset upload and cleaning controls.
# Arguments: Module id.
# Returns: Shiny UI.
dataUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Setup"),
    shiny::p("Upload data and apply the first cleaning steps."),
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card(
        bslib::card_header("Inputs"),
        shiny::fileInput(ns("dataFile"), "Upload dataset", accept = c(".csv", ".txt", ".xlsx", ".xls")),
        shiny::div(
          class = "example-row",
          shiny::selectInput(ns("exampleData"), "Example dataset", choices = getExampleDatasets(), selected = "datasets::iris", selectize = FALSE),
          shiny::actionButton(ns("loadExample"), "Load example")
        ),
        shiny::p(class = "input-note", "Upload another file to replace the active session dataset."),
        shiny::uiOutput(ns("currentDataset")),
        shiny::selectInput(ns("sheetName"), "Excel sheet", choices = c("First sheet" = "1"), selected = "1", selectize = FALSE),
        shiny::selectInput(ns("dictionarySheet"), "Dictionary sheet", choices = c("None" = ""), selected = "", selectize = FALSE),
        shiny::textAreaInput(
          ns("abnormalValues"),
          "Abnormal values converted to NA",
          value = paste(defaultAbnormalValues(), collapse = ", "),
          rows = 4
        ),
        shiny::selectizeInput(ns("numericVars"), "Convert variables to numeric", choices = NULL, multiple = TRUE),
        shiny::selectizeInput(ns("integerVars"), "Convert variables to integer", choices = NULL, multiple = TRUE),
        shiny::selectizeInput(ns("factorVars"), "Convert variables to factor", choices = NULL, multiple = TRUE),
        shiny::selectInput(ns("birthdateVar"), "Birthdate variable", choices = NULL),
        shiny::textInput(ns("dateOrigin"), "Excel date origin", value = "1899-12-30"),
        shiny::textInput(ns("ageVar"), "Age variable name", value = "Age")
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
  )
}

# Purpose: Server for dataset upload and cleaning.
# Arguments: Module id.
# Returns: List of raw data, cleaned data, abnormal table and parameter log reactives.
dataServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    fileState <- shiny::reactiveVal(NULL)
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

      shiny::updateSelectInput(session, "sheetName", choices = c("Not used for example data" = "1"), selected = "1")
      shiny::updateSelectInput(session, "dictionarySheet", choices = c("None" = ""), selected = "")
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
        shiny::updateSelectInput(session, "sheetName", choices = stats::setNames(sheets, sheets), selected = selectedSheet)
        dictionaryChoices <- c("None" = "", stats::setNames(sheets, sheets))
        shiny::updateSelectInput(session, "dictionarySheet", choices = dictionaryChoices, selected = selectedDictionarySheet)
      } else {
        shiny::updateSelectInput(session, "sheetName", choices = c("Not used for CSV/TXT" = "1"), selected = "1")
        shiny::updateSelectInput(session, "dictionarySheet", choices = c("None" = ""), selected = "")
      }
    })

    rawData <- shiny::reactive({
      activeFile <- fileState()
      shiny::req(activeFile)
      if (identical(activeFile$source, "example")) {
        return(loadExampleDataset(activeFile$exampleKey))
      }

      sheet <- input$sheetName
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
        shiny::tags$span(paste0("Sheet: ", input$sheetName)),
        shiny::tags$span(paste0(nrow(rawData()), " rows x ", ncol(rawData()), " columns"))
      )
    })

    shiny::observeEvent(rawData(), {
      vars <- names(rawData())
      likelyNumeric <- suggestNumericVars(rawData())
      likelyInteger <- suggestIntegerVars(rawData())
      likelyFactor <- suggestFactorVars(rawData())
      shiny::updateSelectizeInput(session, "numericVars", choices = vars, selected = likelyNumeric, server = TRUE)
      shiny::updateSelectizeInput(session, "integerVars", choices = vars, selected = likelyInteger, server = TRUE)
      shiny::updateSelectizeInput(session, "factorVars", choices = vars, selected = likelyFactor, server = TRUE)
      birthdateDefault <- if ("birthdate" %in% vars) "birthdate" else ""
      shiny::updateSelectInput(session, "birthdateVar", choices = c("None" = "", vars), selected = birthdateDefault)
    })

    abnormalValues <- shiny::reactive(parseAbnormalValues(input$abnormalValues))

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

      readDictionaryMetadata(activeFile$path, activeFile$ext, input$dictionarySheet)
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
        sheet = input$sheetName,
        dictionarySheet = input$dictionarySheet,
        abnormalValues = abnormalValues(),
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
      DT::datatable(head(cleanData(), 50), options = list(scrollX = TRUE, pageLength = 10))
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
