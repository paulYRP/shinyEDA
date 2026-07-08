# Purpose: UI for generalized linear models.
# Arguments: Module id.
# Returns: Shiny UI.
glmRegressionUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Generalized Linear Models"),
    controlCard(
      dropdownInput(ns("datasetVersion"), "Dataset Version", choices = analysisOutlierChoices(), selected = "boxplotRule"),
      dropdownInput(ns("outcome1"), "Model 1 Outcome", choices = NULL),
      dropdownInput(ns("family1"), "Model 1 Family", choices = glmFamilyChoices(), selected = "gaussian"),
      dropdownInput(ns("outcome2"), "Model 2 Outcome", choices = NULL),
      dropdownInput(ns("family2"), "Model 2 Family", choices = glmFamilyChoices(), selected = "gaussian"),
      dropdownInput(ns("outcome3"), "Model 3 Outcome", choices = NULL),
      dropdownInput(ns("family3"), "Model 3 Family", choices = glmFamilyChoices(), selected = "gaussian"),
      checkboxDropdownInput(ns("predictors"), "Model Predictors", choices = NULL, placeholder = "Select predictors"),
      dropdownInput(ns("interactionBase"), "Interaction Base", choices = c("None" = ""), selected = ""),
      checkboxDropdownInput(ns("interactionVars"), "Interaction Predictors", choices = NULL, placeholder = "Select interactions"),
      checkboxDropdownInput(ns("logVars"), "Log Variables", choices = NULL, placeholder = "Select variables"),
      dropdownInput(ns("logMethod"), "Log Method", choices = logMethodChoices(), selected = "log"),
      dropdownInput(ns("naAction"), "NA Handling", choices = naActionChoices(), selected = "na.omit"),
      shiny::actionButton(ns("runModel"), "Run model", class = "control-action")
    ),
    formulaCard(ns("formulaText")),
    bslib::card(
      bslib::card_header("Model summaries"),
      shiny::uiOutput(ns("summaryCards"))
    ),
    bslib::card(
      bslib::card_header("AIC model comparison"),
      DT::DTOutput(ns("selectionTable"))
    )
  )
}

# Purpose: Server for generalized linear models.
# Arguments: Module id and prepared model-data versions reactive.
# Returns: None.
glmRegressionServer <- function(id, modelDataVersions) {
  shiny::moduleServer(id, function(input, output, session) {
    selectedData <- shiny::reactive({
      versions <- modelDataVersions()
      shiny::req(length(versions) > 0)
      version <- scalarText(input$datasetVersion, "boxplotRule")
      if (!version %in% names(versions)) {
        version <- "boxplotRule"
      }
      versions[[version]]
    })

    defaultPredictors <- function(dat, outcomes) {
      baseDrop <- intersect(c("index", "user_id", "timepoint"), names(dat))
      riskVars <- intersect(defaultRiskVars(), names(dat))
      setdiff(names(dat), c(baseDrop, outcomes, riskVars))
    }

    updateFormulaControls <- function(dat, outcomes = NULL, predictors = NULL) {
      outcomes <- selectedModelOutcomes(outcomes, dat)
      predictors <- if (is.null(predictors)) defaultPredictors(dat, outcomes) else predictors
      predictors <- setdiff(intersect(predictors, names(dat)), outcomes)
      predictorChoices <- setdiff(names(dat), c(intersect(c("index", "user_id", "timepoint"), names(dat)), outcomes, intersect(defaultRiskVars(), names(dat))))
      predictors <- intersect(predictors, predictorChoices)
      interactionBase <- scalarText(input$interactionBase)
      if (!interactionBase %in% predictors) {
        interactionBase <- ""
      }
      numericPredictors <- intersect(predictors, getNumericVars(dat))

      shiny::updateCheckboxGroupInput(session, "predictors", choices = predictorChoices, selected = predictors)
      shiny::updateSelectInput(session, "interactionBase", choices = c("None" = "", stats::setNames(predictors, predictors)), selected = interactionBase)
      shiny::updateCheckboxGroupInput(session, "interactionVars", choices = predictors, selected = intersect(input$interactionVars, predictors))
      shiny::updateCheckboxGroupInput(session, "logVars", choices = numericPredictors, selected = intersect(input$logVars, numericPredictors))
    }

    shiny::observeEvent(modelDataVersions(), {
      shiny::updateSelectInput(session, "datasetVersion", choices = analysisOutlierChoices(), selected = "boxplotRule")
    })

    shiny::observeEvent(selectedData(), {
      dat <- selectedData()
      outcomeChoices <- modelOutcomeChoices(dat)
      outcomes <- selectedModelOutcomes(character(), dat)
      outcomes <- if (length(outcomes) < 3) c(outcomes, rep("", 3 - length(outcomes))) else outcomes
      shiny::updateSelectInput(session, "outcome1", choices = outcomeChoices, selected = outcomes[1])
      shiny::updateSelectInput(session, "outcome2", choices = outcomeChoices, selected = outcomes[2])
      shiny::updateSelectInput(session, "outcome3", choices = outcomeChoices, selected = outcomes[3])
      updateFormulaControls(dat, outcomes)
    })

    shiny::observeEvent(list(input$outcome1, input$outcome2, input$outcome3), {
      dat <- selectedData()
      outcomes <- selectedModelOutcomes(c(input$outcome1, input$outcome2, input$outcome3), dat)
      updateFormulaControls(dat, outcomes, input$predictors)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$predictors, {
      dat <- selectedData()
      outcomes <- selectedModelOutcomes(c(input$outcome1, input$outcome2, input$outcome3), dat)
      updateFormulaControls(dat, outcomes, input$predictors)
    }, ignoreInit = TRUE)

    currentSettings <- shiny::reactive({
      dat <- selectedData()
      outcomes <- selectedModelOutcomes(c(input$outcome1, input$outcome2, input$outcome3), dat)
      predictors <- input$predictors
      if (is.null(predictors) || length(predictors) == 0) {
        predictors <- defaultPredictors(dat, outcomes)
      }
      predictors <- setdiff(intersect(predictors, names(dat)), outcomes)
      predictorChoices <- setdiff(names(dat), c(intersect(c("index", "user_id", "timepoint"), names(dat)), outcomes, intersect(defaultRiskVars(), names(dat))))
      predictors <- intersect(predictors, predictorChoices)
      interactionBase <- scalarText(input$interactionBase)
      if (!interactionBase %in% predictors) {
        interactionBase <- ""
      }
      numericPredictors <- intersect(predictors, getNumericVars(dat))
      list(
        outcomes = outcomes,
        families = c(
          scalarText(input$family1, "gaussian"),
          scalarText(input$family2, "gaussian"),
          scalarText(input$family3, "gaussian")
        ),
        predictors = predictors,
        interactionBase = interactionBase,
        interactionVars = setdiff(intersect(input$interactionVars, predictors), interactionBase),
        logVars = intersect(input$logVars, numericPredictors),
        logMethod = scalarText(input$logMethod, "log"),
        naAction = scalarText(input$naAction, "na.omit"),
        datasetVersion = scalarText(input$datasetVersion, "boxplotRule")
      )
    })

    output$formulaText <- shiny::renderText({
      settings <- currentSettings()
      if (length(settings$predictors) == 0 || length(settings$outcomes) == 0) {
        return("Select outcomes and at least one predictor.")
      }
      rhs <- buildModelRhsTerms(
        settings$predictors,
        settings$logVars,
        settings$logMethod,
        settings$interactionBase,
        settings$interactionVars
      )
      forms <- vapply(seq_along(settings$outcomes), function(i) {
        paste0(
          "Model ", i, ": glm(\n  ",
          modelTerm(settings$outcomes[i]), " ~ ", paste(rhs, collapse = " + "),
          ",\n  data = ", settings$datasetVersion,
          ",\n  family = ", settings$families[i],
          ",\n  na.action = ", settings$naAction,
          "\n)"
        )
      }, character(1))
      paste(forms, collapse = "\n\n")
    })

    modelFits <- shiny::eventReactive(input$runModel, {
      if (is.null(input$runModel) || input$runModel < 1) {
        return(list())
      }
      settings <- currentSettings()
      dat <- selectedData()
      shiny::req(is.data.frame(dat), nrow(dat) > 0, length(settings$outcomes) > 0, length(settings$predictors) > 0)
      shiny::withProgress(message = "Fitting GLM models", value = 0, {
        shiny::incProgress(0.3, detail = "Validating outcomes")
        fits <- fitGlmOutcomeModels(
          dat,
          settings$outcomes,
          settings$families,
          settings$predictors,
          settings$naAction,
          settings$logVars,
          settings$logMethod,
          settings$interactionBase,
          settings$interactionVars
        )
        shiny::incProgress(0.7, detail = "Summarising models")
        fits
      })
    }, ignoreInit = FALSE)

    output$summaryCards <- shiny::renderUI({
      shiny::validate(shiny::need(input$runModel > 0, "Review the formulas and click Run model."))
      shiny::validate(shiny::need(length(modelFits()) > 0, "No GLM models are available."))
      shiny::div(
        class = "model-summary-grid",
        lapply(modelFits(), function(fit) {
          txt <- if (is.null(fit$model)) {
            paste("Model unavailable:", fit$error)
          } else {
            paste(suppressMessages(utils::capture.output(summary(fit$model))), collapse = "\n")
          }
          modelSummaryCard(paste0(fit$name, ": ", fit$outcome), txt)
        })
      )
    })

    output$selectionTable <- DT::renderDT({
      shiny::validate(shiny::need(input$runModel > 0, "Review the formulas and click Run model."))
      shiny::validate(shiny::need(length(modelFits()) > 0, "No model comparison is available."))
      DT::datatable(glmOutcomeModelSummary(modelFits()), options = list(scrollX = TRUE, pageLength = 5), rownames = FALSE)
    })
  })
}
