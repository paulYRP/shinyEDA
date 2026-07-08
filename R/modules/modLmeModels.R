# Purpose: UI for generalized linear mixed-effects models.
# Arguments: Module id.
# Returns: Shiny UI.
lmeModelsUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Generalized Linear Mixed Effects Models"),
    controlCard(
      dropdownInput(ns("datasetVersion"), "Dataset Version", choices = analysisOutlierChoices(), selected = "boxplotRule"),
      dropdownInput(ns("outcome1"), "Model 1 Outcome", choices = NULL),
      dropdownInput(ns("family1"), "Model 1 Family", choices = glmFamilyChoices(), selected = "gaussian"),
      dropdownInput(ns("outcome2"), "Model 2 Outcome", choices = NULL),
      dropdownInput(ns("family2"), "Model 2 Family", choices = glmFamilyChoices(), selected = "gaussian"),
      dropdownInput(ns("outcome3"), "Model 3 Outcome", choices = NULL),
      dropdownInput(ns("family3"), "Model 3 Family", choices = glmFamilyChoices(), selected = "gaussian"),
      dropdownInput(ns("timeVar"), "Time Variable", choices = NULL),
      checkboxDropdownInput(ns("fixedPredictors"), "Fixed Effect Predictors", choices = NULL, placeholder = "Select predictors"),
      dropdownInput(ns("timeInteraction"), "Time Interaction", choices = c("None" = ""), selected = ""),
      dropdownInput(ns("groupVar"), "Random Group Variable", choices = NULL),
      checkboxDropdownInput(ns("randomSlopes"), "Random Slope Variables", choices = NULL, placeholder = "Select slopes"),
      checkboxDropdownInput(ns("logVars"), "Log Variables", choices = NULL, placeholder = "Select variables"),
      dropdownInput(ns("logMethod"), "Log Method", choices = logMethodChoices(), selected = "log"),
      dropdownInput(ns("naAction"), "NA Handling", choices = naActionChoices(), selected = "na.omit"),
      shiny::actionButton(ns("runModel"), "Run model", class = "control-action")
    ),
    formulaCard(ns("formulaText")),
    expandablePlotCard(
      "Outcome trajectories",
      ns("trajectoryPlot"),
      ns("expandTrajectory"),
      height = 520
    ),
    bslib::card(
      bslib::card_header("Model summaries"),
      shiny::uiOutput(ns("summaryCards"))
    ),
    bslib::card(
      bslib::card_header("AIC and R2 model comparison"),
      DT::DTOutput(ns("modelTable"))
    )
  )
}

# Purpose: Server for generalized linear mixed-effects models.
# Arguments: Module id, prepared data versions and user/time variable reactives.
# Returns: None.
lmeModelsServer <- function(id, modelDataVersions, userVar, timeVar) {
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

    defaultGroupVar <- function(dat) {
      vars <- names(dat)
      currentUserVar <- scalarText(userVar(), if ("user_id" %in% vars) "user_id" else "")
      if (hasScalarText(currentUserVar) && currentUserVar %in% vars) {
        return(currentUserVar)
      }
      if (length(vars) > 0) vars[1] else ""
    }

    defaultTimeVar <- function(dat) {
      vars <- names(dat)
      currentTimeVar <- scalarText(timeVar(), if ("timepoint" %in% vars) "timepoint" else "")
      if (hasScalarText(currentTimeVar) && currentTimeVar %in% vars) currentTimeVar else ""
    }

    defaultPredictors <- function(dat, outcomes, groupVar, timeVar) {
      riskVars <- intersect(defaultRiskVars(), names(dat))
      baseDrop <- intersect(c("index", "user_id", "timepoint", groupVar, timeVar), names(dat))
      candidates <- unique(c(defaultFocalPredictors(dat), defaultAdjustmentVars(dat)))
      candidates <- if (length(candidates) > 0) candidates else setdiff(names(dat), c(baseDrop, outcomes, riskVars))
      setdiff(intersect(candidates, names(dat)), c(baseDrop, outcomes, riskVars))
    }

    updateFormulaControls <- function(dat, outcomes = NULL, predictors = NULL) {
      vars <- names(dat)
      outcomes <- selectedModelOutcomes(outcomes, dat)
      groupDefault <- defaultGroupVar(dat)
      timeDefault <- defaultTimeVar(dat)
      predictorChoices <- setdiff(vars, c(intersect(c("index", "user_id", "timepoint", groupDefault, timeDefault), vars), outcomes, intersect(defaultRiskVars(), vars)))
      predictors <- if (is.null(predictors)) defaultPredictors(dat, outcomes, groupDefault, timeDefault) else predictors
      predictors <- intersect(setdiff(predictors, outcomes), predictorChoices)
      timeInteraction <- scalarText(input$timeInteraction)
      if (!timeInteraction %in% predictors) {
        defaultFocal <- intersect(defaultFocalPredictors(dat), predictors)
        timeInteraction <- if (length(defaultFocal) > 0) defaultFocal[1] else if (length(predictors) > 0) predictors[1] else ""
      }
      numericPredictors <- intersect(predictors, getNumericVars(dat))

      shiny::updateCheckboxGroupInput(session, "fixedPredictors", choices = predictorChoices, selected = predictors)
      shiny::updateSelectInput(session, "timeInteraction", choices = c("None" = "", stats::setNames(predictors, predictors)), selected = timeInteraction)
      shiny::updateCheckboxGroupInput(session, "randomSlopes", choices = setdiff(vars, c(outcomes, groupDefault)), selected = intersect(input$randomSlopes, setdiff(vars, c(outcomes, groupDefault))))
      shiny::updateCheckboxGroupInput(session, "logVars", choices = numericPredictors, selected = intersect(input$logVars, numericPredictors))
    }

    shiny::observeEvent(modelDataVersions(), {
      shiny::updateSelectInput(session, "datasetVersion", choices = analysisOutlierChoices(), selected = "boxplotRule")
    })

    shiny::observeEvent(selectedData(), {
      dat <- selectedData()
      vars <- names(dat)
      outcomeChoices <- modelOutcomeChoices(dat)
      outcomes <- selectedModelOutcomes(character(), dat)
      outcomes <- if (length(outcomes) < 3) c(outcomes, rep("", 3 - length(outcomes))) else outcomes
      shiny::updateSelectInput(session, "outcome1", choices = outcomeChoices, selected = outcomes[1])
      shiny::updateSelectInput(session, "outcome2", choices = outcomeChoices, selected = outcomes[2])
      shiny::updateSelectInput(session, "outcome3", choices = outcomeChoices, selected = outcomes[3])
      shiny::updateSelectInput(session, "timeVar", choices = c("None" = "", vars), selected = defaultTimeVar(dat))
      shiny::updateSelectInput(session, "groupVar", choices = vars, selected = defaultGroupVar(dat))
      updateFormulaControls(dat, outcomes)
    })

    shiny::observeEvent(list(input$outcome1, input$outcome2, input$outcome3, input$timeVar, input$groupVar), {
      dat <- selectedData()
      outcomes <- selectedModelOutcomes(c(input$outcome1, input$outcome2, input$outcome3), dat)
      updateFormulaControls(dat, outcomes, input$fixedPredictors)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$fixedPredictors, {
      dat <- selectedData()
      outcomes <- selectedModelOutcomes(c(input$outcome1, input$outcome2, input$outcome3), dat)
      updateFormulaControls(dat, outcomes, input$fixedPredictors)
    }, ignoreInit = TRUE)

    currentSettings <- shiny::reactive({
      dat <- selectedData()
      outcomes <- selectedModelOutcomes(c(input$outcome1, input$outcome2, input$outcome3), dat)
      groupVar <- scalarText(input$groupVar, defaultGroupVar(dat))
      timeVar <- scalarText(input$timeVar, defaultTimeVar(dat))
      predictors <- input$fixedPredictors
      if (is.null(predictors) || length(predictors) == 0) {
        predictors <- defaultPredictors(dat, outcomes, groupVar, timeVar)
      }
      predictorChoices <- setdiff(names(dat), c(intersect(c("index", "user_id", "timepoint", groupVar, timeVar), names(dat)), outcomes, intersect(defaultRiskVars(), names(dat))))
      predictors <- intersect(setdiff(predictors, outcomes), predictorChoices)
      timeInteraction <- scalarText(input$timeInteraction)
      if (!timeInteraction %in% predictors) {
        timeInteraction <- ""
      }
      randomSlopes <- input$randomSlopes
      if (is.null(randomSlopes)) {
        randomSlopes <- character()
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
        groupVar = groupVar,
        timeVar = timeVar,
        timeInteraction = timeInteraction,
        randomSlopes = setdiff(intersect(randomSlopes, names(dat)), c(outcomes, groupVar)),
        logVars = intersect(input$logVars, numericPredictors),
        logMethod = scalarText(input$logMethod, "log"),
        naAction = scalarText(input$naAction, "na.omit"),
        datasetVersion = scalarText(input$datasetVersion, "boxplotRule")
      )
    })

    output$formulaText <- shiny::renderText({
      settings <- currentSettings()
      if (!hasScalarText(settings$groupVar) || length(settings$outcomes) == 0) {
        return("Select outcomes, fixed effects and a grouping variable.")
      }
      fixedTerms <- buildFormulaTerms(settings$predictors, settings$logVars, settings$logMethod)
      if (hasScalarText(settings$timeVar)) {
        timeTerm <- modelTerm(settings$timeVar)
        if (hasScalarText(settings$timeInteraction)) {
          interactionTerm <- buildFormulaTerms(settings$timeInteraction, settings$logVars, settings$logMethod)
          fixedTerms <- c(paste0(timeTerm, " * ", interactionTerm), buildFormulaTerms(setdiff(settings$predictors, settings$timeInteraction), settings$logVars, settings$logMethod))
        } else {
          fixedTerms <- c(timeTerm, fixedTerms)
        }
      }
      fixedTerms <- fixedTerms[!is.na(fixedTerms) & nzchar(fixedTerms)]
      rhs <- c(if (length(fixedTerms) == 0) "1" else fixedTerms, buildRandomEffectTerm(settings$groupVar, settings$randomSlopes))
      forms <- vapply(seq_along(settings$outcomes), function(i) {
        fn <- if (identical(settings$families[i], "gaussian")) "lmer" else "glmer"
        paste0(
          "Model ", i, ": ", fn, "(\n  ",
          modelTerm(settings$outcomes[i]), " ~ ", paste(rhs, collapse = " + "),
          ",\n  data = ", settings$datasetVersion,
          if (identical(fn, "glmer")) paste0(",\n  family = ", settings$families[i]) else "",
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
      shiny::req(
        is.data.frame(dat),
        nrow(dat) > 0,
        length(settings$outcomes) > 0,
        hasScalarText(settings$groupVar)
      )
      shiny::withProgress(message = "Fitting mixed models", value = 0, {
        shiny::incProgress(0.3, detail = "Validating outcomes")
        fits <- fitMixedOutcomeModels(
          dat,
          settings$outcomes,
          settings$families,
          settings$predictors,
          settings$groupVar,
          settings$timeVar,
          settings$timeInteraction,
          settings$randomSlopes,
          settings$naAction,
          settings$logVars,
          settings$logMethod
        )
        shiny::incProgress(0.7, detail = "Summarising models")
        fits
      })
    }, ignoreInit = FALSE)

    output$trajectoryPlot <- shiny::renderPlot({
      settings <- currentSettings()
      shiny::req(settings$groupVar, settings$timeVar)
      dat <- selectedData()
      numericOutcomes <- intersect(settings$outcomes, getNumericVars(dat))
      if (length(numericOutcomes) == 0) {
        numericOutcomes <- defaultAnalysisOutcomes(dat)
      }
      plotOutcomeTrajectories(dat, settings$groupVar, settings$timeVar, numericOutcomes)
    }, res = appPlotResolution())

    observeExpandedPlot(
      input,
      output,
      session,
      "expandTrajectory",
      "trajectoryPlotFull",
      "Outcome trajectories",
      function() {
        settings <- currentSettings()
        shiny::req(settings$groupVar, settings$timeVar)
        dat <- selectedData()
        numericOutcomes <- intersect(settings$outcomes, getNumericVars(dat))
        if (length(numericOutcomes) == 0) {
          numericOutcomes <- defaultAnalysisOutcomes(dat)
        }
        plotOutcomeTrajectories(dat, settings$groupVar, settings$timeVar, numericOutcomes)
      }
    )

    output$summaryCards <- shiny::renderUI({
      shiny::validate(shiny::need(input$runModel > 0, "Review the formulas and click Run model."))
      shiny::validate(shiny::need(length(modelFits()) > 0, "No mixed models are available."))
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

    output$modelTable <- DT::renderDT({
      shiny::validate(shiny::need(input$runModel > 0, "Review the formulas and click Run model."))
      shiny::validate(shiny::need(length(modelFits()) > 0, "No model comparison is available."))
      DT::datatable(mixedOutcomeModelSummary(modelFits()), options = list(scrollX = TRUE, pageLength = 5), rownames = FALSE)
    })
  })
}
