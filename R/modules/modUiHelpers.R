# Purpose: Render a horizontal control strip.
# Arguments: Control inputs passed through ...
# Returns: A bslib card containing controls in a responsive grid.
controlCard <- function(...) {
  bslib::card(
    class = "control-card",
    bslib::card_header("Controls"),
    shiny::div(class = "control-grid", ...)
  )
}

# Purpose: Render a native single-select dropdown with app-wide styling.
# Arguments: Input id, label, choices and optional selected value.
# Returns: A Shiny select input that avoids selectize overlay issues.
dropdownInput <- function(inputId, label, choices = NULL, selected = NULL) {
  shiny::selectInput(
    inputId,
    label,
    choices = choices,
    selected = selected,
    selectize = FALSE
  )
}

# Purpose: Render a checkbox-based dropdown for multi-select controls.
# Arguments: Input id, label, choices, selected values and placeholder text.
# Returns: A compact dropdown containing a Shiny checkbox group.
checkboxDropdownInput <- function(inputId, label, choices = NULL, selected = NULL, placeholder = "Select values") {
  shiny::div(
    class = "checkbox-dropdown shiny-input-container",
    `data-placeholder` = placeholder,
    shiny::tags$label(class = "control-label", label),
    shiny::tags$button(
      type = "button",
      class = "checkbox-dropdown-toggle",
      shiny::tags$span(class = "checkbox-dropdown-summary", placeholder),
      shiny::tags$span(class = "checkbox-dropdown-arrow", "\u25be")
    ),
    shiny::tags$div(
      class = "checkbox-dropdown-menu",
      shiny::checkboxGroupInput(inputId, label = NULL, choices = choices, selected = selected)
    )
  )
}

# Purpose: Render a compact formula preview block.
# Arguments: Output id created with ns().
# Returns: A bslib card with a verbatim formula display.
formulaCard <- function(outputId) {
  bslib::card(
    class = "formula-card",
    bslib::card_header("Formula"),
    shiny::verbatimTextOutput(outputId)
  )
}

# Purpose: Render one compact model summary card.
# Arguments: Card title and summary text.
# Returns: Shiny UI for a scrollable model summary.
modelSummaryCard <- function(title, summaryText) {
  shiny::div(
    class = "model-summary-card",
    shiny::h4(title),
    shiny::tags$pre(summaryText)
  )
}

# Purpose: Render a plot card with a top-right expand button.
# Arguments: Card title, plot output id, expand button id, plot height and optional child UI.
# Returns: A bslib card containing the plot and optional UI below it.
expandablePlotCard <- function(title, plotOutputId, expandInputId, height = 420, ...) {
  bslib::card(
    bslib::card_header(
      shiny::div(
        class = "plot-card-header",
        shiny::span(title),
        shiny::actionButton(
          expandInputId,
          label = NULL,
          icon = shiny::icon("expand"),
          class = "btn btn-sm btn-outline-secondary plot-expand-button",
          title = "Expand plot"
        )
      )
    ),
    shiny::plotOutput(plotOutputId, height = height),
    ...
  )
}

# Purpose: Show a larger modal containing an existing plot output.
# Arguments: Module session, modal title, un-namespaced output id and modal plot height.
# Returns: Opens a Shiny modal.
showExpandedPlot <- function(session, title, outputId, height = "760px") {
  shiny::showModal(
    shiny::modalDialog(
      title = title,
      shiny::div(
        class = "expanded-plot-wrap",
        shiny::plotOutput(session$ns(outputId), height = height)
      ),
      size = "xl",
      easyClose = TRUE,
      footer = shiny::modalButton("Close")
    )
  )
}

# Purpose: Register a full-size plot output and connect it to an expand button.
# Arguments: Module input/output/session, button id, output id, title and plotting function.
# Returns: Observer for the expand button.
observeExpandedPlot <- function(input, output, session, buttonId, outputId, title, plotFunc, height = "760px") {
  output[[outputId]] <- shiny::renderPlot({
    plotFunc()
  }, res = appPlotResolution())

  shiny::observeEvent(input[[buttonId]], {
    modalHeight <- if (is.function(height)) height() else height
    showExpandedPlot(session, title, outputId, modalHeight)
  })
}
