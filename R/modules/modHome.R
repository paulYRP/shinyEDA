# Purpose: UI for landing page.
# Arguments: Module id.
# Returns: Shiny UI.
homeUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h2("shinyEDA"),
    shiny::p("Upload a dataset, review structure and distributions, and export reproducible summary workbooks."),
    bslib::card(
      bslib::card_header("Resources and references"),
      shiny::div(
        class = "home-resource-grid",
        shiny::tags$a(
          href = "https://github.com/paulYRP/shinyEDA",
          target = "_blank",
          rel = "noopener noreferrer",
          class = "resource-link",
          shiny::span(class = "resource-circle", shiny::icon("github")),
          shiny::span(class = "resource-title", "GitHub")
        ),
        shiny::tags$a(
          href = "https://orcid.org/0009-0007-6714-3566",
          target = "_blank",
          rel = "noopener noreferrer",
          class = "resource-link",
          shiny::span(class = "resource-circle orcid-circle", shiny::icon("orcid")),
          shiny::span(class = "resource-title", "ORCID")
        ),
        shiny::tags$a(
          href = "https://shiny.posit.co/",
          target = "_blank",
          rel = "noopener noreferrer",
          class = "resource-link",
          shiny::span(class = "resource-circle shiny-circle", shiny::span(class = "shiny-wordmark", "Shiny")),
          shiny::span(class = "resource-title", "Shiny")
        ),
        shiny::tags$a(
          href = "https://www.routledge.com/Exploratory-Data-Analysis-Using-R/Pearson/p/book/9780367571566",
          target = "_blank",
          rel = "noopener noreferrer",
          class = "resource-link",
          shiny::span(class = "resource-circle", shiny::icon("book-open")),
          shiny::span(class = "resource-title", "Reference")
        ),
        shiny::tags$a(
          href = "https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/00Index.html",
          target = "_blank",
          rel = "noopener noreferrer",
          class = "resource-link",
          shiny::span(class = "resource-circle datasets-circle", shiny::span(class = "datasets-wordmark", "R")),
          shiny::span(class = "resource-title", "R")
        )
      )
    ),
    bslib::card(
      bslib::card_header("Workflow map"),
      shiny::tags$ul(
        shiny::tags$li("Setup: upload data and apply cleaning controls."),
        shiny::tags$li("Exploration: inspect structure, missingness, numeric and categorical patterns."),
        shiny::tags$li("Characterising variables: binary, categorical, discrete and continuous summaries."),
        shiny::tags$li("Outliers detection: three-sigma, Hampel and boxplot-rule diagnostics."),
        shiny::tags$li("Dictionary: export reproducible output tables and parameters.")
      )
    ),
    bslib::card(
      bslib::card_header("README"),
      shiny::uiOutput(ns("readme"))
    )
  )
}

# Purpose: Locate the app README file.
# Arguments: None.
# Returns: Normalized README path or NA when unavailable.
findReadmePath <- function() {
  candidates <- c("README.md", file.path(getwd(), "README.md"))
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) {
    return(NA_character_)
  }

  normalizePath(found[1], winslash = "/", mustWork = TRUE)
}

# Purpose: Server for landing page.
# Arguments: Module id.
# Returns: None.
homeServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    readmePath <- findReadmePath()

    readmeText <- shiny::reactiveFileReader(
      intervalMillis = 1000,
      session = session,
      filePath = readmePath,
      readFunc = function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
    )

    output$readme <- shiny::renderUI({
      if (is.na(readmePath) || !file.exists(readmePath)) {
        return(shiny::p("README.md was not found."))
      }

      text <- readmeText()
      if (requireNamespace("markdown", quietly = TRUE)) {
        rendered <- tryCatch(
          htmltools::HTML(markdown::markdownToHTML(text = text, fragment.only = TRUE)),
          error = function(e) NULL
        )
        if (!is.null(rendered)) {
          return(rendered)
        }
      }

      shiny::pre(class = "readme-source", shiny::code(text))
    })
  })
}
