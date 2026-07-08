# Purpose: UI for landing page.
# Arguments: Module id.
# Returns: Shiny UI.
homeUi <- function(id) {
  shiny::tagList(
    shiny::h2("shinyEDA"),
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
      ),
    ),
    bslib::card(
      bslib::card_header("Demo"),
      shiny::div(
        class = "home-demo",
        shiny::tags$img(
          src = "shinyeda-demo.gif",
          alt = "Animated walkthrough showing upload, setup, exploration, modelling and export steps.",
          class = "home-demo-media"
        )
      )
    )
  )
}

# Purpose: Server for landing page.
# Arguments: Module id.
# Returns: None.
homeServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    invisible(NULL)
  })
}
