# Purpose: Render documentation-style navigation sidebar.
# Arguments: Module id.
# Returns: Shiny UI.
sidebarUi <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "sidebar-title",
      shiny::h2("shinyEDA")
    ),
    shiny::textInput(ns("search"), label = NULL, placeholder = "Search sections"),
    shiny::uiOutput(ns("navUi"))
  )
}

# Purpose: Server logic for sidebar navigation.
# Arguments: Module id and navigation data frame.
# Returns: Reactive selected navigation key.
sidebarServer <- function(id, nav) {
  shiny::moduleServer(id, function(input, output, session) {
    selected <- shiny::reactiveVal("home")

    output$navUi <- shiny::renderUI({
      ns <- session$ns
      filtered <- filterNav(nav, input$search)

      sectionTags <- lapply(unique(filtered$section), function(sectionName) {
        rows <- filtered[filtered$section == sectionName, , drop = FALSE]
        buttons <- lapply(seq_len(nrow(rows)), function(i) {
          shiny::actionButton(
            ns(paste0("nav_", rows$key[i])),
            label = rows$label[i],
            class = "nav-button"
          )
        })

        shiny::tags$details(
          open = TRUE,
          shiny::tags$summary(sectionName),
          buttons
        )
      })

      shiny::tagList(sectionTags)
    })

    shiny::observe({
      lapply(nav$key, function(key) {
        shiny::observeEvent(input[[paste0("nav_", key)]], {
          selected(key)
        }, ignoreInit = TRUE)
      })
    })

    selected
  })
}
