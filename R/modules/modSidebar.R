# Purpose: Render one navigation section.
# Arguments: Namespace function, section name and section rows.
# Returns: Details block containing section buttons.
sidebarSectionUi <- function(ns, sectionName, rows) {
  buttons <- lapply(seq_len(nrow(rows)), function(i) {
    key <- rows$key[i]
    shiny::tags$button(
      id = ns(paste0("nav_", key)),
      type = "button",
      class = paste("nav-button", if (identical(key, "home")) "active" else ""),
      `data-nav-key` = key,
      `data-nav-label` = paste(rows$section[i], rows$label[i]),
      `data-nav-input` = ns("selectedKey"),
      rows$label[i]
    )
  })

  shiny::tags$details(
    open = TRUE,
    `data-nav-section` = sectionName,
    shiny::tags$summary(sectionName),
    buttons
  )
}

# Purpose: Render documentation-style navigation sidebar.
# Arguments: Module id and navigation data frame.
# Returns: Shiny UI.
sidebarUi <- function(id, nav = getEdaNav()) {
  ns <- shiny::NS(id)
  sectionTags <- lapply(unique(nav$section), function(sectionName) {
    rows <- nav[nav$section == sectionName, , drop = FALSE]
    sidebarSectionUi(ns, sectionName, rows)
  })

  shiny::tagList(
    shiny::div(
      class = "sidebar-title",
      shiny::h2("shinyEDA")
    ),
    shiny::div(
      class = "sidebar-search",
      shiny::textInput(ns("search"), label = NULL, placeholder = "Search sections")
    ),
    shiny::div(class = "nav-tree", sectionTags)
  )
}

# Purpose: Server logic for sidebar navigation.
# Arguments: Module id and navigation data frame.
# Returns: Reactive selected navigation key.
sidebarServer <- function(id, nav) {
  shiny::moduleServer(id, function(input, output, session) {
    selected <- shiny::reactive({
      key <- input$selectedKey
      if (is.null(key) || !key %in% nav$key) {
        return("home")
      }
      key
    })

    selected
  })
}
