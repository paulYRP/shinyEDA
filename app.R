# Purpose: Source all R files from one app folder.
# Arguments: Relative directory path.
# Returns: Invisibly returns sourced file paths.
sourceAppDir <- function(path) {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  files <- sort(files)
  invisible(lapply(files, source, local = FALSE))
}

sourceAppDir("R")
loadAppPackages()
sourceAppDir(file.path("R", "modules"))

# Purpose: Wrap one section so navigation can show it without destroying it.
# Arguments: Navigation key and section UI.
# Returns: Conditional Shiny UI panel.
sectionPanel <- function(key, sectionUi) {
  shiny::conditionalPanel(
    condition = sprintf("output.selectedKey == '%s'", key),
    sectionUi
  )
}

# Purpose: Keep all section UIs mounted for the whole app session.
# Arguments: None.
# Returns: Shiny UI containing every app section.
allPagesUi <- function() {
  shiny::tagList(
    sectionPanel("home", homeUi("home")),
    sectionPanel("setup", dataUi("data")),
    sectionPanel("explorationOverview", overviewUi("overview")),
    sectionPanel("numericVariables", numericUi("numeric")),
    sectionPanel("explorationCategorical", categoricalUi("explorationCategorical")),
    sectionPanel("conditioningVariable", conditioningUi("conditioning")),
    sectionPanel("binaryVariables", binaryUi("binary")),
    sectionPanel("characterCategorical", categoricalUi("characterCategorical")),
    sectionPanel("discreteVariables", discreteUi("discrete")),
    sectionPanel("continuousVariables", continuousUi("continuous")),
    sectionPanel("outliersDetection", outliersUi("outliers")),
    sectionPanel("inliersDetection", inliersUi("inliers")),
    sectionPanel("dictionary", dictionaryUi("dictionary"))
  )
}

# Purpose: Render the top-right display mode control.
# Arguments: None.
# Returns: Shiny UI.
topBarUi <- function() {
  shiny::div(
    class = "app-topbar",
    shiny::div(
      class = "top-theme-control",
      bslib::input_dark_mode(id = "themeMode", mode = "light")
    )
  )
}

ui <- bslib::page_fillable(
  title = "shinyEDA",
  theme = appTheme("light"),
  shiny::tags$head(
    shiny::includeCSS(file.path("www", "styles.css"))
  ),
  shiny::div(
    class = "app-shell",
    shiny::tags$aside(class = "app-sidebar", sidebarUi("sidebar")),
    shiny::tags$main(class = "app-main", topBarUi(), allPagesUi())
  )
)

server <- function(input, output, session) {
  nav <- getEdaNav()
  selected <- sidebarServer("sidebar", nav)

  shiny::observeEvent(input$themeMode, {
    mode <- input$themeMode
    if (is.logical(mode)) {
      mode <- if (isTRUE(mode)) "dark" else "light"
    }

    if (is.null(mode) || length(mode) == 0) {
      mode <- "light"
    }

    mode <- tolower(mode[[1]])
    if (!mode %in% c("light", "dark")) {
      mode <- "light"
    }

    if (is.function(session$setCurrentTheme)) {
      session$setCurrentTheme(appTheme(mode))
    }
  }, ignoreInit = FALSE)

  dataState <- dataServer("data")
  homeServer("home")
  overviewServer("overview", dataState$cleanData, dataState$abnormalTable, dataState$activeFileName)
  numericServer("numeric", dataState$cleanData, dataState$activeFileName)
  categoricalServer("explorationCategorical", dataState$cleanData, dataState$activeFileName)
  conditioningServer("conditioning", dataState$cleanData, dataState$activeFileName)
  binaryServer("binary", dataState$cleanData, dataState$activeFileName)
  categoricalServer("characterCategorical", dataState$cleanData, dataState$activeFileName)
  discreteServer("discrete", dataState$cleanData, dataState$activeFileName)
  continuousServer("continuous", dataState$cleanData, dataState$activeFileName)
  outlierState <- outliersServer("outliers", dataState$cleanData, dataState$activeFileName)
  inliersServer("inliers", dataState$cleanData, dataState$activeFileName)
  dictionaryServer("dictionary", dataState$cleanData, dataState$params, outlierState, dataState$activeFileName, dataState$dictionaryMetadata)

  output$selectedKey <- shiny::renderText({
    selected()
  })
  shiny::outputOptions(output, "selectedKey", suspendWhenHidden = FALSE)
}

shiny::shinyApp(ui, server)
