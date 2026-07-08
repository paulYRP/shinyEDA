library(magick)

makeFrame <- function(step, title, detail, accent = "#2f6f73") {
  img <- image_blank(width = 960, height = 500, color = "#f5f7f8")
  img <- image_draw(img)
  par(mar = c(0, 0, 0, 0), usr = c(0, 960, 0, 500), xaxs = "i", yaxs = "i")

  rect(30, 40, 250, 460, col = "#ffffff", border = "#d9e1e4", lwd = 2)
  text(55, 425, "shinyEDA", adj = 0, cex = 1.8, font = 2, col = "#263238")
  rect(55, 370, 225, 405, col = "#f7f9fa", border = "#d9e1e4")
  text(70, 388, "Search sections", adj = 0, cex = 0.9, col = "#607d86")

  sections <- c("Home", "Setup", "Exploration", "GLM", "LME", "Dictionary")
  y <- seq(330, 150, length.out = length(sections))
  for (i in seq_along(sections)) {
    active <- i == step
    rect(55, y[i] - 18, 225, y[i] + 18, col = if (active) accent else "#ffffff", border = "#d9e1e4")
    text(75, y[i], sections[i], adj = 0, cex = 1, font = if (active) 2 else 1, col = if (active) "#ffffff" else "#263238")
  }

  rect(285, 40, 930, 460, col = "#ffffff", border = "#d9e1e4", lwd = 2)
  text(325, 410, title, adj = 0, cex = 2.2, font = 2, col = "#263238")
  text(325, 365, detail, adj = 0, cex = 1.15, col = "#607d86")

  rect(325, 285, 890, 330, col = "#f7f9fa", border = "#d9e1e4")
  rect(345, 298, 500, 318, col = accent, border = NA)
  rect(525, 298, 680, 318, col = "#d9e1e4", border = NA)
  rect(705, 298, 860, 318, col = "#d9e1e4", border = NA)

  rect(325, 110, 890, 250, col = "#f7f9fa", border = "#d9e1e4")
  for (i in 0:3) {
    rect(350, 220 - i * 28, 865, 238 - i * 28, col = if (i %% 2 == 0) "#ffffff" else "#edf2f3", border = NA)
  }
  text(350, 255, paste("Step", step, "of", length(sections)), adj = 0, cex = 0.9, col = accent)
  dev.off()
  img
}

frames <- list(
  makeFrame(1, "Start", "Review the workflow and choose a section."),
  makeFrame(2, "Upload", "Load a file or use an example dataset."),
  makeFrame(3, "Explore", "Inspect distributions, missingness and categories."),
  makeFrame(4, "Build GLM", "Select outcome, predictors, family and dataset."),
  makeFrame(5, "Build LME", "Choose timepoints, fixed effects and random effects."),
  makeFrame(6, "Export", "Download tables, plots, models and dictionary output.")
)

anim <- image_join(frames) |>
  image_animate(fps = 1, dispose = "previous")

image_write(anim, path = file.path("www", "shinyeda-demo.gif"), format = "gif")
