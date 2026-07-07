# shinyEDA

`shinyEDA` is a modular Shiny application for upload-driven exploratory data analysis. It supports local execution with `shiny::runApp()` and static GitHub Pages deployment with `shinylive`.

## Local Run

From the repository root:

```r
install.packages(c(
  "shiny", "bslib", "DT", "ggplot2", "dplyr", "tidyr",
  "openxlsx", "jsonlite", "htmltools", "scales", "rpart"
))

shiny::runApp(".")
```

Upload a CSV, TXT, XLSX, or XLS dataset in **Setup**. For multi-sheet Excel workbooks, the app detects available sheets and prefers sheets with names such as `combined`, `all`, or `complete` when present.

The active uploaded file is retained in the Shiny session and shown in the **Current dataset** status in Setup. Browser file-picker controls may appear blank after navigation or refresh-like UI events; use the status box and preview table as the source of truth for the active dataset.

## App Structure

```text
app.R
R/
  config.R
  downloads.R
  edaData.R
  edaPlots.R
  edaStats.R
  navigation.R
  modules/
www/
tools/
.github/workflows/
```

The left sidebar includes:

- Home
- Setup
- Exploration
- Characterising variables
- Outliers detection
- Inliers detection
- Dictionary

The top-right app bar includes a light/dark display toggle.

Question-response plots auto-detect variables such as `q1`, `q2`, `qA1`, or similar question-style names. The selected question variables can be edited in the categorical sections.

## GitHub Pages Deployment

Push this repository to GitHub, then enable GitHub Pages with **GitHub Actions** as the source. The included workflow exports the app with `shinylive` and deploys the generated static site.

To export locally:

```r
install.packages("shinylive")
```

```bash
Rscript tools/export-shinylive.R
```

To preview the exported static site locally:

```r
httpuv::runStaticServer("shinylive")
```

## Internal Workbook Test

Run the workbook smoke test with an explicit workbook path:

```bash
Rscript tools/test-workbook.R ../example.xlsx
```

Close the workbook in Excel first if it is locked by another process.

## shinylive Compatibility Notes

The app intentionally uses a compact package set to improve browser-based deployment compatibility: `shiny`, `bslib`, `DT`, `ggplot2`, `dplyr`, `tidyr`, `openxlsx`, `jsonlite`, `htmltools`, `scales`, and `rpart`.

If additional packages are introduced later, confirm that WebAssembly binaries are available for shinylive. webR/shinylive cannot install R packages from source inside the browser.
