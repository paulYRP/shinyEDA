# shinyEDA

`shinyEDA` is a modular Shiny application for upload-driven exploratory data analysis. It supports local execution with `shiny::runApp()` and static GitHub Pages deployment with `shinylive`.

## Local Run

From the repository root:

```r
install.packages(c(
  "shiny", "bslib", "DT", "ggplot2", "dplyr", "tidyr",
  "openxlsx", "jsonlite", "htmltools", "scales", "rpart",
  "MASS", "lme4", "lmerTest", "performance"
))

shiny::runApp(".")
```

Upload a CSV, TXT, XLSX, or XLS dataset in **Setup**. For multi-sheet Excel workbooks, the app detects available sheets.

The Setup page also includes a lightweight example dataset, `datasets::iris`.

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
- GLM analysis
- LME analysis
- Dictionary

The top-right app bar includes a light/dark display toggle.

## GitHub Pages Deployment

The included workflow exports the app with `shinylive` and deploys the generated static site.

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

The app uses `shiny`, `bslib`, `DT`, `ggplot2`, `dplyr`, `tidyr`, `openxlsx`, `jsonlite`, `htmltools`, `scales`, `rpart`, `MASS`, `lme4`, `lmerTest`, and `performance`.

If additional packages are introduced later, confirm that WebAssembly binaries are available for shinylive. webR/shinylive cannot install R packages from source inside the browser.
