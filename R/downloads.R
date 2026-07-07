# Purpose: Write a named list of data frames to an XLSX workbook.
# Arguments: Named list of tables and destination file path.
# Returns: File path invisibly.
writeWorkbookTables <- function(tables, file) {
  wb <- openxlsx::createWorkbook()

  for (nm in names(tables)) {
    sheet <- substr(gsub("[^A-Za-z0-9_]", "_", nm), 1, 31)
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, tables[[nm]])
    if (sheet == "summary") {
      openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(tables[[nm]])))
    } else {
      openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(tables[[nm]])), widths = "auto")
    }
  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}

# Purpose: Build a reproducibility parameter log.
# Arguments: Named list of parameters.
# Returns: Two-column data frame.
buildParameterLog <- function(params) {
  data.frame(
    parameter = names(params),
    value = vapply(params, function(x) paste(x, collapse = ", "), character(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# Purpose: Save a ggplot to a requested image format.
# Arguments: ggplot object, file path, extension, dimensions.
# Returns: File path invisibly.
savePlotFile <- function(plot, file, ext = "png", width = 9, height = 6) {
  device <- switch(
    tolower(ext),
    png = "png",
    jpg = "jpeg",
    jpeg = "jpeg",
    tiff = "tiff",
    "png"
  )

  ggplot2::ggsave(file, plot = plot, device = device, width = width, height = height, dpi = 300)
  invisible(file)
}

# Purpose: Build a safe download filename from the uploaded file name.
# Arguments: Source file name, section suffix and output extension.
# Returns: Download filename using source name plus upper-case suffix.
buildDownloadName <- function(fileName, suffix, ext) {
  if (is.null(fileName) || !nzchar(fileName)) {
    base <- "dataset"
  } else {
    base <- tools::file_path_sans_ext(basename(fileName))
  }

  base <- gsub("[^A-Za-z0-9]+", "", base)
  if (!nzchar(base)) {
    base <- "dataset"
  }

  paste0(base, toupper(suffix), ".", tolower(ext))
}
