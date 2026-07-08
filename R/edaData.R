# Purpose: Read an uploaded table file.
# Arguments: path to uploaded file, file extension, optional Excel sheet.
# Returns: Data frame.
readUploadedData <- function(path, ext, sheet = NULL) {
  ext <- tolower(ext)

  if (ext %in% c("csv", "txt")) {
    return(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE))
  }

  if (ext %in% c("xlsx", "xls")) {
    if (is.null(sheet) || !nzchar(trimws(sheet))) {
      return(openxlsx::read.xlsx(path, sheet = 1))
    }
    return(openxlsx::read.xlsx(path, sheet = sheet))
  }

  shiny::validate(shiny::need(FALSE, "Upload a .csv, .txt, .xlsx or .xls file."))
}

# Purpose: Return available packaged example datasets.
# Arguments: None.
# Returns: Named character vector of example dataset keys.
getExampleDatasets <- function() {
  c("Iris - datasets package" = "datasets::iris")
}

# Purpose: Load a lightweight packaged example dataset.
# Arguments: Example dataset key.
# Returns: Data frame.
loadExampleDataset <- function(exampleKey) {
  if (identical(exampleKey, "datasets::iris")) {
    return(as.data.frame(datasets::iris))
  }

  shiny::validate(shiny::need(FALSE, "Select a valid example dataset."))
}

# Purpose: Read workbook sheet names safely.
# Arguments: Uploaded file path and extension.
# Returns: Character vector of sheet names, or "1" for non-Excel files.
getWorkbookSheets <- function(path, ext) {
  ext <- tolower(ext)
  if (!ext %in% c("xlsx", "xls")) {
    return("1")
  }

  tryCatch(
    openxlsx::getSheetNames(path),
    error = function(e) character(0)
  )
}

# Purpose: Choose the best default sheet for multi-sheet workbooks.
# Arguments: Character vector of workbook sheet names.
# Returns: One selected sheet name or "1".
chooseDefaultSheet <- function(sheets) {
  if (length(sheets) == 0) {
    return("1")
  }

  combined <- sheets[grepl("complete|combined|merged|full|all", sheets, ignore.case = TRUE)]
  if (length(combined) > 0) {
    return(combined[1])
  }

  dataLike <- setdiff(sheets, c("ReadME", "README", "summary", "dictionary", "columns"))
  if (length(dataLike) > 0) {
    return(dataLike[1])
  }

  sheets[1]
}

# Purpose: Choose the default dictionary sheet when available.
# Arguments: Character vector of workbook sheet names.
# Returns: Sheet name or empty string.
chooseDefaultDictionarySheet <- function(sheets) {
  if (length(sheets) == 0) {
    return("")
  }

  exact <- sheets[tolower(sheets) %in% c("dictionary", "data dictionary", "codebook", "columns")]
  if (length(exact) > 0) {
    return(exact[1])
  }

  partial <- sheets[grepl("dict|codebook|metadata|columns", sheets, ignore.case = TRUE)]
  if (length(partial) > 0) {
    return(partial[1])
  }

  ""
}

# Purpose: Extract variable descriptions and features from a dictionary table.
# Arguments: Data frame read from a dictionary sheet.
# Returns: List containing named description and feature vectors.
parseDictionaryMetadata <- function(dictionaryData) {
  if (is.null(dictionaryData) || nrow(dictionaryData) == 0) {
    return(NULL)
  }

  namesLower <- tolower(gsub("[^a-zA-Z0-9]+", "", names(dictionaryData)))
  columnIndex <- match(TRUE, namesLower %in% c("columnname", "column", "variable", "variablename", "name"))
  descriptionIndex <- match(TRUE, namesLower %in% c("description", "definition", "label", "meaning"))
  featureIndex <- match(TRUE, namesLower %in% c("feature", "group", "category", "domain", "section"))

  if (is.na(columnIndex) || is.na(descriptionIndex)) {
    return(NULL)
  }

  variable <- trimws(as.character(dictionaryData[[columnIndex]]))
  description <- trimws(as.character(dictionaryData[[descriptionIndex]]))
  keep <- !is.na(variable) & nzchar(variable)
  variable <- variable[keep]
  description <- description[keep]

  descriptions <- description
  names(descriptions) <- variable

  features <- character(0)
  if (!is.na(featureIndex)) {
    feature <- trimws(as.character(dictionaryData[[featureIndex]]))[keep]
    features <- feature
    names(features) <- variable
  }

  list(descriptions = descriptions, features = features)
}

# Purpose: Read dictionary metadata from an uploaded workbook.
# Arguments: File path, extension and selected dictionary sheet.
# Returns: Parsed dictionary metadata or NULL.
readDictionaryMetadata <- function(path, ext, dictionarySheet) {
  ext <- tolower(ext)
  if (!ext %in% c("xlsx", "xls") || is.null(dictionarySheet) || !nzchar(trimws(dictionarySheet))) {
    return(NULL)
  }

  dictData <- tryCatch(
    openxlsx::read.xlsx(path, sheet = dictionarySheet),
    error = function(e) NULL
  )

  parseDictionaryMetadata(dictData)
}

# Purpose: Parse a comma-separated abnormal-value list.
# Arguments: Text entered by the user.
# Returns: Character vector of abnormal-value tokens.
parseAbnormalValues <- function(text) {
  if (is.null(text) || length(text) == 0 || all(!nzchar(trimws(as.character(text))))) {
    return(character(0))
  }

  values <- unlist(strsplit(as.character(text), ",", fixed = TRUE), use.names = FALSE)
  values <- trimws(values)
  unique(values[nzchar(values)])
}

# Purpose: Replace configured abnormal-value tokens with NA.
# Arguments: Data frame and character vector of abnormal tokens.
# Returns: Data frame with abnormal tokens replaced by NA.
replaceAbnormalValues <- function(data, errors) {
  if (length(errors) == 0) {
    return(data)
  }

  out <- data
  keys <- tolower(trimws(errors))

  out[] <- lapply(out, function(x) {
    charX <- trimws(tolower(as.character(x)))
    x[is.na(x) | charX %in% keys] <- NA
    x
  })

  out
}

# Purpose: Count abnormal-value tokens before replacement.
# Arguments: Data frame and character vector of abnormal tokens.
# Returns: Data frame with token counts.
countAbnormalValues <- function(data, errors) {
  if (length(errors) == 0) {
    return(data.frame(value = character(), count = integer()))
  }

  x <- unlist(lapply(data, as.character), use.names = FALSE)
  key <- trimws(tolower(x))
  keys <- tolower(trimws(errors))
  isAbnormal <- is.na(x) | key %in% keys

  value <- ifelse(is.na(x), "<NA>", ifelse(trimws(x) == "", "<empty>", x))
  tab <- sort(table(value[isAbnormal]), decreasing = TRUE)

  data.frame(value = names(tab), count = as.integer(tab), row.names = NULL)
}

# Purpose: Convert selected variables to numeric.
# Arguments: Data frame and variable names.
# Returns: Data frame with selected variables converted.
convertNumericVars <- function(data, vars) {
  vars <- intersect(vars, names(data))
  if (length(vars) == 0) {
    return(data)
  }

  out <- data
  out[vars] <- lapply(out[vars], function(x) as.numeric(trimws(as.character(x))))
  out
}

# Purpose: Convert selected variables to integer.
# Arguments: Data frame and variable names.
# Returns: Data frame with selected variables converted.
convertIntegerVars <- function(data, vars) {
  vars <- intersect(vars, names(data))
  if (length(vars) == 0) {
    return(data)
  }

  out <- data
  out[vars] <- lapply(out[vars], function(x) {
    suppressWarnings(as.integer(as.numeric(trimws(as.character(x)))))
  })
  out
}

# Purpose: Convert selected variables to factor.
# Arguments: Data frame and variable names.
# Returns: Data frame with selected variables converted.
convertFactorVars <- function(data, vars) {
  vars <- intersect(vars, names(data))
  if (length(vars) == 0) {
    return(data)
  }

  out <- data
  out[vars] <- lapply(out[vars], as.factor)
  out
}

# Purpose: Convert an Excel serial or character date column to Date.
# Arguments: Data frame, date variable, Excel origin date.
# Returns: Data frame with converted date variable.
convertDateVar <- function(data, dateVar, origin = "1899-12-30") {
  dateVar <- scalarText(dateVar)
  origin <- scalarText(origin, "1899-12-30")
  if (!hasScalarText(dateVar) || !dateVar %in% names(data)) {
    return(data)
  }

  out <- data
  x <- out[[dateVar]]

  if (inherits(x, "Date")) {
    return(out)
  }

  if (is.numeric(x)) {
    out[[dateVar]] <- as.Date(x, origin = origin)
  } else {
    out[[dateVar]] <- as.Date(x)
  }

  out
}

# Purpose: Add an age variable from a birthdate variable.
# Arguments: Data frame, birthdate variable, output variable name.
# Returns: Data frame with age column if possible.
addAgeFromBirthdate <- function(data, birthdateVar, outputVar = "Age") {
  birthdateVar <- scalarText(birthdateVar)
  outputVar <- scalarText(outputVar, "Age")
  if (!hasScalarText(birthdateVar) || !birthdateVar %in% names(data)) {
    if (outputVar %in% names(data) && is.numeric(data[[outputVar]]) &&
        all(is.na(data[[outputVar]]) | data[[outputVar]] == floor(data[[outputVar]]))) {
      data[[outputVar]] <- as.integer(data[[outputVar]])
    }
    return(data)
  }
  if (outputVar %in% names(data)) {
    if (is.numeric(data[[outputVar]]) &&
        all(is.na(data[[outputVar]]) | data[[outputVar]] == floor(data[[outputVar]]))) {
      data[[outputVar]] <- as.integer(data[[outputVar]])
    }
    return(data)
  }

  out <- data
  birthdate <- out[[birthdateVar]]
  if (!inherits(birthdate, "Date")) {
    return(out)
  }

  today <- Sys.Date()
  out[[outputVar]] <- as.integer(format(today, "%Y")) -
    as.integer(format(birthdate, "%Y")) -
    (format(today, "%m%d") < format(birthdate, "%m%d"))

  out
}

# Purpose: Return a variable-class summary table.
# Arguments: Data frame.
# Returns: Data frame with variable names, classes and missing counts.
getVariableSummary <- function(data) {
  data.frame(
    variable = names(data),
    class = vapply(data, function(x) paste(class(x), collapse = ", "), character(1)),
    nMissing = vapply(data, function(x) sum(is.na(x)), integer(1)),
    nUnique = vapply(data, function(x) length(unique(x)), integer(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# Purpose: Get numeric variable names.
# Arguments: Data frame.
# Returns: Character vector of numeric variable names.
getNumericVars <- function(data) {
  names(data)[vapply(data, is.numeric, logical(1))]
}

# Purpose: Get categorical variable names.
# Arguments: Data frame.
# Returns: Character vector of categorical variable names.
getCategoricalVars <- function(data) {
  names(data)[vapply(data, function(x) is.character(x) || is.factor(x), logical(1))]
}

# Purpose: Get integer-like variable names.
# Arguments: Data frame.
# Returns: Character vector of integer-like variable names.
getIntegerVars <- function(data) {
  names(data)[vapply(data, is.integer, logical(1))]
}

# Purpose: Get continuous numeric variable names.
# Arguments: Data frame.
# Returns: Character vector of non-integer numeric variable names.
getContinuousVars <- function(data) {
  setdiff(getNumericVars(data), getIntegerVars(data))
}
