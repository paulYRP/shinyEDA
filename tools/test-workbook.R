# Purpose: Run an internal workbook smoke test against analysis sheets.
# Arguments: Workbook path.
# Returns: Prints one summary block per tested sheet and stops on errors.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Pass a workbook path, for example: Rscript tools/test-workbook.R ../example.xlsx")
}

workbook <- args[1]
if (!file.exists(workbook)) {
  stop("Workbook not found: ", workbook)
}

source("R/config.R")
source("R/edaData.R")
source("R/edaStats.R")
source("R/edaPlots.R")

availableSheets <- getWorkbookSheets(workbook, "xlsx")
skipSheets <- c("ReadME", "README", "summary", "dictionary", "columns")
sheets <- setdiff(availableSheets, skipSheets)
if (length(sheets) == 0) {
  stop("No analysis sheets found in workbook.")
}

dictionarySheet <- chooseDefaultDictionarySheet(availableSheets)
dictionaryMetadata <- readDictionaryMetadata(workbook, "xlsx", dictionarySheet)

for (sheet in sheets) {
  rawDat <- readUploadedData(workbook, "xlsx", sheet)
  cleanDat <- replaceAbnormalValues(rawDat, defaultAbnormalValues())
  cleanDat <- convertNumericVars(cleanDat, suggestNumericVars(rawDat))

  birthdateVar <- ""
  cleanDat <- convertDateVar(cleanDat, birthdateVar, "1899-12-30")
  cleanDat <- addAgeFromBirthdate(cleanDat, birthdateVar, "Age")

  miss <- missingSummary(cleanDat)
  catVars <- setdiff(getCategoricalVars(cleanDat), c("RiskStress", "RiskDiabetes2", "RiskCardiovascular"))
  numVars <- getNumericVars(cleanDat)
  intVars <- getIntegerVars(cleanDat)
  contVars <- getContinuousVars(cleanDat)
  qVars <- intersect(detectQuestionVars(cleanDat), names(cleanDat))
  dict <- variableDictionarySummary(cleanDat, dictionaryMetadata)

  outlierVars <- setdiff(names(cleanDat), c("index", "user_id", "timepoint", "answered_at", "birthdate"))
  out <- detectOutliers(cleanDat[outlierVars])
  exportTables <- buildEdaExportTables(cleanDat, out, dictionaryMetadata)

  if (length(numVars) > 0) {
    distributionTable(cleanDat[numVars])
    plotNumericDistribution(cleanDat, numVars[1], FALSE)
  }
  if (length(catVars) > 0) {
    categoricalTable(cleanDat[catVars])
  }
  if (length(intVars) > 0) {
    discreteTable(cleanDat[intVars])
  }
  if (length(qVars) > 0) {
    plotQuestionResponses(cleanDat, qVars)
  }

  cat("\n", sheet, "\n", sep = "")
  cat("rows=", nrow(cleanDat), " cols=", ncol(cleanDat), "\n", sep = "")
  cat("suggestNumeric=", paste(suggestNumericVars(rawDat), collapse = ", "), "\n", sep = "")
  cat(
    "numeric=", length(numVars),
    " categorical=", length(catVars),
    " integer=", length(intVars),
    " continuous=", length(contVars),
    " questions=", length(qVars), "\n",
    sep = ""
  )
  cat(
    "missingMax=", max(miss$nMissing),
    " dictionaryRows=", nrow(dict),
    " outlierRows=", nrow(out$detail),
    " exportSheets=", paste(names(exportTables), collapse = ", "), "\n",
    sep = ""
  )
}

cat("\ninternal_workbook_test_ok\n")
