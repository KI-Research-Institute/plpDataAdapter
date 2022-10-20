#' Transform plp data to a data frame
#'
#' @description Transform plp data to a dense data frame. Set row names to the original rowId of the input
#'
#' @param plpData plpData
#' @param populationSettings population settings
#' @param outcomeId outcome id
#'
#' @return a data frame whose row ids or the original rowId of the input. The columns include features and the outcome
#'   (in column 'outcome')
#'
#' @export
transformPlpDataToDataFrame <- function(plpData, populationSettings, outcomeId) {
  if(!is.null(plpData)){
    labels <- PatientLevelPrediction::createStudyPopulation(
      plpData = plpData,
      outcomeId = outcomeId,
      populationSettings = populationSettings
    )
  } else
    return(NULL)
  # convert to matrix
  dataObject <- PatientLevelPrediction::toSparseM(
    plpData = plpData,
    cohort = labels
  )
  columnDetails <- as.data.frame(dataObject$covariateRef)

  cnames <- columnDetails$covariateName[order(columnDetails$columnId)]

  ipMat <- as.matrix(dataObject$dataMatrix)
  ipdata <- as.data.frame(ipMat)
  colnames(ipdata) <-  makeFriendlyNames(cnames)
  ipdata$outcome <- dataObject$labels$outcomeCount
  rownames(ipdata) <- dataObject$labels$originalRowId  # maintain original rowId to allow alignment with predictions
  return(ipdata)
}

#' Make friendly names
#'
#' @description remove punctuation marks and spaces
#'
#' @param columnNames column names
#'
#' @return new column names
makeFriendlyNames <- function(columnNames){

  columnNames <- gsub("[[:punct:]]", " ", columnNames)
  columnNames <- gsub(" ", "_", columnNames)
  return(columnNames)

}


#' Align data and prediction
#'
#' @description align data and prediction
#'
#' @param dataXY data frame of features and outcome (in column 'outcome')
#' @param plpResults plp results object from runPlp that was executed on the same object that is the source of dataXY
#' @param subsets a string of 'Train', 'Test' or 'CV' or a subset of them
#'
#' @return new column names
#'
#' @export
allignDataAndPrediction <- function(dataXY, plpResults, subsets=c('Train', 'Test')) {

  if (any(!(subsets %in% c('Train', 'Test', 'CV'))))
    stop("subsets should be one of ('Train', 'Test', 'CV')")
  if (!('outcome' %in% names(dataXY)))
    stop("missing column 'outcome' in dataXY")
  evalIdx <- plpResults$prediction$evaluationType %in% subsets
  prediction <- plpResults$prediction[evalIdx, ]
  internalXYP <- merge(prediction, dataXY, by.y = 'row.names', by.x = 'rowId')
  if (nrow(internalXYP) < nrow(prediction))
    stop('Missing items in row names of dataXY')
  rownames(internalXYP) <- internalXYP[['rowId']]
  dataXY <- internalXYP[, names(dataXY)]
  prediction <- internalXYP[, names(prediction)]
  if (sum(prediction$outcomeCount != dataXY[['outcome']]) > 0)
    stop('need to allign prediction and dataXY')
  return(list(dataXY=dataXY, prediction=prediction))

}
