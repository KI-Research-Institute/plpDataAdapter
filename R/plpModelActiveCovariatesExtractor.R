#' get active model covariates of a logistic regression model
#'
#' @description get model covariate of a logistic model whose coefficients are different than 0
#'
#' @param plpResults a plp result object obtain by runPlp
#'
#' @return list of covariate names
#'
#' @export
getLRModelCovariates <- function(plpResults) {

  covariateImportance <- plpResults$model$covariateImportance
  importantCovariates <- covariateImportance[abs(covariateImportance['covariateValue']) > 0, ]
  estimationCovariates <- importantCovariates[['covariateName']]
  estimationCovariates <- makeFriendlyNames(estimationCovariates)
  return(estimationCovariates)

}
