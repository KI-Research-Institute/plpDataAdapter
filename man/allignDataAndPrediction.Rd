% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transformPlpData.R
\name{allignDataAndPrediction}
\alias{allignDataAndPrediction}
\title{Align data and prediction}
\usage{
allignDataAndPrediction(dataXY, plpResults, subsets = c("Train", "Test"))
}
\arguments{
\item{dataXY}{data frame of features and outcome (in column 'outcome')}

\item{plpResults}{plp results object from runPlp that was executed on the same object that is the source of dataXY}

\item{subsets}{a string of 'Train', 'Test' or 'CV' or a subset of them}
}
\value{
new column names
}
\description{
align data and prediction
}
