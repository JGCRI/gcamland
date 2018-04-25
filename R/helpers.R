# helpers.R

# Extra functions that may be useful

#' get_yr_to_per
#'
#' @param aYear Year to convert
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#' @details Convert a year to a period number
#' @return Period number
#' @author KVC October 2017
get_yr_to_per <- function(aYear, scentype) {
  per <- -1
  if(aYear %in% YEARS[[scentype]]) {
    per <- which(aYear == YEARS[[scentype]])
  } else{
    per <- max(which(aYear > YEARS[[scentype]]))
  }

  return(per)
}

#' get_per_to_yr
#'
#' @param aPer Period to convert
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#' @details Convert a period to a year
#' @return Year
#' @author KVC October 2017
get_per_to_yr <- function(aPer, scentype) {
  return(YEARS[[scentype]][[aPer]])
}

#' get_timestep
#'
#' @param aPer Period to get timestep for
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#' @details Calculate the length of the timestep leading up to a particular period
#' @return Years
#' @author KVC October 2017
get_timestep <- function(aPer, scentype) {
  if(aPer > 1){
    yrs <- YEARS[[scentype]][[aPer]] - YEARS[[scentype]][[aPer - 1]]
  } else{
    stop("Invalid period passed to get_timestep")
  }

  return(yrs)
}

#' getStartYear
#'
#' @details Get model start year
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#' @return Year
#' @author KVC October 2017
getStartYear <- function(scentype) {
  return(min(YEARS[[scentype]]))
}


# %!in%
#
# @param x list of things to exclude
# @param y full list
# @author KVC October 2017
'%!in%' <- function(x, y) {
  !('%in%'(x, y))
}

#' getScenName
#'
#' @details Get the scenario name to use in ensemble runs
#' @param aScenName Scenario base name
#' @param aExpectation Expectation type
#' @param aYears Years for Lagged or Linear expectations (NULL for Perfect)
#' @param aAgFor Logit exponent for AgroForestLand
#' @param aAgForNonPast Logit exponent for AgroForestLand_NonPasture
#' @param aCrop Logit exponent for Cropland
#'
#' @return Scenario name
#' @author KVC November 2017
getScenName <- function(aScenName, aExpectation, aYears, aAgFor, aAgForNonPast, aCrop) {
  # Add expectation information
  if(aExpectation == "Linear") {
    scenNameAdj <- paste(aScenName, "_", aExpectation, aYears, sep="")
  } else if (aExpectation == "Lagged") {
    scenNameAdj <- paste(aScenName, "_", aExpectation, aYears, sep="")
  } else {
    scenNameAdj <- paste(aScenName, "_", aExpectation, sep="")
  }

  # Add logit info
  scenNameAdj <- paste(scenNameAdj, "_AgroForest", aAgFor, sep="")
  scenNameAdj <- paste(scenNameAdj, "_AgroForestNonPasture", aAgForNonPast, sep="")
  scenNameAdj <- paste(scenNameAdj, "_Cropland", aCrop, sep="")

  return(scenNameAdj)
}

#' Sum values, minimizing roundoff error
#'
#' This version of \code{sum} should be used when the values to be summed are
#' likely to differ dramatically in magnitude, such as when summing likelihood
#' values.
#'
#' @param x Values to be summed.
#' @keywords internal
sumx <- function(x)
{
    sum(x[order(abs(x))])
}
