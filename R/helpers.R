# helpers.R

# Extra functions that may be useful

#' get_yr_to_per
#'
#' @param aYear Year to convert
#' @param aScenType Scenario type: either "Reference" or "Hindcast"
#' @details Convert a year to a period number
#' @return Period number
#' @author KVC October 2017
get_yr_to_per <- function(aYear, aScenType) {
  per <- -1
  if(aYear %in% YEARS[[aScenType]]) {
    per <- which(aYear == YEARS[[aScenType]])
  } else{
    per <- max(which(aYear > YEARS[[aScenType]]))
  }

  return(per)
}

#' get_per_to_yr
#'
#' @param aPer Period to convert
#' @param aScenType Scenario type: either "Reference" or "Hindcast"
#' @details Convert a period to a year
#' @return Year
#' @author KVC October 2017
get_per_to_yr <- function(aPer, aScenType) {
  return(YEARS[[aScenType]][[aPer]])
}

#' get_timestep
#'
#' @param aPer Period to get timestep for
#' @param aScenType Scenario type: either "Reference" or "Hindcast"
#' @details Calculate the length of the timestep leading up to a particular period
#' @return Years
#' @author KVC October 2017
get_timestep <- function(aPer, aScenType) {
  if(aPer > 1 && aPer <= length(YEARS[[aScenType]])) {
    yrs <- YEARS[[aScenType]][[aPer]] - YEARS[[aScenType]][[aPer - 1]]
  } else{
    stop("Invalid period passed to get_timestep")
  }

  return(yrs)
}

#' getStartYear
#'
#' @details Get model start year
#' @param aScenType Scenario type: either "Reference" or "Hindcast"
#' @return Year
#' @author KVC October 2017
getStartYear <- function(aScenType) {
  return(min(YEARS[[aScenType]]))
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
#' @param aYears Years for Adaptive or Linear expectations (NULL for Perfect)
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
  } else if (aExpectation == "Adaptive") {
    scenNameAdj <- paste(aScenName, "_", aExpectation, aYears, sep="")
  } else if (aExpectation == "HybridLinearAdaptive") {
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
