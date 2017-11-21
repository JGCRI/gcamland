# helpers.R

# Extra functions that may be useful

#' get_yr_to_per
#'
#' @param aYear Year to convert
#' @details Convert a year to a period number
#' @return Period number
#' @author KVC October 2017
get_yr_to_per <- function(aYear) {
  per <- -1
  if(aYear %in% YEARS) {
    per <- which(aYear == YEARS)
  } else{
    per <- max(which(aYear > YEARS))
  }

  return(per)
}

#' get_per_to_yr
#'
#' @param aPer Period to convert
#' @details Convert a period to a year
#' @return Year
#' @author KVC October 2017
get_per_to_yr <- function(aPer) {
  return(YEARS[[aPer]])
}

#' get_timestep
#'
#' @param aPer Period to get timestep for
#' @details Calculate the length of the timestep leading up to a particular period
#' @return Years
#' @author KVC October 2017
get_timestep <- function(aPer) {
  if(aPer > 1){
    yrs <- YEARS[[aPer]] - YEARS[[aPer - 1]]
  } else{
    print("ERROR: Invalid period passed to get_timestep")
  }

  return(yrs)
}

#' getStartYear
#'
#' @details Get model start year
#' @return Year
#' @author KVC October 2017
getStartYear <- function() {
  return(min(YEARS))
}


#' \%!in\%
#'
#' @param x list of things to exclude
#' @param y full list
#' @rdname NotIn
#' @author KVC October 2017
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



