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
  } else if (aExpectation == "Mixed") {
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

#' LandAllocator_getLandAreaByCriteria
#'
#' @details Calculate total land area that meets a particular criteria.
#'          Criteria is currently determined by a string appearing in the
#'          name of the land type
#' @param aLandAllocator Land allocator
#' @param aString String to group land area by
#' @param aPeriod Model period to get land area for
#'
#' @return Land area
LandAllocator_getLandAreaByCriteria <- function(aLandAllocator, aString, aPeriod) {
  # Set total land to 0
  totalLand <- 0

  for(child in aLandAllocator$mChildren) {
    if(inherits(child, "LandNode")) {
      totalLand <- totalLand + LandNode_getLandAreaByCriteria(child, aString, aPeriod)
    } else {
      totalLand <- totalLand + LandLeaf_getLandAreaByCriteria(child, aString, aPeriod)
    }
  }

  return(totalLand)
}

#' LandNode_getLandAreaByCriteria
#'
#' @details Calculate total land area that meets a particular criteria.
#'          Criteria is currently determined by a string appearing in the
#'          name of the land type
#' @param aLandNode Land allocator
#' @param aString String to group land area by
#' @param aPeriod Model period to get land area for
#'
#' @return Land area
LandNode_getLandAreaByCriteria <- function(aLandNode, aString, aPeriod) {
  # Set land to zero for this node
  land <- 0

  # Loop over children, calling their `getLandAreaByCriteria` function
  for(child in aLandNode$mChildren) {
    if(inherits(child, "LandNode")) {
      land <- land + LandNode_getLandAreaByCriteria(child, aString, aPeriod)
    } else {
      land <- land + LandLeaf_getLandAreaByCriteria(child, aString, aPeriod)
    }
  }

  return(land)
}


#' LandLeaf_getLandAreaByCriteria
#'
#' @details Calculate total land area that meets a particular criteria.
#'          Criteria is currently determined by a string appearing in the
#'          name of the land type
#' @param aLandLeaf Land allocator
#' @param aString String to group land area by
#' @param aPeriod Model period to get land area for
#'
#' @return Land area for this leaf
LandLeaf_getLandAreaByCriteria <- function(aLandLeaf, aString, aPeriod) {
  if(grepl(aString, aLandLeaf$mName[1])) {
    return(aLandLeaf$mLandAllocation[[aPeriod]])
  } else {
    return(0)
  }
}
