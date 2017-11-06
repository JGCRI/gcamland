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
