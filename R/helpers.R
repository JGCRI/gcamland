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
