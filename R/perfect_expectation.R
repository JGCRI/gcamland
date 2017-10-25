# perfect_expectation.R

#' PerfectExpectation_calcExpectedYield
#'
#' @param aLandLeaf LandLeaf to calculate expected yield for
#' @param aPeriod Current model period
#' @details Calculate the expected yield assuming perfect
#'          information (i.e., expected = actual).
#' @author KVC October 2017
PerfectExpectation_calcExpectedYield <- function(aLandLeaf, aPeriod) {
  expectedYield <- aLandLeaf$mYield[[aPeriod]]

  return(expectedYield)
}

#' PerfectExpectation_calcExpectedPrice
#'
#' @param aLandLeaf LandLeaf to calculate expected price for
#' @param aPeriod Current model period
#' @details Calculate the expected price for a LandLeaf assuming
#'          perfect expectations, i.e., expected = actual
#' @author KVC October 2017
PerfectExpectation_calcExpectedPrice <- function(aLandLeaf, aPeriod){
  # Silence package checks
  year <- sector <- NULL

  # Read in prices
  prices <- get_prices()

  # Get year
  y <- get_per_to_yr(aPeriod)

  # Get price for this leaf in this period only
  prices %>%
    filter(year == y, sector == aLandLeaf$mProductName[1]) ->
    currPrice

  expectedPrice <- currPrice[[c("price")]]

  return(expectedPrice)
}
