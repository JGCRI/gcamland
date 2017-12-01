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

  # Save expected yield
  aLandLeaf$mExpectedYield[aPeriod] <- expectedYield

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

  # Get year
  y <- get_per_to_yr(aPeriod)

  # Get price for this leaf in this period only
  if(aLandLeaf$mProductName[1] %in% unique(PRICES$sector)) {
    PRICES %>%
      filter(year == y, sector == aLandLeaf$mProductName[1]) ->
      currPrice

    expectedPrice <- currPrice[[c("price")]]
  } else {
    # TODO: Figure out what to do if price is missing.
    expectedPrice <- 1
  }

  # Save expected price data
  aLandLeaf$mExpectedPrice[aPeriod] <- expectedPrice

  return(expectedPrice)
}
