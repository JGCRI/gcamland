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
#' @importFrom readr read_csv
#' @author KVC October 2017
PerfectExpectation_calcExpectedPrice <- function(aLandLeaf, aPeriod){
  # Read in prices
  prices <- suppressMessages(read_csv("./inst/extdata/calibration-data/price.csv"))

  # Get price for this leaf in this period only
  prices %>%
    filter(Period == aPeriod, Product == aLandLeaf$mName) ->
    prices

  expectedPrice <- prices[[c("mPrice")]]

  return(expectedPrice)
}
