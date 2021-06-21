# perfect_expectation.R

#' PerfectExpectation_calcExpectedYield
#'
#' @param aLandLeaf LandLeaf to calculate expected yield for
#' @param aPeriod Current model period
#' @param aScenarioInfo Scenario parameter structure
#' @details Calculate the expected yield assuming perfect
#'          information (i.e., expected = actual).
#' @author KVC October 2017
PerfectExpectation_calcExpectedYield <- function(aLandLeaf, aPeriod, aScenarioInfo) {
  expectedYield <- aLandLeaf$mYield[[aPeriod]]

  # Save expected yield
  aLandLeaf$mExpectedYield[aPeriod] <- expectedYield

  return(expectedYield)
}

#' PerfectExpectation_calcExpectedPrice
#'
#' @param aLandLeaf LandLeaf to calculate expected price for
#' @param aPeriod Current model period
#' @param aScenarioInfo Scenario parameter structure
#' @details Calculate the expected price for a LandLeaf assuming
#'          perfect expectations, i.e., expected = actual
#' @author KVC October 2017
PerfectExpectation_calcExpectedPrice <- function(aLandLeaf, aPeriod, aScenarioInfo){
  # Silence package checks
  year <- sector <- NULL

  # Get year
  y <- get_per_to_yr(aPeriod, aScenarioInfo$mScenarioType)

  # Get price for this leaf in this period only
  price_table <- PRICES[[aScenarioInfo$mScenarioType]]
  if(aLandLeaf$mProductName[1] %in% price_table$sector) {
    if(aScenarioInfo$mScenarioType == "PCHES") {
      # PCHES uses subregional prices. First, figure out the subregion by decomposing the land leaf name
      # Note: can't use separate on this because some subregions have "_" in their name.
      subRegion <- sub(paste(aLandLeaf$mProductName, "_", sep=""), "", aLandLeaf$mName)
      subRegion <- sub("_Irrigated", "", subRegion)
      subRegion <- sub("_Rainfed", "", subRegion)
      expectedPrice <- price_table$price[price_table$year == y & price_table$sector == aLandLeaf$mProductName[1]
                                         & price_table$subregion == subRegion]
    } else {
      expectedPrice <- price_table$price[price_table$year == y & price_table$sector == aLandLeaf$mProductName[1]]
    }
  } else {
    # TODO: Figure out what to do if price is missing.
    expectedPrice <- 1
  }

  # Save expected price data
  aLandLeaf$mExpectedPrice[aPeriod] <- expectedPrice

  return(expectedPrice)
}
