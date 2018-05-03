# lagged_expectation.R

#' LaggedExpectation_calcExpectedYield
#'
#' @details Calculate the expected yield for a LandLeaf using
#'          a lagged approach -- use linear combination of previous expectation and
#'          new information.
#' @param aLandLeaf LandLeaf to calculate expected yield for
#' @param aPeriod Current model period
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @author KVC November 2017
LaggedExpectation_calcExpectedYield <- function(aLandLeaf, aPeriod, aScenarioInfo) {
  # Silence package checks
  sector <- year <- yield <- lm <- predict <- NULL

  # Get previous expectation
  if ( aPeriod > 1 ) {
    previousExpectation <- aLandLeaf$mExpectedYield[[aPeriod - 1]]
  } else {
    # If we don't have data, then use current yield
    previousExpectation <- aLandLeaf$mYield[[aPeriod]]
  }

  # Get new information (the yield that has happened since last decision)
  if ( aPeriod > 1 ) {
    newInformation <- aLandLeaf$mYield[[aPeriod - 1]]
  } else {
    newInformation <- aLandLeaf$mYield[[aPeriod]]
  }

  # Calculate expected yield
  expectedYield <- aScenarioInfo$mLaggedShareOld * previousExpectation +
      (1 - aScenarioInfo$mLaggedShareOld) * newInformation

  # Save expected yield
  aLandLeaf$mExpectedYield[aPeriod] <- expectedYield

  return(expectedYield)
}

#' LaggedExpectation_calcExpectedPrice
#'
#' @details Calculate the expected price for a LandLeaf using
#'          a lagged approach -- use linear combination of previous expectation and
#'          new information.
#' @param aLandLeaf LandLeaf to calculate expected price for
#' @param aPeriod Current model period
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @author KVC November 2017
LaggedExpectation_calcExpectedPrice <- function(aLandLeaf, aPeriod, aScenarioInfo){
  # Silence package checks
  sector <- lm <- predict <- year <- price <- NULL

  price_table <- PRICES[[aScenarioInfo$mScenarioType]]

  if(aLandLeaf$mProductName[1] %in% unique(price_table$sector)) {
    # Calculate expectations
    if ( aPeriod > 1 ) {
      prevYear <- get_per_to_yr(aPeriod - 1, aScenarioInfo$mScenarioType)

      # Get previous expectation
      previousExpectation <- aLandLeaf$mExpectedPrice[[aPeriod-1]]

      # Get new information (i.e., last years actual price)
      price_table %>%
        filter(year == prevYear, sector == aLandLeaf$mProductName[1]) ->
        currPrice
      newInformation <- currPrice[[c("price")]]

      # Calculate expected price
      expectedPrice <- aScenarioInfo$mLaggedShareOld * previousExpectation +
                              (1 - aScenarioInfo$mLaggedShareOld) * newInformation
    } else {
      # If we don't have data, then use current price as expectation
      currYear <- get_per_to_yr(aPeriod, aScenarioInfo$mScenarioType)
      price_table %>%
        filter(year == currYear, sector == aLandLeaf$mProductName[1]) ->
        currPrice
      expectedPrice <- currPrice[[c("price")]]
    }
  } else {
    expectedPrice <- 1
  }

  # Save expected price data
  aLandLeaf$mExpectedPrice[aPeriod] <- expectedPrice

  return(expectedPrice)
}
