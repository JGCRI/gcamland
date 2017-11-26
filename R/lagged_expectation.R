# lagged_expectation.R

#' LaggedExpectation_calcExpectedYield
#'
#' @param aLandLeaf LandLeaf to calculate expected yield for
#' @param aPeriod Current model period
#' @details Calculate the expected yield for a LandLeaf using
#'          a lagged approach -- use previous expectation + fraction
#'          of new information.
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

  # Get length of time that has happened since last decision
  if ( aPeriod > 1 ) {
    time <- get_timestep(aPeriod)
  } else {
    time <- 1
  }

  # Calculate expected yield
  expectedYield <- previousExpectation + (time / aScenarioInfo$mLaggedYears) * newInformation

  # Save expected yield
  aLandLeaf$mExpectedYield[aPeriod] <- expectedYield

  return(expectedYield)
}

#' LaggedExpectation_calcExpectedPrice
#'
#' @param aLandLeaf LandLeaf to calculate expected price for
#' @param aPeriod Current model period
#' @details Calculate the expected price for a LandLeaf using
#'          a lagged approach -- use previous expectation + fraction
#'          of new information.
#' @author KVC November 2017
LaggedExpectation_calcExpectedPrice <- function(aLandLeaf, aPeriod, aScenarioInfo){
  # Silence package checks
  sector <- lm <- predict <- year <- price <- NULL

  if(aLandLeaf$mProductName[1] %in% unique(PRICES$sector)) {
    # Get previous expectation
    if ( aPeriod > 1 ) {
      yr <- get_per_to_yr(aPeriod - 1)
      EXPECTED_PRICES %>%
        filter(year == yr, sector == aLandLeaf$mProductName[1]) ->
        currExpectedPrice
      previousExpectation <- currExpectedPrice[[c("price")]]
    } else {
      # If we don't have data, then use current price
      yr <- get_per_to_yr(aPeriod)
      PRICES %>%
        filter(year == yr, sector == aLandLeaf$mProductName[1]) ->
        currPrice
      previousExpectation <- currPrice[[c("price")]]
    }

    # Get new information (the price that has happened since last decision)
    if ( aPeriod > 1 ) {
      yr <- get_per_to_yr(aPeriod - 1)
    } else {
      # If we don't have data, then use current price
      yr <- get_per_to_yr(aPeriod)
    }
    PRICES %>%
      filter(year == yr, sector == aLandLeaf$mProductName[1]) ->
      currPrice
    newInformation <- currPrice[[c("price")]]

    # Get length of time that has happened since last decision
    if ( aPeriod > 1 ) {
      time <- get_timestep(aPeriod)
    } else {
      time <- 1
    }

    # Calculate expected price
    expectedPrice <- previousExpectation + (time / aScenarioInfo$mLaggedYears) * newInformation
  } else {
    expectedPrice <- 1
  }

  # Save expected price data
  yr <- get_per_to_yr(aPeriod)
  EXPECTED_PRICES$price[year == yr & sector == aLandLeaf$mProductName[1]] <- expectedPrice

  return(expectedPrice)
}
