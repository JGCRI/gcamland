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

  # Get price table for the scenario/land type
  price_table <- PRICES[[aScenarioInfo$mScenarioType]]
  price_table <- subset(price_table, sector == aLandLeaf$mProductName[1])

  if(aLandLeaf$mProductName[1] %in% unique(price_table$sector)) {
    # Calculate expectations
    if ( aPeriod > 1 ) {
      prevYear <- get_per_to_yr(aPeriod - 1, aScenarioInfo$mScenarioType)

      # Get previous expectation
      previousExpectation <- aLandLeaf$mExpectedPrice[[aPeriod-1]]

      # Get new information (i.e., last years actual price)
      newInformation <- price_table$price[price_table$year == prevYear]
    } else {
      # Define current year, previous year, start year, and time step
      currYear <- get_per_to_yr(aPeriod, aScenarioInfo$mScenarioType)
      timeStep <- get_per_to_yr(aPeriod + 1, aScenarioInfo$mScenarioType) - currYear
      prevYear <- currYear - timeStep
      startYear <- min(price_table$year)

      # Get new information, if we have a previous year's data. If not, set to current year's information
      if(prevYear %in% price_table$year) {
        newInformation <- price_table$price[price_table$year == prevYear]
      } else {
        newInformation <- price_table$price[price_table$year == currYear]
      }

      # Calculate previousExpectation by looping from startYear to previous year
      # Note: prevYear isn't included (i.e., "<" not "<=") because it is counted as newInformation
      previousExpectation <- price_table$price[price_table$year == startYear]
      i <- startYear
      while(i < prevYear) {
        previousExpectation <- aScenarioInfo$mLaggedShareOld * previousExpectation +
          (1 - aScenarioInfo$mLaggedShareOld) * price_table$price[price_table$year == i]
        i <- i + timeStep
      }
    }

    # Calculate expected price
    expectedPrice <- aScenarioInfo$mLaggedShareOld * previousExpectation +
      (1 - aScenarioInfo$mLaggedShareOld) * newInformation
  } else {
    expectedPrice <- 1
  }

  # Save expected price data
  aLandLeaf$mExpectedPrice[aPeriod] <- expectedPrice

  return(expectedPrice)
}

#' Compute autoregressive model for price expectation
#'
#' This function computes price expectation as an autoregressive
#' function of the price time series.
#' \deqn{
#' y_i = \alpha y_{i-1} + (1-\alpha) x_i
#' }
#'
#' There is some dispute over what the final term in this series
#' should be.  In this implementation the final term is just like
#' all the others; therefore, for \eqn{\alpha = 0} this formula
#' reduces to \eqn{y_i = x_i}, the equivalent of perfect expectation.
#'
#' This function is not currently vectorized; a separate call is needed
#' for each value of \code{t}.
#'
#' @param t The current year, for which we wish to calculate the expected
#' price.
#' @param alpha Coefficient of previous year term in the autoregressive
#' series.
#' @param pricetable Table of price vs. year.  This series is assumed
#' not to have any gaps in it and to be in year order, but neither of
#' these conditions are checked.
#' @export
calc_lagged_expectation <- function(t, alpha, pricetable)
{
  year <- pricetable[['year']]
  x <- pricetable[['price']]
  startyear <- year[1]
  N <- length(year)
  endyear <- year[N]
  if(t <= startyear) {
    ## For times at or before the beginning of the time series, just return
    ## the initial value
    return(x[year==startyear])
  }
  else if(t > endyear) {
    ## For times after the end year, fill out the time series to the
    ## required length with its final value
    nfill <- t-endyear
    x <- c(x, rep(x[N], nfill))
  }
  else {
    x <- x[year <= t]
  }

  N <- length(x)    # Update N, since we have either extended or truncated x
  i <- seq(2,N)
  fac <- alpha^(N-i)

  ## return value
  alpha^(N-1) * x[1] + (1-alpha)*sum(fac*x[i])
}
