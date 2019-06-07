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
  sector <- year <- yield <- lm <- predict <- GCAM_commodity <- NULL

  # Calculate expectations. For model periods > 1, we calculate this iteratively to save time.
  if ( aPeriod > 1 ) {
    # Get previous expectation
    previousExpectation <- aLandLeaf$mExpectedYield[[aPeriod - 1]]

    # Get new information
    if( aScenarioInfo$mLaggedIncludeCurr ) {
      newInformation <- aLandLeaf$mYield[[aPeriod]]
    } else {
      newInformation <- aLandLeaf$mYield[[aPeriod - 1]]
    }

    # Calculate expected yield
    expectedYield <- aScenarioInfo$mLaggedShareOld * previousExpectation +
      (1 - aScenarioInfo$mLaggedShareOld) * newInformation
  } else {
    # If we don't have saved previousExpectations, we need to generate them
    # using calc_lagged_expectation(). First, we need to set up a yield table
    years <- seq(from=min(YIELD.RATIOS$year), to=get_per_to_yr(aPeriod, aScenType=aScenarioInfo$mScenarioType), by=1)
    yield_table <- data.frame(year = years,
                              base_yield = rep_len(aLandLeaf$mYield[[aPeriod]], length(years)),
                              yield_ratio = rep_len(1, length(years)))
    if(aLandLeaf$mProductName[1] %in% YIELD.RATIOS$GCAM_commodity) {
      for(i in years) {
        if(i %in% YIELD.RATIOS$year) {
          temp <- subset(YIELD.RATIOS, year == i & GCAM_commodity == aLandLeaf$mProductName[1])
          yield_table$yield_ratio[yield_table$year == i] <- temp$yieldRatio
        } else {
          temp <- subset(YIELD.RATIOS, year == min(YIELD.RATIOS$year) & GCAM_commodity == aLandLeaf$mProductName[1])
          yield_table$yield_ratio[yield_table$year == i] <- temp$yieldRatio
        }
      }
    }
    yield_table$yield <- yield_table$base_yield * yield_table$yield_ratio

    # Now, we call calc_lagged_expectation() to calculate the expectations
    if( aScenarioInfo$mLaggedIncludeCurr ) {
      currYear <- get_per_to_yr(aPeriod, aScenarioInfo$mScenarioType)
      expectedYield <- calc_lagged_expectation(currYear, aScenarioInfo$mLaggedShareOld, yield_table, 'yield')
    } else {
      prevYear <- get_per_to_yr(aPeriod, aScenarioInfo$mScenarioType) - 1
      expectedYield <- calc_lagged_expectation(prevYear, aScenarioInfo$mLaggedShareOld, yield_table, 'yield')
    }
  }

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
  price_table <- dplyr::filter(price_table, sector == aLandLeaf$mProductName[1])

  if(aLandLeaf$mProductName[1] %in% unique(price_table$sector)) {
    # Calculate expected price
    if( aScenarioInfo$mLaggedIncludeCurr ) {
      currYear <- get_per_to_yr(aPeriod, aScenarioInfo$mScenarioType)
      expectedPrice <- calc_lagged_expectation(currYear, aScenarioInfo$mLaggedShareOld, price_table, 'price')
    } else {
      if( aPeriod > 1 ) {
        prevYear <- get_per_to_yr(aPeriod-1, aScenarioInfo$mScenarioType)
      } else {
        prevYear <- get_per_to_yr(aPeriod, aScenarioInfo$mScenarioType) - 1
      }
      expectedPrice <- calc_lagged_expectation(prevYear, aScenarioInfo$mLaggedShareOld, price_table, 'price')
    }

  } else {
    expectedPrice <- 1
  }

  # Save expected price data
  aLandLeaf$mExpectedPrice[aPeriod] <- expectedPrice

  return(expectedPrice)
}

#' Compute autoregressive model for expectation
#'
#' This function computes expectation (generally for price or yield) as an autoregressive
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
#' @param datatbl Table of price vs. year.  This series is assumed
#' not to have any gaps in it and to be in year order, but neither of
#' these conditions are checked.
#' @param colname Name of the column that has the data for which we are
#' computing the expectation (e.g. \code{'price'})
#' @export
calc_lagged_expectation <- function(t, alpha, datatbl, colname)
{
  year <- datatbl[['year']]
  x <- datatbl[[colname]]
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
