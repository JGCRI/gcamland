# linear_expectation.R

#' LinearExpectation_calcExpectedYield
#'
#' @param aLandLeaf LandLeaf to calculate expected yield for
#' @param aPeriod Current model period
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @details Calculate the expected yield for a LandLeaf using
#'          a linear extrapolation from recent history.
#' @importFrom stats lm predict
#' @author KVC October 2017
LinearExpectation_calcExpectedYield <- function(aLandLeaf, aPeriod, aScenarioInfo) {
  # Silence package checks
  sector <- year <- yield <- lm <- predict <- NULL

  scentype <- aScenarioInfo$mScenarioType

  currYear <- get_per_to_yr(aPeriod, aScenarioInfo$mScenarioType)
  startYear <- currYear - aScenarioInfo$mLinearYears

  # Create a tibble with yields and years
  data.frame(yield = rep(-1, aScenarioInfo$mLinearYears),
         year = seq(from=startYear, to=(currYear - 1), by=1)) ->
    all.yields

  # Update yield tibble to include actual yields
  i <- startYear
  while(i < currYear){
    if(i < getStartYear(scentype)) {
      # We won't have data prior to the start year, so we'll want
      # to just use its data as many times as needed
      # TODO: do we want to read in data prior to startYear so this works?
      per <- 1
    } else {
      per <- get_yr_to_per(i, aScenarioInfo$mScenarioType)
    }

    all.yields$yield[all.yields$year == i] <- aLandLeaf$mYield[[per]]

    i <- i + 1
  }

  # Linearly extrapolate yield to get an expected yield for the current year
  model.lm <- lm(yield ~ year, data = all.yields)
  expectedYield <- predict(model.lm, newdata = data.frame(year = currYear))

  # Save expected yield
  aLandLeaf$mExpectedYield[aPeriod] <- expectedYield

  return(expectedYield)
}

#' LinearExpectation_calcExpectedPrice
#'
#' @param aLandLeaf LandLeaf to calculate expected price for
#' @param aPeriod Current model period
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @details Calculate the expected price for a LandLeaf using
#'          a linear extrapolation from recent history.
#' @importFrom stats lm predict
#' @author KVC October 2017
LinearExpectation_calcExpectedPrice <- function(aLandLeaf, aPeriod, aScenarioInfo){
  # Silence package checks
  sector <- lm <- predict <- year <- price <- NULL

  currYear <- get_per_to_yr(aPeriod, aScenarioInfo$mScenarioType)
  startYear <- currYear - aScenarioInfo$mLinearYears

  # Create a tibble with yields and years
  data.frame(price = rep(-1, aScenarioInfo$mLinearYears),
         year = seq(from=startYear, to=(currYear - 1), by=1)) ->
    all.prices

  # Get prices for this land leaf/scenario type
  price_table <- PRICES[[aScenarioInfo$mScenarioType]]
  price_table <- subset(price_table, sector == aLandLeaf$mProductName[1])

  # Update price tibble to include actual prices
  i <- startYear
  while(i < currYear){
    if(i %in% price_table$year) {
      yr <- i
    } else if(i < getStartYear(aScenarioInfo$mScenarioType)) {
      # If we don't have data and it is prior to the start year,
      # then use information from the first period
      yr <- min(price_table$year)
    } else {
      # Get closest period
      per <- get_yr_to_per(i, aScenarioInfo$mScenarioType)
      yr <- get_per_to_yr(per, aScenarioInfo$mScenarioType)
    }

    if(aLandLeaf$mProductName[1] %in% price_table$sector) {
      all.prices$price[all.prices$year == i] <- price_table$price[price_table$year == yr]
    } else {
      # TODO: Figure out what to do if price is missing.
      all.prices$price[all.prices$year == i] <- 1
    }

    i <- i + 1
  }

  # Linearly extrapolate price to get an expected price for the current year
  model.lm <- lm(price ~ year, data = all.prices)
  expectedPrice <- predict(model.lm, newdata = data.frame(year = currYear))

  # Save expected price data
  aLandLeaf$mExpectedPrice[aPeriod] <- expectedPrice

  return(expectedPrice)
}
