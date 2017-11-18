# linear_expectation.R

#' LinearExpectation_calcExpectedYield
#'
#' @param aLandLeaf LandLeaf to calculate expected yield for
#' @param aPeriod Current model period
#' @details Calculate the expected yield for a LandLeaf using
#'          a linear extrapolation from recent history.
#' @importFrom stats lm predict
#' @author KVC October 2017
LinearExpectation_calcExpectedYield <- function(aLandLeaf, aPeriod) {
  # Silence package checks
  sector <- year <- yield <- lm <- predict <- NULL

  currYear <- get_per_to_yr(aPeriod)
  startYear <- currYear - LINEAR.YEARS

  # Create a tibble with yields and years
  tibble(yield = rep(-1, LINEAR.YEARS),
         year = seq(from=startYear, to=(currYear - 1), by=1)) ->
    all.yields

  # Update yield tibble to include actual yields
  i <- startYear
  while(i < currYear){
    if(i < getStartYear()) {
      # We won't have data prior to the start year, so we'll want
      # to just use its data as many times as needed
      # TODO: do we want to read in data prior to startYear so this works?
      per <- 1
    } else {
      per <- get_yr_to_per(i)
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
#' @details Calculate the expected price for a LandLeaf using
#'          a linear extrapolation from recent history.
#' @importFrom stats lm predict
#' @author KVC October 2017
LinearExpectation_calcExpectedPrice <- function(aLandLeaf, aPeriod){
  # Silence package checks
  sector <- lm <- predict <- year <- price <- NULL

  currYear <- get_per_to_yr(aPeriod)
  startYear <- currYear - LINEAR.YEARS

  # Create a tibble with yields and years
  tibble(price = rep(-1, LINEAR.YEARS),
         year = seq(from=startYear, to=(currYear - 1), by=1)) ->
    all.prices

  # Update yield tibble to include actual yields
  i <- startYear
  while(i < currYear){
    if(i < getStartYear()) {
      # We won't have data prior to the start year, so we'll want
      # to just use its data as many times as needed
      # TODO: do we want to read in data prior to startYear so this works?
      per <- 1
    } else {
      per <- get_yr_to_per(i)
    }

    yr <- get_per_to_yr(per)
    if(aLandLeaf$mProductName[1] %in% unique(PRICES$sector)) {
      PRICES %>%
        filter(year == yr, sector == aLandLeaf$mProductName[1]) ->
        curr.price

      all.prices$price[all.prices$year == i] <- curr.price[[c("price")]]
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
  EXPECTED_PRICES$price[year == y & sector == aLandLeaf$mProductName[1]] <- expectedPrice

  return(expectedPrice)
}
