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
  sector <- year <- yield <- lm <- predict <- GCAM_commodity <- NULL

  # Get number of years -- Note that numYears can differ based on crop group. Determine which crop this leaf belongs in
  if(aLandLeaf$mProductName %in% CROP_GROUP1 ) {
    numYears <- aScenarioInfo$mLinearYears1
  } else if(aLandLeaf$mProductName %in% CROP_GROUP2 ) {
    numYears <- aScenarioInfo$mLinearYears2
  } else if(aLandLeaf$mProductName %in% CROP_GROUP3 ) {
    numYears <- aScenarioInfo$mLinearYears3
  } else if(aLandLeaf$mProductName %in% CROP_GROUP4 ) {
    numYears <- aScenarioInfo$mLinearYears4
  } else if(aLandLeaf$mProductName %in% CROP_GROUP5 ) {
    numYears <- aScenarioInfo$mLinearYears5
  } else {
    message("Error: crop grouping not specified.")
  }

  # subset the YIELD.RATIOS to only work with the region of interest
  rgn.YIELD.RATIOS <- subset(YIELD.RATIOS, region == aScenarioInfo$mRegion[1])

  # Get scenario type
  scentype <- aScenarioInfo$mScenarioType

  # Get start year
  currYear <- get_per_to_yr(aPeriod, aScenarioInfo$mScenarioType)
  startYear <- currYear - numYears

  # Create a tibble with yields and years
  data.frame(yield = rep(-1, numYears),
         year = seq(from=startYear, to=(currYear - 1), by=1)) ->
    all.yields

  # Update yield tibble to include actual yields
  i <- startYear
  while(i < currYear){
    if(i < getStartYear(scentype)) {
      if(aLandLeaf$mProductName[1] %in% rgn.YIELD.RATIOS$GCAM_commodity) {
        if(i %in% rgn.YIELD.RATIOS$year) {
          temp <- subset(rgn.YIELD.RATIOS, year == i & GCAM_commodity == aLandLeaf$mProductName[1])
          ratio <- temp$yieldRatio
        } else {
          temp <- subset(rgn.YIELD.RATIOS, year == min(rgn.YIELD.RATIOS$year) & GCAM_commodity == aLandLeaf$mProductName[1])
          ratio <- temp$yieldRatio
        }
      } else {
        ratio <- 1
      }
      all.yields$yield[all.yields$year == i] <- aLandLeaf$mYield[[1]] * ratio
    } else {
      per <- get_yr_to_per(i, aScenarioInfo$mScenarioType)
      all.yields$yield[all.yields$year == i] <- aLandLeaf$mYield[[per]]
    }

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

  # Get number of years -- Note that numYears can differ based on crop group. Determine which crop this leaf belongs in
  if(aLandLeaf$mProductName %in% CROP_GROUP1 ) {
    numYears <- aScenarioInfo$mLinearYears1
  } else if(aLandLeaf$mProductName %in% CROP_GROUP2 ) {
    numYears <- aScenarioInfo$mLinearYears2
  } else if(aLandLeaf$mProductName %in% CROP_GROUP3 ) {
    numYears <- aScenarioInfo$mLinearYears3
  } else if(aLandLeaf$mProductName %in% CROP_GROUP4 ) {
    numYears <- aScenarioInfo$mLinearYears4
  } else if(aLandLeaf$mProductName %in% CROP_GROUP5 ) {
    numYears <- aScenarioInfo$mLinearYears5
  } else {
    message("Error: crop grouping not specified.")
  }

  # Get start year
  currYear <- get_per_to_yr(aPeriod, aScenarioInfo$mScenarioType)
  startYear <- currYear - numYears

  # Create a tibble with yields and years
  data.frame(price = rep(-1, numYears),
         year = seq(from=startYear, to=(currYear - 1), by=1)) ->
    all.prices

  # Get prices for this land leaf/scenario type
  price_table <- PRICES[[aScenarioInfo$mScenarioType]]
  price_table <- subset(price_table, sector == aLandLeaf$mProductName[1])
  price_table <- subset(price_table, region == aScenarioInfo$mRegion[1])

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
