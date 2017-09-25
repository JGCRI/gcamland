# ag_production_technology.R

#' AgProductionTechnology_initCalc
#'
#' @details Initial calculation for the AgProductionTechnology.
#'          Adjusts costs & yields for technical change, calculate
#'          profit rate and make it available for the land allocator.
#'          Note: this method must be called before LandAllocator initCalc
#' @param aLandLeaf Land leaf
#' @param aPeriod Model time period.
#' @author KVC September 2017
AgProductionTechnology_initCalc <- function(aLandLeaf, aPeriod) {
  # Compute tech change values for this period for both ag productivity and
  # the nonLandVariableCost.

  # If no nonLandVariableCost is read in, get the previous period cost.
  # Note: you can never overwrite a positive cost with a zero cost. If the model sees a
  #       zero non-land cost, it will copy from the previous period.
  if ( aLandLeaf$mCost[[aPeriod]] == 0 && aPeriod > 1 ) {
    # Adjust last period's variable cost by tech change
    aLandLeaf$mCost[aPeriod] = aLandLeaf$mCost[[aPeriod - 1]] /
                                          ((1 + aLandLeaf$mNonLandCostTechChange[[aPeriod]]) ^ TIMESTEP)
  }

  # Note: C++ code says "Only do the ag productivity change calc if a calibration value is not read in that period"
  # TODO: Do we need to check for cal values?
  if( aLandLeaf$mCalOutput[[aPeriod]] != -1 ) {
    aLandLeaf$mYield[aPeriod] <- aLandLeaf$mCalOutput[[aPeriod]] / aLandLeaf$mLandAllocation[[aPeriod]]
  } else if( aPeriod > 1) {
    # Unless a yield is read in for this period, get the previous period yield from the market info.
    # Note: you can never overwrite a positive yield with a zero yield. If the model sees a
    #       zero yield, it will copy from the previous period.
    if( length(aLandLeaf$mYield < aPeriod ) ) {
      # Yield has not been set
      preYield <- aLandLeaf$mYield[[aPeriod-1]]

      # Adjust last period's variable cost by tech change
      aLandLeaf$mYield[aPeriod] <- preYield * ((1 + aLandLeaf$mAgProdChange[[aPeriod]]) ^ TIMESTEP)
    } else if ( aLandLeaf$mYield[[aPeriod]] == 0 ) {
      # Yield has been set to zero
      preYield <- aLandLeaf$mYield[[aPeriod-1]]

      # Adjust last period's variable cost by tech change
      aLandLeaf$mYield[aPeriod] <- preYield * ((1 + aLandLeaf$mAgProdChange[[aPeriod]]) ^ TIMESTEP)
    }
  }

  # Calculate profit rate
  AgProductionTechnology_calcProfitRate(aLandLeaf, aPeriod)
}

#' AgProductionTechnology_calcProfitRate
#'
#' @details Calculates the profit rate which is equal to the market price minus
#'          the variable cost. Profit rate can be negative.
#'          Profit rate is in 1975$ per billion m2, so computation includes yield.
#' @param aLandLeaf Land leaf
#' @param aPeriod Current model period
#' @importFrom readr read_csv
#' @author KVC September 2017
AgProductionTechnology_calcProfitRate <- function(aLandLeaf, aPeriod) {
  # Silence package checks
  Period <- Product <- NULL

  # Read in prices
  prices <- suppressMessages(read_csv("./inst/extdata/calibration-data/price.csv"))

  # Get price for this leaf in this period only
  prices %>%
    filter(Period == aPeriod, Product == aLandLeaf$mName) ->
    prices

  # Price in model is 1975$/kg. Land and ag costs are now assumed to be in 1975$.
  # We multiply by 1e9 since profitRate initially is in $/m2
  # and the land allocator needs it in $/billion m2. This assumes yield is in kg/m2.
  # TODO: Do I need to do something different for each?
  if(aPeriod <= FINAL_CALIBRATION_PERIOD) {
    aLandLeaf$mProfitRate[aPeriod] <- (prices[[c("mPrice")]] - aLandLeaf$mCost[[aPeriod]]) * aLandLeaf$mYield[[aPeriod]] * 1e9
  } else {
    aLandLeaf$mProfitRate[aPeriod] <- (prices[[c("mPrice")]] - aLandLeaf$mCost[[aPeriod]]) * aLandLeaf$mYield[[aPeriod]] * 1e9
  }
}

#' AgProductionTechnology_readData
#'
#' @details Read in technology related calibration data
#'          (e.g., CalOutput, technical change, cost, etc.)
#' @param aLandLeaf Land leaf
#' @author KVC September 2017
AgProductionTechnology_readData <- function(aLandLeaf) {
  # Read in data
  calOutput <- suppressMessages(read_csv("./inst/extdata/calibration-data/calOutput.csv"))

  # Get name of leaf
  name <- aLandLeaf$mName

  # Loop through all periods and read in data
  for ( per in PERIODS ) {
    # Only read in mCalOutput data for calibration periods
    if ( per <= FINAL_CALIBRATION_PERIOD ) {
      # Filter for this period/leaf combination
      calOutput %>%
        filter(Period == per, LandLeaf == name) ->
        currCalOutput

      # Set calOutput
      aLandLeaf$mCalOutput[[per]] <- currCalOutput[[c("mCalOutput")]]
    } else{
      # Only read in technical change information for future periods

      # Set data that shouldn't exist in the future to -1
      aLandLeaf$mCalOutput[[per]] <- -1
    }
  }

}
