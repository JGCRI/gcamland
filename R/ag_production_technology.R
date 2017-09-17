# ag_production_technology.R

#' AgProductionTechnology_initCalc
#'
#' @details Initial calculation for the AgProductionTechnology.
#'          Adjusts costs & yields for technical change, calculate
#'          profit rate and make it available for the land allocator.
#'          Note: this method must be called before LandAllocator initCalc
#' @param aLandAllocator Land allocator
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
                                          ((1 + aLandLeaf$mNonLandCostTechChange[[aPeriod]]) ^ TIMESTEP);
  }

  # Note: C++ code says "Only do the ag productivity change calc if a calibration value is not read in that period"
  # TODO: Do we need to check for cal values?


  # Unless a yield is read in for this period, get the previous period yield from the market info.
  # Note: you can never overwrite a positive yield with a zero yield. If the model sees a
  #       zero yield, it will copy from the previous period.
  if ( aLandLeaf$mYield[[aPeriod]] == 0 && aPeriod > 1 ) {
    preYield = aLandLeaf$mYield[[aPeriod-1]]

    # Adjust last period's variable cost by tech change
    aLandLeaf$mYield[aPeriod] = preYield * ((1 + aLandLeaf$mAgProdChange[[aPeriod]]) ^ TIMESTEP)
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
#' @author KVC September 2017
AgProductionTechnology_calcProfitRate <- function(aLandLeaf, aPeriod) {
  # TODO: Fix this to use read-in data and figure out future periods
  # Price in model is 1975$/kg. Land and ag costs are now assumed to be in 1975$.
  # We multiply by 1e9 since profitRate initially is in $/m2
  # and the land allocator needs it in $/billion m2. This assumes yield is in kg/m2.
  if(aPeriod <= FINAL_CALIBRATION_PERIOD) {
    # Figure out how to read in price, cost, yield
    price <- 1
    aLandLeaf$mProfitRate[aPeriod] <- (price - aLandLeaf$mCost[[aPeriod]]) * aLandLeaf$mYield[[aPeriod]] * 1e9
  } else {
    # TODO: Fix this to use future data
    # TODO: allow for more than one node
    # Figure out how to read in/differentiate price, cost, yield
    price <- 1
    aLandLeaf$mProfitRate[aPeriod] <- (price - aLandLeaf$mCost[[aPeriod]]) * aLandLeaf$mYield[[aPeriod]] * 1e9
  }
}
