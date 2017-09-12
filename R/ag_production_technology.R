# ag_production_technology.R

#' AgProductionTechnology_initCalc
#'
#' @details Initial calculation for the AgProductionTechnology.
#'          Adjusts costs & yields for technical change, calculate
#'          profit rate and make it available for the land allocator.
#'          Note: this method must be called before LandAllocator initCalc
#' @param aRegionName Region name.
#' @param aPeriod Model time period.
#' @author KVC September 2017
AgProductionTechnology_initCalc <- function(aRegionName, aPeriod) {
  if(DEBUG){
    print(paste("AgProductionTechnology_initCalc for ", aRegionName, aPeriod))
  }
#   // Compute tech change values for this period for both ag productivity and
#   // the nonLandVariableCost.  Since technologies are distinct by vintage and don't
#   // previous period technologies, need to save a previous period compounded cumulative
#   // change in the MarketInfo
#
#   // Create a unique regional string for the yield and for the
#   // variable cost.
#
#   const string preVarCostName = "preVarCost-" + mName + "-" + aRegionName;
#   double preVarCost = 0.0;
#
#   const string preYieldName = "preYield-" + mName + "-" + aRegionName;
#   double preYield = 0.0;
#
#
#   // Get the information object for this market.
#   Marketplace* marketplace = scenario->getMarketplace();
#   IInfo* marketInfo = marketplace->getMarketInfo( aSectorName, aRegionName, aPeriod, true );
#   assert( marketInfo );
#
#   // If no nonLandVariableCost is read in, get the previous period cost from the market info.
#   // Note: you can never overwrite a positive yield with a zero yield. If the model sees a
#   // zero non-land cost, it will copy from the previous period.
#   if ( mNonLandVariableCost == 0 && aPeriod != 0 ) {
#   preVarCost = marketInfo->getDouble( preVarCostName, true );
#   // Adjust last period's variable cost by tech change
#   int timestep = modeltime->gettimestep( aPeriod );
#   mNonLandVariableCost = preVarCost / pow(1 + mNonLandCostTechChange , timestep);
#   }
#
# // Only do the ag productivity change calc if a calibration value is not read in that period
# if( !mCalValue.get() ){
#   // Unless a yield is read in for this period, get the previous period yield from the market info.
#   // Note: you can never overwrite a positive yield with a zero yield. If the model sees a
#   // zero yield, it will copy from the previous period.
#   if ( mYield == 0 && aPeriod != 0 ) {
#     preYield = marketInfo->getDouble( preYieldName, true );
#     // Adjust last period's variable cost by tech change
#     int timestep = modeltime->gettimestep( aPeriod );
#     mYield = preYield * pow(1 + mAgProdChange , timestep);
#   }
# }

  # Calculate profit rate
  AgProductionTechnology_calcProfitRate(aRegionName, aPeriod)
}

#' AgProductionTechnology_calcProfitRate
#'
#' @details Calculates the profit rate which is equal to the market price minus
#'          the variable cost. Profit rate can be negative.
#'          Profit rate is in 1975$ per billion m2, so computation includes yield.
#' @param aRegionName Region name.
#' @author KVC September 2017
AgProductionTechnology_calcProfitRate <- function(aRegionName, aPeriod) {
  if(DEBUG){
    print(paste("AgProductionTechnology_calcProfitRate for ", aRegionName, aPeriod))
  }

  # TODO: Fix this to use read-in data and figure out future periods
  # Price in model is 1975$/kg. Land and ag costs are now assumed to be in 1975$.
  # We multiply by 1e9 since profitRate initially is in $/m2
  # and the land allocator needs it in $/billion m2. This assumes yield is in kg/m2.
  if(aPeriod <= FINAL_CALIBRATION_PERIOD) {
    LANDLEAF_CALDATA %>%
      mutate(profit = (price - cost) * yield * 1e9) %>%
      select(name, profit) ->
      LANDLEAF_PROFIT
  } else {
    # TODO: Fix this to use future data
    LANDLEAF_CALDATA %>%
      mutate(profit = (price - cost) * yield * 1e9) %>%
      select(name, profit) ->
      LANDLEAF_PROFIT
  }


  if(DEBUG) {
    print(LANDLEAF_PROFIT)
  }

  # TODO: Change how we store data
  write_csv(LANDLEAF_PROFIT, "./inst/extdata/temp-data/LANDLEAF_PROFIT.csv")
}
