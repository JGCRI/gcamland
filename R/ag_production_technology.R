# ag_production_technology.R

#' AgProductionTechnology_initCalc
#'
#' @details Initial calculation for the AgProductionTechnology.
#'          Adjusts costs & yields for technical change, calculate
#'          profit rate and make it available for the land allocator.
#'          Note: this method must be called before LandAllocator initCalc
#' @param aLandLeaf Land leaf
#' @param aPeriod Model time period.
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @author KVC September 2017
AgProductionTechnology_initCalc <- function(aLandLeaf, aPeriod, aScenarioInfo) {
  # First, set the nonLandVariableCost for this technology in this period.
  # If no nonLandVariableCost is read in, get the previous period cost.
  # Note: you can never overwrite a positive cost with a zero cost. If the model sees a
  #       zero non-land cost, it will copy from the previous period.
  if(aLandLeaf$mCost[[aPeriod]] == 0 && aPeriod > 1) {
    # Get the length of the current time step
    timestep <- get_timestep(aPeriod, aScenarioInfo$mScenarioType)

    # Adjust last period's variable cost by tech change
    aLandLeaf$mCost[aPeriod] = aLandLeaf$mCost[[aPeriod - 1]] /
                                          ((1 + aLandLeaf$mNonLandCostTechChange[[aPeriod]]) ^ timestep)
  }

  # Next, set the yield for this technology in this period.
  # If calibration values are read in, then yield = mCalOutput / mCalLandAllocation.
  # If mCalOutput is read in, but not land area, yield is set to zero.
  # If calibration values are missing, yield is set to the previous value, then adjusted by AgProdChange.
  if(aLandLeaf$mCalOutput[[aPeriod]] != -1) {
    # Calibration values exist, so calculate yield
    if(aLandLeaf$mCalLandAllocation[[aPeriod]] > 0) {
      aLandLeaf$mYield[aPeriod] <- aLandLeaf$mCalOutput[[aPeriod]] / aLandLeaf$mCalLandAllocation[[aPeriod]]
    } else {
      aLandLeaf$mYield[aPeriod] <- 0
    }
  } else if(aPeriod > 1) {
    # Calibration values do not exist, and it is after the first model period

    # Get the length of the current time step
    timestep <- get_timestep(aPeriod, aScenarioInfo$mScenarioType)

    # Unless a yield is read in for this period, get the previous period yield from the market info.
    # Note: you can never overwrite a positive yield with a zero yield. If the model sees a
    #       zero yield, it will copy from the previous period.
    if(length(aLandLeaf$mYield) < aPeriod) {
      # Yield has not been set
      preYield <- aLandLeaf$mYield[[aPeriod-1]]

      # Adjust last period's variable cost by tech change
      aLandLeaf$mYield[aPeriod] <- preYield * ((1 + aLandLeaf$mAgProdChange[[aPeriod]]) ^ timestep)
    } else if (aLandLeaf$mYield[[aPeriod]] == 0) {
      # Yield has been set to zero
      preYield <- aLandLeaf$mYield[[aPeriod-1]]

      # Adjust last period's yield by tech change
      aLandLeaf$mYield[aPeriod] <- preYield * ((1 + aLandLeaf$mAgProdChange[[aPeriod]]) ^ timestep)
    }
  } else if (length(aLandLeaf$mYield) > 0) {
    # Do nothing. Yield was read in for the calibration period. This will happen for bioenergy
  }
  else {
    # Calibration values don't exist and it is the first model period, so we need to set a yield.
    # Note: this isn't in the C++ code, but I seem to need something here
    aLandLeaf$mYield[aPeriod] <- 0.0
  }

  # Calculate profit rate
  AgProductionTechnology_calcProfitRate(aLandLeaf, aPeriod, aScenarioInfo)
}

#' AgProductionTechnology_calcProfitRate
#'
#' @details Calculates the profit rate which is equal to the market price minus
#'          the variable cost. Profit rate can be negative.
#'          Profit rate is in 1975$ per billion m2, so computation includes yield.
#' @param aLandLeaf Land leaf
#' @param aPeriod Current model period
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @author KVC September 2017
AgProductionTechnology_calcProfitRate <- function(aLandLeaf, aPeriod, aScenarioInfo) {
  # Silence package checks
  Period <- Product <- NULL

  # Get expected price and yield. These will be used to calculate profit.
  if(aScenarioInfo$mExpectationType == "Perfect") {
    expectedPrice <- PerfectExpectation_calcExpectedPrice(aLandLeaf, aPeriod, aScenarioInfo)
    expectedYield <- PerfectExpectation_calcExpectedYield(aLandLeaf, aPeriod, aScenarioInfo)
  } else if(aScenarioInfo$mExpectationType == "Linear") {
    expectedPrice <- LinearExpectation_calcExpectedPrice(aLandLeaf, aPeriod, aScenarioInfo)
    expectedYield <- LinearExpectation_calcExpectedYield(aLandLeaf, aPeriod, aScenarioInfo)
  } else if(aScenarioInfo$mExpectationType == "Adaptive" | aScenarioInfo$mExpectationType == "HybridPerfectAdaptive") {
    expectedPrice <- AdaptiveExpectation_calcExpectedPrice(aLandLeaf, aPeriod, aScenarioInfo)
    expectedYield <- AdaptiveExpectation_calcExpectedYield(aLandLeaf, aPeriod, aScenarioInfo)
  } else if(aScenarioInfo$mExpectationType == "HybridLinearAdaptive") {
    expectedPrice <- AdaptiveExpectation_calcExpectedPrice(aLandLeaf, aPeriod, aScenarioInfo)
    expectedYield <- LinearExpectation_calcExpectedYield(aLandLeaf, aPeriod, aScenarioInfo)
  }

  # Calculate expected profit.
  # Price in model is 1975$/kg. Land and ag costs are now assumed to be in 1975$.
  # We multiply by 1e9 since profitRate initially is in $/m2
  # and the land allocator needs it in $/billion m2. This assumes yield is in kg/m2.
  aLandLeaf$mProfitRate[aPeriod] <- (expectedPrice - aLandLeaf$mCost[[aPeriod]]) * expectedYield * 1e9

  if ( aScenarioInfo$mIncludeSubsidies ) {
    # Subsidies are assumed to br in $/billion m2
    aLandLeaf$mProfitRate[aPeriod] <- aLandLeaf$mProfitRate[[aPeriod]] + aLandLeaf$mSubsidy[[aPeriod]]
  }
}
