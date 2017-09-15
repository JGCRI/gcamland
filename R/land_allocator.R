# land_allocator.R

#' LandAllocator
#'
#' @details Initialize an Class called LandAllocator
#' @param aRegionName Region name
#' @param aLogitExponent Logit exponent of the top level of the land allocator
#' @param aLandAllocation Land allocation for this region
#' @field mRegionName Region name
#' @field mLogitExponent Logit exponent for the top level of the nest
#' @field mLandAllocation Land allocation for this region
#' @field mShare Share of land
#' @field mChild Children of the LandAllocator (currently one LandNode only)
#'
#' @return New, initialized LandAllocator
#' @author KVC September 2017
LandAllocator <- function(aRegionName, aLogitExponent, aLandAllocation) {
  mRegionName = aRegionName
  mLogitExponent = aLogitExponent # TODO: do I need this?
  mLandAllocation = aLandAllocation
  mShare = NULL
  mChild = LandNode("Crop", LOGIT_EXPONENT, LAND_ALLOCATION)
  greet = function() {
    cat(paste0("Hello, my name is ", self$mRegionName, ".\n"))
  }

  self <- environment()
  class(self) <- "LandAllocator"
  self
}

#' LandAllocator_initCalc
#'
#' @param aLandAllocator The land allocator for this region
#' @param aPeriod Current time period
#' @details Initial calculations needed for the land allocator.
#' @author KVC September 2017
LandAllocator_initCalc <- function(aLandAllocator, aPeriod) {
  if(aPeriod <= FINAL_CALIBRATION_PERIOD){
    LandAllocator_calibrateLandAllocator(aLandAllocator, aPeriod)
  }

  # Call land node's initCalc
  # TODO: loop over children
  LandNode_initCalc(aLandAllocator$mChild, aPeriod)
}

#' LandAllocator_calibrateLandAllocator
#'
#' @param aLandAllocator Land allocator for this region
#' @param aPeriod model period.
#' @details Sets initial land shares, sets unmanaged land profit rates,
#'          calculate calibration prfot rates, then calculates share profit scalers
#'          that will be used for scaling the profit rates for future year sharing
#' @author KVC September 2017
LandAllocator_calibrateLandAllocator <- function(aLandAllocator, aPeriod){
  # /*  Step 1. Calculate and set initial land shares based on read in data for a
  # calibration period. */
  LandAllocator_setInitShares(aLandAllocator, aPeriod)

  # /* Step 2. Set the profit rate of unmanaged land leafs equal to the read in land price
  # (which is also the marginal profit rate) for that region or land node subregion. This
  # is the only way the unmanaged land will have a profit rate at this point. It is implied
  # by the read in price of land.  */
  # TODO: Implement this so it uses read-in data and can differentiate by region
  # LandAllocator_setUnmanagedLandProfitRate(aRegionName, UNMANAGED_LAND_VALUE, aPeriod)

  # /* For these steps to work, the profit rates of managed land leaves will have been computed before
  # this method call (i.e., calibrateLandAllocator) in the initCalc() of the agTechnology Class
  # and set in the leafs based on read in calibration prices, yields, and non-land variable
  # costs. These might also be called "observed profits" since they are based on this simple
  # calculation. Also, the node profits do not need to be calculated here as they are not a
  # part of the calibration.  All the info needed is in the leaves. */

  #
  # /* Step 3. Calculate the profit rate implied by the shares in the calibration data. These rates
  # are what the profit rates would have to be based on the actual shares, the logit exponent, and
  # the average profit of the containing node. These are equivalent to what was called "intrinsic
  # rates" in the 2008 version of the code based on Sands and Leimbech. */
  LandNode_calculateNodeProfitRates(aLandAllocator$mChild, UNMANAGED_LAND_VALUE, "relative-cost", aPeriod)

  # /* Step 4. Calculate profit scalers. Because the calibration profit rate computed in Step 4
  # will most likely differ from the profit rate computed using the yield times price - cost, a
  # scaler or multiplier is solved for which makes the profit equal to the calibration profit. In
  # future periods, this scaler is then applied to the computed profit for use in the sharing
  # and node profit equations.  It is analagous to the share weight calibration approach. Also, it
  # will work the same for unmanaged land leafs with the land price being used as the profit.
  #
  # All of the calibration is captured in the leaves, so the share profit scalers for nodes are
  # set equal to 1.  */
  LandNode_calculateShareWeights(aLandAllocator$mChild, "relative-cost", aPeriod)
}

#' LandAllocator_setInitShares
#'
#' @param aLandAllocator Land allocator for this region.
#' @param aPeriod model period.
#' @details Calculates and sets initial land shares
#' @author KVC September 2017
LandAllocator_setInitShares <- function(aLandAllocator, aPeriod) {
  # Call setInitShares for nodes
  # TODO: set up loop over all land nodes
  LandNode_setInitShares(aLandAllocator$mChild, LAND_ALLOCATION, aPeriod)
}

#' LandAllocator_calcLandShares
#'
#' @details Calculates land shares.
#'          Sets unmanaged profit rate, then calls land node's
#'          calcLandShares. Also, sets root share to 1.
#' @param aLandAllocator Land allocator for this region
#' @param aChoiceFnAbove Type of logit choice function for parent node.
#' @param aPeriod Model time period.
#' @author KVC September 2017
LandAllocator_calcLandShares <- function(aLandAllocator, aChoiceFnAbove, aPeriod) {
  # First, set value of unmanaged land leaves
  # TODO: fix this so different regions can have different values
  # setUnmanagedLandProfitRate( aRegionName, mUnManagedLandValue, aPeriod );

  # Then, calculate land shares
  LandNode_calcLandShares(aLandAllocator$mChild, "relative-cost", aPeriod)

  # This is the root node so its share is 100%.
  aLandAllocator$mShare <- 1
}

#' LandAllocator_calcLandAllocation
#'
#' @details Initiates calculation of land allocation by
#'          calling calcLandAllocation on all children
#' @param aLandAllocator Land allocator for this region
#' @param aPeriod Model time period.
#' @author KVC September 2017
LandAllocator_calcLandAllocation <- function(aLandAllocator, aPeriod) {
  LandNode_calcLandAllocation(aLandAllocator$mChild, aLandAllocator$mLandAllocation, aPeriod)
}

#' LandAllocator_calcFinalLandAllocation
#'
#' @details Initiates calculation of land allocation
#'          Calls calibrateLandAllocator, calcLandShares, and calcLandAllocation
#' @param aLandAllocator Land allocator for this region.
#' @param aPeriod Model time period.
#' @author KVC September 2017
LandAllocator_calcFinalLandAllocation <- function(aLandAllocator, aPeriod) {
  # In calibration periods, check land area and set calibration values
  # TODO: Do we really need to do this twice? It also happens in initCalc
  if (aPeriod <= FINAL_CALIBRATION_PERIOD) {
    LandAllocator_calibrateLandAllocator(aLandAllocator, aPeriod)
  }

  # Calculate land shares
  LandAllocator_calcLandShares(aLandAllocator, "relative-cost", aPeriod)

  # Calculate land allocation
  LandAllocator_calcLandAllocation(aLandAllocator, aPeriod)
}
