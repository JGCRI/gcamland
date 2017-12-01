# land_leaf.R

#' LandLeaf
#'
#' @details Initialize an Class called LandLeaf
#' @param aName Leaf name
#' @field mName Leaf name
#' @field mProductName Name of product produced by this leaf
#' @field mLandAllocation Land allocation for this leaf
#' @field mCalLandAllocation Calibration land allocation for this leaf
#' @field mShare Share of land allocated to this leaf
#' @field mShareWeight Share weight of this leaf
#' @field mProfitRate Profit rate of this leaf
#' @field mCost Non-land variable cost of this leaf
#' @field mYield yield for this leaf
#' @field mExpectedYield expected yield for this leaf
#' @field mExpectedPrice expected price for this leaf
#' @field mCalOutput calibrated-output for this leaf
#' @field mNonLandCostTechChange Technical change on the cost of this leaf
#' @field mAgProdChange Technical change on yield for this leaf
#'
#' @return New, initialized LandLeaf
#' @author KVC September 2017
LandLeaf <- function(aName) {
  mName = aName
  mProductName = NULL
  mLandAllocation = list()
  mCalLandAllocation = list()
  mShare = list()
  mShareWeight = NULL
  mProfitRate = list()
  # We are including cost, yield, and tech change here, rather than AgProductionTechnology for convenience
  mCost = list()
  mYield = list()
  mExpectedYield = list()
  mExpectedPrice = list()
  mCalOutput = list() # Note: we read this in to calculate yield for consistency with C++
  mNonLandCostTechChange = list()
  mAgProdChange = list()
  greet = function() {
    cat(paste0("Hello, I am a LandLeaf named ", self$mName, ".\n"))
  }

  self <- environment()
  class(self) <- "LandLeaf"
  self
}

#' LandLeaf_initCalc
#'
#' @param aLandLeaf Land leaf
#' @param aPeriod Current time period
#' @details Initial calculations needed for the land leaf.
#' @author KVC September 2017
LandLeaf_initCalc <- function(aLandLeaf, aPeriod) {
  # TODO: Implement this if needed
#   if ( aPeriod > 1 ) {
#     // If leaf is a "new tech" get the scaler from its parent
#     if ( !mShareWeight[ aPeriod ].isInited()) {
#       mShareWeight[ aPeriod ] = mShareWeight[ aPeriod - 1 ];
#     }
#   }
}

#' LandLeaf_setInitShares
#'
#' @details Calculates the share of land allocated to a leaf.
#'          This method is called during the calibration process
#'          so the shares set are prior to any calculations
#'          of share weights.
#' @param aLandLeaf Land leaf to perform calculations on
#' @param aLandAllocationAbove Land allocation of the parent node
#' @param aPeriod Model period
#' @author KVC September 2017
LandLeaf_setInitShares <- function(aLandLeaf, aLandAllocationAbove, aPeriod) {
  # If there is no land allocation for the parent land type, set the share to a small number.
  # Otherwise, set the share of this node.
  if(aLandAllocationAbove <= 0) {
    aLandLeaf$mShare[aPeriod] <- SMALL_NUMBER
  } else {
    aLandLeaf$mShare[aPeriod] <- aLandLeaf$mCalLandAllocation[[aPeriod]] / aLandAllocationAbove
  }
}

#' LandLeaf_calcLandShares
#'
#' @details This method implements the logit function. A land type's
#'          share of land is based on its profit rate and the
#'          distribution assumed for the parent node ( aLogitExpAbove )
#' @param aLandLeaf Land leaf to perform calculations on
#' @param aChoiceFnAbove The discrete choice function from the level above.
#' @param aPeriod Model period
#' @return Unormalized land shares
#' @author KVC September 2017
LandLeaf_calcLandShares <- function(aLandLeaf, aChoiceFnAbove, aPeriod) {
  # Calculate the unnormalized share for this leaf
  # The unnormalized share is used by the parent node to
  # calculate the leaf's share of the parent's land
  # TODO: Implement AbsoluteCostLogit
  if(aChoiceFnAbove$mType == "relative-cost") {
    unNormalizedShare <- RelativeCostLogit_calcUnnormalizedShare(aChoiceFnAbove,
                                                               aLandLeaf$mShareWeight,
                                                               aLandLeaf$mProfitRate[[aPeriod]],
                                                               aPeriod)
  } else {
    print("ERROR: Invalid choice function in LandLeaf_calcLandShares")
  }

  return(unNormalizedShare)
}

#' LandLeaf_calcLandAllocation
#'
#' @details Land allocation is the product of the land
#'          allocated to the parent node and the share
#'          of land specified for this land leaf which is calculated
#'          previously using the logit function. Note: this method
#'          is called in every time period including calibration
#'          periods. Thus, land in a calibration period is not
#'          necessarily equal to read in values
#' @param aLandLeaf Land leaf to perform calculations on
#' @param aLandAllocationAbove Land allocated to the parent node
#' @param aPeriod Model period
#' @author KVC September 2017
LandLeaf_calcLandAllocation <- function(aLandLeaf, aLandAllocationAbove, aPeriod) {
  # TODO: asserts?
  #   assert( mShare[ aPeriod ] >= 0 && mShare[ aPeriod ] <= 1 );
  if(aLandAllocationAbove > 0.0) {
    aLandLeaf$mLandAllocation[aPeriod] <- aLandAllocationAbove * aLandLeaf$mShare[[aPeriod]]
  } else {
    aLandLeaf$mLandAllocation[aPeriod] <- 0.0;
  }
}

#' LandLeaf_getCalLandAllocation
#'
#' @details Get the amount of land in a particular period
#' @param aLandLeaf LandLeaf
#' @param aPeriod Model Period
#'
#' @return Land allocation for this leaf in this time period
#' @author KVC October 2017
LandLeaf_getCalLandAllocation <- function(aLandLeaf, aPeriod) {
  return(aLandLeaf$mCalLandAllocation[[aPeriod]])
}

#' LandLeaf_calculateShareWeights
#'
#' @details Calculates share weights for land leafs
#' @param aLandLeaf Land leaf to perform calculations on
#' @param aChoiceFnAbove Type of logit
#' @param aPeriod Model time period
#' @author KVC September 2017
LandLeaf_calculateShareWeights <- function(aLandLeaf, aChoiceFnAbove, aPeriod) {
  # TODO: move output cost to a member variable; implement absolute cost logit
  if( aChoiceFnAbove$mType == "relative-cost") {
    aLandLeaf$mShareWeight <- RelativeCostLogit_calcShareWeight(aChoiceFnAbove,
                                                              aLandLeaf$mShare[[aPeriod]],
                                                              aLandLeaf$mProfitRate[[aPeriod]],
                                                              aPeriod)
  } else {
    print("ERROR: Invalid choice function in LandLeaf_calculateShareWeight")
  }

  # TODO: Implement this
  # if we are in the final calibration year and we have "ghost" share-weights to calculate,
  # we do that now with the current profit rate in the final calibration period.
  if(aPeriod == FINAL_CALIBRATION_PERIOD) {
    #     double shareAdj = 1.0;
    #     double profitRateForCal = mProfitRate[ aPeriod ];
    #     if( mIsGhostShareRelativeToDominantCrop ) {
    #       double newCropAvgProfitRate;
    #       getObservedAverageProfitRate( newCropAvgProfitRate, shareAdj, aPeriod );
    #       double dominantCropAvgProfitRate;
    #       const ALandAllocatorItem* maxChild = getParent()->getChildWithHighestShare( aPeriod );
    #       if( maxChild ) {
    #         maxChild->getObservedAverageProfitRate( dominantCropAvgProfitRate, shareAdj, aPeriod );
    #         profitRateForCal *= dominantCropAvgProfitRate / newCropAvgProfitRate;
    #       }
    #       else {
    #         // there are no valid crops in this nest and we were instructed to make the ghost share
    #         // profit rate relative to them so we will zero out this land item.
    #         for( int futurePer = aPeriod + 1; futurePer < modeltime->getmaxper(); ++futurePer ) {
    #           mShareWeight[ futurePer ] = 0.0;
    #         }
    #         return;
    #       }
    #     }
    #     for( int futurePer = aPeriod + 1; futurePer < modeltime->getmaxper(); ++futurePer ) {
    #       if( mGhostUnormalizedShare[ futurePer ].isInited() ) {
    #
    #         mShareWeight[ futurePer ] = aChoiceFnAbove->calcShareWeight( mGhostUnormalizedShare[ futurePer ] /* * shareAdj */,
    #                                                                      profitRateForCal,
    #                                                                      futurePer );
    #       }
    #     }
  }
}


