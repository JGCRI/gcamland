# land_leaf.R

#' LandLeaf
#'
#' @details Initialize an Class called LandLeaf
#' @param aName Leaf name
#' @param aLandAllocation Land allocation for this leaf
#' @field mName Leaf name
#' @field mLandAllocation Land allocation for this leaf
#' @field mShare Share of land allocated to this leaf
#' @field mShareWeight Share weight of this leaf
#' @field mProfitRate Profit rate of this leaf
#' @field mCost Non-land variable cost of this leaf
#' @field mYield yield for this leaf
#' @field mNonLandCostTechChange Technical change on the cost of this leaf
#' @field mAgProdChange Technical change on yield for this leaf
#'
#' @return New, initialized LandLeaf
#' @author KVC September 2017
LandLeaf <- function(aName, aLandAllocation) {
  mName = aName
  mLandAllocation = list(`1` = aLandAllocation, `2` = aLandAllocation)
  mShare = list(`1` = -1, `2` = -1)
  mShareWeight = NULL
  mProfitRate = list(`1` = -1, `2` = -1)
  # We are including cost, yield, and tech change here, rather than AgProductionTechnology for convenience
  mCost = list(`1` = 0, `2` = 0)
  mYield = list(`1` = 1, `2` = 0)
  mNonLandCostTechChange = list(`1` = 0, `2` = 0)
  mAgProdChange = list(`1` = 0, `2` = 0.02)
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
LandLeaf_initCalc <- function(aLandLeaf, aPeriod ) {
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
  if( aLandAllocationAbove <= 0) {
    aLandLeaf$mShare[aPeriod] <- SMALL_NUMBER
  } else {
    aLandLeaf$mShare[aPeriod] <- aLandLeaf$mLandAllocation[[aPeriod]] / aLandAllocationAbove
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
  unNormalizedShare <- RelativeCostLogit_calcUnnormalizedShare(aLandLeaf$mShareWeight,
                                                               aLandLeaf$mProfitRate[[aPeriod]],
                                                               aPeriod)

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
  if ( aLandAllocationAbove > 0.0 ) {
    aLandLeaf$mLandAllocation[aPeriod] <- aLandAllocationAbove * aLandLeaf$mShare[[aPeriod]]
  } else {
    aLandLeaf$mLandAllocation[aPeriod] <- 0.0;
  }
}

#' LandLeaf_calculateShareWeight
#'
#' @details Calculates share weights for land leafs
#' @param aLandLeaf Land leaf to perform calculations on
#' @param aChoiceFnAbove Type of logit
#' @param aPeriod Model time period
#' @param NODE_PROFIT Profit rate of node (TODO: put this in the choice function)
#' @author KVC September 2017
LandLeaf_calculateShareWeight <- function(aLandLeaf, aChoiceFnAbove, aPeriod, NODE_PROFIT) {
  # TODO: move output cost to a member variable; implement absolute cost logit
  aLandLeaf$mShareWeight <- RelativeCostLogit_calcShareWeight(aLandLeaf$mShare[[aPeriod]],
                                                              aLandLeaf$mProfitRate[[aPeriod]],
                                                              aPeriod, NODE_PROFIT)

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
