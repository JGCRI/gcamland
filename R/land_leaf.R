# land_leaf.R

#' LandLeaf
#'
#' @details Initialize an Class called LandLeaf
#' @param aName Leaf name
#' @param aFinalCalPeriod Final calibration period
#' @param aFinalPeriod Final model period
#' @field mName Leaf name
#' @field mFinalCalPeriod Final calibration period
#' @field mFinalPeriod Final model period
#' @field mProductName Name of product produced by this leaf
#' @field mLandAllocation Land allocation for this leaf
#' @field mHarvestedLand Harvested land for this leaf
#' @field mHAtoCL Ratio of harvested area to physical crop land
#' @field mCalLandAllocation Calibration land allocation for this leaf
#' @field mShare Share of land allocated to this leaf
#' @field mShareWeight Share weight of this leaf
#' @field mProfitRate Profit rate of this leaf
#' @field mIsNewTech Flag indicating leaf is a new technology (no calibration data)
#' @field mIsGhostShareRelativeToDominantCrop Flag indicating whether to compare profits of a new crop to a dominant one
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
LandLeaf <- function(aName, aFinalCalPeriod, aFinalPeriod) {
  self <- new.env(parent=emptyenv())
  self$mName <- aName
  self$mFinalCalPeriod <- aFinalCalPeriod
  self$mFinalPeriod <- aFinalPeriod
  self$mProductName = NULL
  self$mLandAllocation = list()
  self$mHarvestedLand = list()
  self$mHAtoCL = list()
  self$mCalLandAllocation = list()
  self$mShare = list()
  self$mShareWeight = list()
  self$mProfitRate = list()
  self$mIsNewTech = FALSE
  self$mIsGhostShareRelativeToDominantCrop = TRUE
  # We are including cost, yield, and tech change here, rather than AgProductionTechnology for convenience
  self$mCost = list()
  self$mYield = list()
  self$mExpectedYield = list()
  self$mExpectedPrice = list()
  self$mCalOutput = list() # Note: we read this in to calculate yield for consistency with C++
  self$mNonLandCostTechChange = list()
  self$mAgProdChange = list()
  self$mSubsidy = list()

  class(self) <- c("LandLeaf", class(self))
  self
}

#' LandLeaf_initCalc
#'
#' @param aLandLeaf Land leaf
#' @param aPeriod Current time period
#' @details Initial calculations needed for the land leaf.
#' @author KVC September 2017
LandLeaf_initCalc <- function(aLandLeaf, aPeriod) {
  if (aPeriod > 1) {
    # Copy share weights forward
    if (length(aLandLeaf$mShareWeight) < aPeriod) {
      aLandLeaf$mShareWeight[aPeriod] <- aLandLeaf$mShareWeight[[aPeriod - 1]];
    }
  }
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
                                                               aLandLeaf$mShareWeight[[aPeriod]],
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
    aLandLeaf$mHarvestedLand[aPeriod] <- aLandLeaf$mLandAllocation[[aPeriod]] * aLandLeaf$mHAtoCL[[aPeriod]]
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
#' @param aParent Parent node (needed for calibrating new techs)
#' @param aScenarioInfo Scenario info object
#' @author KVC September 2017
LandLeaf_calculateShareWeights <- function(aLandLeaf, aChoiceFnAbove, aPeriod, aParent, aScenarioInfo) {
  # We only want to calculate share weights if this scenario requires it. Otherwise, we use read in values
  if(aScenarioInfo$mCalibrateShareWt == TRUE) {
    # TODO: move output cost to a member variable; implement absolute cost logit
    if( aChoiceFnAbove$mType == "relative-cost") {
      aLandLeaf$mShareWeight[aPeriod] <- RelativeCostLogit_calcShareWeight(aChoiceFnAbove,
                                                                           aLandLeaf$mShare[[aPeriod]],
                                                                           aLandLeaf$mProfitRate[[aPeriod]],
                                                                           aPeriod)
    } else {
      print("ERROR: Invalid choice function in LandLeaf_calculateShareWeight")
    }
  }

  # if we are in the final calibration year and we have "ghost" share-weights to calculate,
  # we do that now with the current profit rate in the final calibration period.
  if(aPeriod == aLandLeaf$mFinalCalPeriod & aLandLeaf$mIsNewTech == TRUE) {
    profitRateForCal <- aLandLeaf$mProfitRate[[aPeriod]]
    if( aLandLeaf$mIsGhostShareRelativeToDominantCrop == TRUE ) {
      newCropAvgProfitRate <- aLandLeaf$mProfitRate[[aPeriod]]

      # Get profit rate for sibling with the highest share
      maxChild <- LandNode_getChildWithHighestShare(aParent, aPeriod)
      dominantCropAvgProfitRate <- maxChild$mProfitRate[[aPeriod]]
      profitRateForCal <- profitRateForCal * dominantCropAvgProfitRate / newCropAvgProfitRate
    }

    # Loop through future periods setting the share weight
    # Note: we do this so we can adjust the ghost unnormalized share in the future (e.g., phase in bioenergy)
    currPer <- aPeriod + 1
    while(currPer < aLandLeaf$mFinalPeriod) {

      # Note: we'll need to switch this to use the leaf-specific unnormalized share once
      # we move to the multiple-management technoloiges
      if(length(aParent$mGhostUnnormalizedShare) >= currPer) {
        aLandLeaf$mShareWeight[currPer] <- RelativeCostLogit_calcShareWeight(aChoiceFnAbove,
                                                                    aParent$mGhostUnnormalizedShare[[currPer]],
                                                                    profitRateForCal,
                                                                    currPer)
      }

      currPer <- currPer + 1
    }
  }
}

