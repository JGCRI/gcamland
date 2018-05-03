# land_node.R

#' LandNode
#'
#' @details Initialize an Class called LandNode
#' @param aName Node name
#' @param aChoiceFunction Choice function (logit type and exponent from node above)
#' @param aLandAllocation Land allocation for this node (reserved for future
#' use)
#' @param aFinalCalPeriod Final calibration period
#' @field mName Node name
#' @field mChoiceFunction Choice function (logit type and exponent) for this node
#' @field mLandAllocation Land allocation for this node
#' @field mFinalCalPeriod Final calibration period
#' @field mUnmanagedLandValue Unmanaged land value in this node
#' @field mShare Share of land allocated to this node
#' @field mShareWeight Share weight of this node
#' @field mProfitRate Profit rate of this node
#' @field mChildren list of LandLeaf children
#'
#' @return New, initialized LandNode
#' @author KVC September 2017
LandNode <- function(aName, aChoiceFunction, aLandAllocation, aFinalCalPeriod) {
  mName <- aName
  mChoiceFunction <- aChoiceFunction
  mFinalCalPeriod <- aFinalCalPeriod
  mLandAllocation = list()
  mUnmanagedLandValue = 0.0
  mShare = list()
  mShareWeight = NULL
  mProfitRate = list()
  mChildren = list()

  self <- environment()
  class(self) <- "LandNode"
  self
}

#' LandNode_initCalc
#'
#' @param aLandNode Land node.
#' @param aPeriod Model time period.
#' @details Initial calculations for the land node.
#'          Currently, this calls initCalc on children.
#' @author KVC September 2017
LandNode_initCalc <- function(aLandNode, aPeriod) {
  # TODO: all kinds of things including error checking
  # Call initCalc on any children
  for (child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      LandNode_initCalc(child, aPeriod)
    } else if (class(child) == "LandLeaf") {
      LandLeaf_initCalc(child, aPeriod)
    } else {
      UnmanagedLandLeaf_initCalc(child, aPeriod)
    }
  }
}

#' LandNode_setInitShares
#'
#' @details Calculates the initial share of land allocated to a node and calls
#'          a similar method for the node's children. This method is
#'          called during the calibration process so the shares
#'          set are prior to any calculations of share weights.
#' @param aLandNode Land node to perform calculations on
#' @param aLandAllocationAbove Land allocation of the parent node
#' @param aPeriod Model period
#' @author KVC September 2017
LandNode_setInitShares <- function(aLandNode, aLandAllocationAbove, aPeriod) {
  # Calculate the total land within this node.
  nodeLandAllocation <- LandNode_getCalLandAllocation(aLandNode, aPeriod)

  # If there is no land allocation for the parent land type, set the share to 0.
  # Otherwise, set the share of this node.
  if(aLandAllocationAbove > 0) {
    aLandNode$mShare[aPeriod] <- nodeLandAllocation / aLandAllocationAbove
  } else{
    aLandNode$mShare[aPeriod] <- 0.0
  }

  # Call setInitShares on all children
  for( child in aLandNode$mChildren ) {
    if(class(child) == "LandNode") {
      LandNode_setInitShares(child, nodeLandAllocation, aPeriod)
    } else {
      LandLeaf_setInitShares(child, nodeLandAllocation, aPeriod)
    }
  }
}

#' LandNode_getCalLandAllocation
#'
#' @details Calculates and returns total land allocation of a given type.
#' @param aLandNode LandNode
#' @param aPeriod Model period
#'
#' @return Calibrated land allocation for this node
#' @author KVC October 2017
LandNode_getCalLandAllocation <- function(aLandNode, aPeriod) {
  sum <- 0.0

  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      sum <- sum + LandNode_getCalLandAllocation(child, aPeriod)
    } else {
      sum <- sum + LandLeaf_getCalLandAllocation(child, aPeriod)
    }
  }

  return(sum)
}

#' LandNode_calcLandShares
#'
#' @details Uses the logit formulation to calculate the share
#'          of land allocated to a particular land type. A node's share
#'          is based on its profit rate and distribution parameter.
#'          A node's profit rate is NOT the weighted average of its
#'          childrens' profit rates but is based on the J. Clarke and Edmonds
#'          Logit paper and uses the scaled profits of the child nodes and leafs.
#' @param aLandNode Land node to perform calculations on
#' @param aChoiceFnAbove The discrete choice function from the level above.
#' @param aPeriod Period.
#' @author KVC September 2017
LandNode_calcLandShares <- function(aLandNode, aChoiceFnAbove, aPeriod) {
  # Step 1. Calculate the unnormalized shares.
  # These calls need to be made to initiate recursion into lower nests even
  # if the current node will have fixed shares.
  # Note: these are the log( unnormalized shares )
  i <- 1
  unNormalizedShares <- tibble::tibble(unnormalized.share = rep(NA, length(aLandNode$mChildren)))
  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      unNormalizedShares$unnormalized.share[i] <- LandNode_calcLandShares(child, aLandNode$mChoiceFunction, aPeriod)
    } else {
      unNormalizedShares$unnormalized.share[i] <- LandLeaf_calcLandShares(child, aLandNode$mChoiceFunction, aPeriod)
    }
    i <- i + 1
  }

  # Step 2. Normalize and set the share of each child. Also calculate info for node profit
  # The unnormalized shares will be normalizd after this call. Note: we
  # are using the normalization method in GCAM4.3
  normalizationInfo <- SectorUtils_normalizeShares(unNormalizedShares)

  i <- 1
  for ( child in aLandNode$mChildren ) {
    child$mShare[aPeriod] <- normalizationInfo$normalizedShares$share[i]
    i <- i + 1
  }

  # Step 3 Compute node profit based on unnormalized sum
  if(aLandNode$mChoiceFunction$mLogitExponent > 0.0) {
    aLandNode$mProfitRate[aPeriod] <- normalizationInfo$unnormalizedSum^(1.0 / aLandNode$mChoiceFunction$mLogitExponent)
  } else{
    aLandNode$mProfitRate[aPeriod] <- aLandNode$mUnmanagedLandValue
  }

  # Step 4. Calculate the unnormalized share for this node, but here using the discrete choice of the
  # containing or parant node.  This will be used to determine this nodes share within its
  # parent node.
  # TODO: Implement AbsoluteCostLogit
  if( aChoiceFnAbove$mType == "relative-cost") {
    if(aLandNode$mShareWeight > 0) {
      unNormalizedShare <- (aLandNode$mShareWeight * aLandNode$mProfitRate[[aPeriod]])^aChoiceFnAbove$mLogitExponent
    } else {
      unNormalizedShare <- 0
    }
  } else {
    print( "ERROR: Invalid choice function in LandNode_calcLandShares" )
  }

  return(unNormalizedShare)
}

#' LandNode_calculateShareWeights
#'
#' @details Calculates share weights at the node level and calls
#'          the share weight calculation for the node's children.
#'          Note: this now blends the calculateCalibrationProfitRate and
#'          calculateProfitScaler code in GCAM4.3. It will be helpful when
#'          implementing an absolute cost logit.
#' @param aLandNode Land node to perform calculations on
#' @param aChoiceFnAbove The discrete choice function from the level above.
#' @param aPeriod Period.
#' @author KVC September 2017
LandNode_calculateShareWeights <- function(aLandNode, aChoiceFnAbove, aPeriod) {
  # TODO: Figure out if this is needed and why.
  if(aChoiceFnAbove$mLogitExponent == 0) {
    aLandNode$mShareWeight <- aLandNode$mShare[[aPeriod]]
  } else {
    aLandNode$mShareWeight <- 1
  }

  # Set node calibration profit as the output cost in the choice function
  if(aChoiceFnAbove$mLogitExponent == 0.0) {
    avgProfit <- aLandNode$mUnmanagedLandValue
  } else {
    avgProfit <- aLandNode$mShare[[aPeriod]]^(1.0 / aChoiceFnAbove$mLogitExponent)
  }

  # Set the output cost for this node as its average profit rate
  aLandNode$mChoiceFunction$mOuputCost <- avgProfit

  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      LandNode_calculateShareWeights(child, aLandNode$mChoiceFunction, aPeriod)
    } else {
      LandLeaf_calculateShareWeights(child, aLandNode$mChoiceFunction, aPeriod)
    }
  }
}

#' LandNode_setUnmanagedLandProfitRate
#'
#' @details Unmanaged land leafs have a base profit rate that
#'          is equal to the average profit rate of that region
#'          or subregion. Loop through all children and set this.
#' @param aLandNode Land node.
#' @param aAverageProfitRate Average profit rate of region or subregion.
#' @param aPeriod model period.
#' @author KVC September 2017
LandNode_setUnmanagedLandProfitRate <- function(aLandNode, aAverageProfitRate, aPeriod) {
  # If node is the root of a fixed land area nest ( typically a subregion )
  # or the root of the entire land allocatory, then set the average profit
  # to the unmanaged land value
  if(aLandNode$mUnmanagedLandValue > 0.0) {
    avgProfitRate <- aLandNode$mUnmanagedLandValue
  } else {
    aLandNode$mUnmanagedLandValue <- aAverageProfitRate
    avgProfitRate <- aAverageProfitRate
  }

  # Loop through all children and call setUnmanagedLandProfitRate
  for(child in aLandNode$mChildren) {
    if (class(child) == "UnmanagedLandLeaf") {
      UnmanagedLandLeaf_setUnmanagedLandProfitRate(child, avgProfitRate, aPeriod)
    } else if (class(child) == "LandNode") {
      LandNode_setUnmanagedLandProfitRate(child, avgProfitRate, aPeriod)
    }
  }
}

#' LandNode_calculateNodeProfitRates
#'
#' @details Calculates profit rates at the node level using the
#'          logit function calculations.
#' @param aLandNode Current land node
#' @param aAverageProfitRateAbove Average profit rate of the parent node.
#' @param aChoiceFnAbove The discrete choice function from the level above.
#' @param aPeriod Period.
#' @author KVC September 2017
LandNode_calculateNodeProfitRates <- function(aLandNode, aAverageProfitRateAbove, aChoiceFnAbove, aPeriod) {
  # First, set average profit rate equal to unmanaged land value. This may be overwritten later.
  avgProfitRate <- aLandNode$mUnmanagedLandValue

  # If we have a valid profit rate above then we can calculate the implied profit rate this node
  # would have to recieve the share it did.  If not (such as at the root) we just use the
  # unmanaged land value.
  if(aAverageProfitRateAbove > 0.0) {
    share <- aLandNode$mShare[[aPeriod]]
    if(share > 0.0) {
      if( aChoiceFnAbove$mType == "relative-cost") {
        avgProfitRate = RelativeCostLogit_calcImpliedCost(aChoiceFnAbove, share,
                                                          aAverageProfitRateAbove, aPeriod)
      } else{
        print("ERROR: Invalid choice function in LandNode_calculateNodeProfitRates")
      }
    } else if(aPeriod == aLandNode$mFinalCalPeriod) {
      # TODO: Implement future technologies
      # It may be the case that this node contains only "future" crop/technologies.  In this case
      # we use the ghost share in it's first available year to calculate the implied profit rate.
      #   for( int futurePer = aPeriod + 1; futurePer < modeltime->getmaxper() && avgProfitRate < 0.0; ++futurePer ) {
      #     if( mGhostUnormalizedShare[ futurePer ].isInited() ) {
      #       avgProfitRate = aChoiceFnAbove->calcImpliedCost( mGhostUnormalizedShare[ futurePer ],
      #                                                        aAverageProfitRateAbove,
      #                                                        aPeriod );
      #     }
      #   }
      #   }
    }
  }

  # Set profit rate and output cost in choice function
  aLandNode$mProfitRate[aPeriod] <- avgProfitRate
  aLandNode$mChoiceFunction$mOutputCost <- avgProfitRate

  # Pass the node profit rate down to children and trigger their calculation
  # and pass down the logit exponent of this node
  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      LandNode_calculateNodeProfitRates(child, avgProfitRate, aLandNode$mChoiceFunction, aPeriod)
    }
  }

  # TODO: implement a absolute cost logit option
  # Calculate a reasonable "base" profit rate to use set the scale for when
  # changes in absolute profit rates would be made relative.  We do this by
  # taking the higest profit rate from any of the direct child items.
  # double maxChildProfitRate = -std::numeric_limits<double>::infinity();
  # for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
  #   maxChildProfitRate = std::max( maxChildProfitRate,
  #                                  mChildren[i]->getProfitRate( aPeriod ) );
  # }
  # mChoiceFn->setBaseCost( maxChildProfitRate, mName );
}

#' LandNode_calcLandAllocation
#'
#' @details Uses the land share and the allocation of land to
#'          the parent node to calculate the allocation of this
#'          node. CalculateLandShares must be called first.
#' @param aLandNode Land node
#' @param aLandAllocationAbove Land allocation of parent.
#' @param aPeriod model period.
#' @author KVC September 2017
LandNode_calcLandAllocation <- function(aLandNode, aLandAllocationAbove, aPeriod) {
  # TODO: asserts?
  # assert( mShare[ aPeriod ] >= 0.0 && mShare[ aPeriod ] <= 1.0 );

  # Calculate node land allocation
  nodeLandAllocation <- 0.0
  if(aLandAllocationAbove > 0.0 && aLandNode$mShare[[aPeriod]] > 0.0) {
    nodeLandAllocation <- aLandAllocationAbove * aLandNode$mShare[[aPeriod]]
  }

  # Set land allocation for the node
  aLandNode$mLandAllocation[aPeriod] <- nodeLandAllocation

  # Call calcLandAllocation for each child
  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode"){
      LandNode_calcLandAllocation(child, nodeLandAllocation, aPeriod)
    } else {
      LandLeaf_calcLandAllocation(child, nodeLandAllocation, aPeriod)
    }
  }
}

#' LandNode_getObservedAverageProfitRate
#'
#' @details Get observed average profit rate.
#' @param aProfitRate Profit rate.
#' @param aShare Share.
#' @param aPeriod Model time period.
#' @author KVC September 2017
LandNode_getObservedAverageProfitRate <- function(aProfitRate, aShare, aPeriod) {
#   aProfitRate = 0.0;
#   for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
#     double currProfitRate;
#     double currShare;
#     mChildren[ i ]->getObservedAverageProfitRate( currProfitRate, currShare, aPeriod );
#     aProfitRate += currShare * currProfitRate;
#   }
#
#   // If this leaf has a calibration share return that and if not try for a ghost
#   // share.
#   if( mShare[ aPeriod ] > 0 ) {
#     aShare = mShare[ aPeriod ];
#   }
#   else {
#     const Modeltime* modeltime = scenario->getModeltime();
#     for( int futurePer = aPeriod + 1; futurePer < modeltime->getmaxper(); ++futurePer ) {
#       if( mGhostUnormalizedShare[ futurePer ].isInited() ) {
#         aShare = mGhostUnormalizedShare[ futurePer ];
#         return;
#       }
#     }
#     // If we get here then there was no ghost share either so just set a share
#     // of zero.
#     aShare = 0.0;
#   }
# }
}

#' LandNode_getChildWithHighestShare
#'
#' @details Finds the child with the highest share (used for bioenergy calibration)
#' @param aPeriod Model time period.
#' @author KVC September 2017
LandNode_getChildWithHighestShare <- function(aPeriod) {
#   double maxShare = 0.0;
#   const ALandAllocatorItem* maxChild = 0;
#   for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
#     if( !mChildren[ i ]->isUnmanagedLandLeaf() && mChildren[ i ]->getShare( aPeriod ) > maxShare ) {
#       maxShare = mChildren[ i ]->getShare( aPeriod );
#       maxChild = mChildren[ i ];
#     }
#   }
#   return maxChild;
}

#' LandNode_calculateShareWeight
#'
#' @details Calculates share weights for land nodes
#' @param aLandNode Land node
#' @param aChoiceFnAbove Type of logit
#' @param aPeriod Model time period
#' @author KVC September 2017
LandNode_calculateShareWeight <- function(aLandNode, aChoiceFnAbove, aPeriod) {
  # Calculate the share weight for the node
  # TODO: implement absolute cost logit too
  if(aChoiceFnAbove$mType == "relative-cost") {
    aLandNode$mShareWeight <- RelativeCostLogit_calcShareWeight(aChoiceFnAbove,
                                                              aLandNode$mShare[[aPeriod]],
                                                              aLandNode$mProfitRate[[aPeriod]],
                                                              aPeriod)
  } else{
    print("ERROR: Invalid choice function in LandNode_calculateShareWeight")
  }

  # If we are in the final calibration year and we have "ghost" share-weights to calculate,
  # we do that now with the current profit rate in the final calibration period.
  if(aPeriod == aLandNode$mFinalCalPeriod) {
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

  # Note: Node share weights should always be 1
  # TODO: Write an assert to check this
}

