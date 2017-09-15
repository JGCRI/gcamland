# land_node.R

#' LandNode_initCalc
#'
#' @param aRegionName Region name.
#' @param aPeriod Model time period.
#' @details Initial calculations for the land node.
#'          Currently, this copies share weights forward and
#'          calls initCalc on children.
#' @author KVC September 2017
LandNode_initCalc <- function(aRegionName, aPeriod) {
#   // TODO: all kinds of things including error checking
#   if ( aPeriod > 1 ) {
#   // Copy share weights forward if new ones haven't been read in or computed
#   if ( !mShareWeight[ aPeriod ].isInited() ) {
#     mShareWeight[ aPeriod ] = mShareWeight[ aPeriod - 1 ];
#   }
#   }
#
# // Call initCalc on any children
# for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
#   mChildren[ i ]->initCalc( aRegionName, aPeriod );
# }
}

#' LandNode_setInitShares
#'
#' @details Calculates the share of land allocated to a node and calls
#'          a similar method for the node's children. This method is
#'          called during the calibration process so the shares
#'          set are prior to any calculations of share weights.
#' @param aRegionName Region.
#' @param aLandAllocationAbove Land allocation of the parent node
#' @param aPeriod Model period
#' @import dplyr tidyr
#' @importFrom readr write_csv
#' @author KVC September 2017
LandNode_setInitShares <- function(aRegionName, aLandAllocationAbove, aPeriod) {
  # Calculate the total land within this node.
  # TODO: make this more robust
  nodeLandAllocation <- LANDNODE_CALDATA$area

  # If there is no land allocation for the parent land type, set the share to a small number.
  # Otherwise, set the share of this node.
  # TODO: Save this information somewhere
  LANDNODE_CALDATA %>%
    mutate(land_above = aLandAllocationAbove) %>%
    mutate(share = if_else(land_above <=0, 0, area / land_above)) %>%
    select(name, share) ->
    LANDNODE_SHARES

  # TODO: Change how we store data
  write_csv(LANDNODE_SHARES, "./inst/extdata/temp-data/LANDNODE_SHARES.csv")

  # Call setInitShares on all children
  # TODO: Loop through somehow
  LandLeaf_setInitShares(aRegionName, nodeLandAllocation, aPeriod)
}

#' LandNode_calcLandShares
#'
#' @details Uses the logit formulation to calculate the share
#'          of land allocated to a particular land type. A node's share
#'          is based on its profit rate and distribution parameter.
#'          A node's profit rate is NOT the weighted average of its
#'          childrens' profit rates but is based on the J. Clarke and Edmonds
#'          Logit paper and uses the scaled profits of the child nodes and leafs.
#' @param aRegionName Region
#' @param aChoiceFnAbove The discrete choice function from the level above.
#' @param aPeriod Period.
#' @author KVC September 2017
LandNode_calcLandShares <- function(aRegionName, aChoiceFnAbove, aPeriod) {
  # Step 1. Calculate the unnormalized shares.
  # These calls need to be made to initiate recursion into lower nests even
  # if the current node will have fixed shares.
  # Note: these are the log( unnormalized shares )
  # TODO: loop over children
  UNNORMALIZED_SHARES <- LandLeaf_calcLandShares(aRegionName, aChoiceFnAbove, aPeriod)

  # Step 2. Normalize and set the share of each child
  # The log( unnormalized ) shares will be normalizd after this call and it will
  # do it making an attempt to avoid numerical instabilities given the profit rates
  # may be large values.  The value returned is a pair<unnormalizedSum, log(scale factor)>
  # again in order to try to make calculations in a numerically stable way.
  unnormalizedSum <- SectorUtils_normalizeLogShares(UNNORMALIZED_SHARES)

  # TODO: Figure out if we need to do this
  #   for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
  #   mChildren[ i ]->setShare( unnormalizedShares[ i ], aPeriod );
  #   }

  # TODO: Complete this
  # Step 3 Option (a) . compute node profit based on share denominator
  #   mProfitRate[ aPeriod ] = mChoiceFn->calcAverageCost( unnormalizedSum.first, unnormalizedSum.second, aPeriod );

  # Step 4. Calculate the unnormalized share for this node, but here using the discrete choice of the
  # containing or parant node.  This will be used to determine this nodes share within its
  # parent node.
  #   double unnormalizedShareAbove = aChoiceFnAbove->calcUnnormalizedShare( mShareWeight[ aPeriod ], mProfitRate[ aPeriod ], aPeriod );

  #   return unnormalizedShareAbove; // the unnormalized share of this node.
}

#' LandNode_calculateShareWeights
#'
#' @details Calculates share weights at the node level and calls
#'          the share weight calculation for the node's children.
#' @param aRegionName Region
#' @param aChoiceFnAbove The discrete choice function from the level above.
#' @param aPeriod Period.
#' @author KVC September 2017
LandNode_calculateShareWeights <- function(aRegionName, aChoiceFnAbove, aPeriod) {
  # For testing, use separate implementations for LandLeaf and LandNode
  LandNode_calculateShareWeight(aRegionName, aChoiceFnAbove, aPeriod)
  LandLeaf_calculateShareWeight(aRegionName, aChoiceFnAbove, aPeriod)

  # TODO: Figure this out
  # we can use the base class implementation to calculate the share weight at this node.
  #ALandAllocatorItem_calculateShareWeights(aRegionName, aChoiceFnAbove, aPeriod)
  # Call share weight calculation for each child
  #   for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
  #   mChildren[ i ]->calculateShareWeights( aRegionName, mChoiceFn.get(), aPeriod );
  #   }
}

#' LandNode_setUnmanagedLandProfitRate
#'
#' @details Unmanaged land leafs have a base profit rate that
#'          is equal to the average profit rate of that region
#'          or subregion.
#' @param aRegionName Region name.
#' @param aAverageProfitRate Average profit rate of region or subregion.
#' @param aPeriod model period.
#' @author KVC September 2017
LandNode_setUnmanagedLandProfitRate <- function(aRegionName, aAverageProfitRate, aPeriod) {
# {
#   double avgProfitRate = aAverageProfitRate;
#   // If node is the root of a fixed land area nest ( typically a subregion )
#   // or the root of the entire land allocatory, then set the average profit
#   // rate to the previously calculated value.
#   if ( mUnManagedLandValue > 0.0 ) {
#   avgProfitRate = mUnManagedLandValue;
#   }
#   else {
#   mUnManagedLandValue = avgProfitRate;
#   }
#   for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
#   // assign the unmanaged land value to this node and to children.
#   mChildren[ i ]->setUnmanagedLandProfitRate( aRegionName, avgProfitRate, aPeriod );
#   }
# }
#
}

#' LandNode_calculateNodeProfitRates
#'
#' @details Calculates profit rates at the node level using the
#'          logit function calculations.
#' @param aRegionName Region
#' @param aAverageProfitRateAbove Average profit rate of the parent node.
#' @param aChoiceFnAbove The discrete choice function from the level above.
#' @param aPeriod Period.
#' @importFrom readr read_csv write_csv
#' @author KVC September 2017
LandNode_calculateNodeProfitRates <- function(aRegionName, aAverageProfitRateAbove, aChoiceFnAbove, aPeriod) {
  avgProfitRate = -SMALL_NUMBER

  # If we have a valid profit rate above then we can calculate the implied profit rate this node
  # would have to recieve the share it did.  If not (such as at the root) we just use the
  # unmanaged land value.
  if(aAverageProfitRateAbove > 0.0) {
    # TODO: Fix the way data is stored
    LANDNODE_SHARES <- suppressMessages(read_csv("./inst/extdata/temp-data/LANDNODE_SHARES.csv"))
    mShare <- LANDNODE_SHARES$share
    if(mShare > 0.0) {
      avgProfitRate = RelativeCostLogit_calcImpliedCost(mShare, aAverageProfitRateAbove, aPeriod)
    } else if(aPeriod == FINAL_CALIBRATION_PERIOD) {
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
    } else {
      avgProfitRate = UNMANAGED_LAND_VALUE
    }
  }

  # TODO: Figure out how to store data
  tibble(name = "Land", profit = avgProfitRate) -> LANDNODE_PROFIT
  write_csv(LANDNODE_PROFIT, "./inst/extdata/temp-data/LANDNODE_PROFIT.csv")

  # TODO: Store the profit rate which will be used during calibration when calculating share-weights, etc.
  # mProfitRate[ aPeriod ] = avgProfitRate;
  # mChoiceFn->setOutputCost( avgProfitRate );

  # TODO: Deal with children
  # pass the node profit rate down to children and trigger their calculation
  # and pass down the logit exponent of this node
  # for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
  #   mChildren[ i ]->calculateNodeProfitRates( aRegionName, avgProfitRate,
  #                                             mChoiceFn.get(), aPeriod );
  # }

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
#' @param aRegionName Region name.
#' @param aLandAllocationAbove Land allocation of parent.
#' @param aPeriod model period.
#' @author KVC September 2017
LandNode_calcLandAllocation <- function(aRegionName, aLandAllocationAbove, aPeriod) {
#   assert( mShare[ aPeriod ] >= 0.0 && mShare[ aPeriod ] <= 1.0 );
#
#   // Calculate node land allocation
#   double nodeLandAllocation = 0.0;
#   if ( aLandAllocationAbove > 0.0 && mShare[ aPeriod ] > 0.0 ) {
#     nodeLandAllocation = aLandAllocationAbove * mShare[ aPeriod ];
#   }
#
#   // Call calcLandAllocation for each child
#   for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
#     mChildren[ i ]->calcLandAllocation( aRegionName, nodeLandAllocation, aPeriod );
#   }
}

#' LandNode_getObservedAverageProfitRate
#'
#' @details Get observed average profit rate.
#' @param aProfitRate Profit rate.
#' @param aShare Share.
#' @param aPeriod Model time period.
#' @author KVC September 2017
LandNode_getObservedAverageProfitRate <- function(aProfitRate, aShare, aPeriod) {
# {
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
#' @param aRegionName Region name
#' @param aChoiceFnAbove Type of logit
#' @param aPeriod Model time period
#' @importFrom readr read_csv
#' @author KVC September 2017
LandNode_calculateShareWeight <- function(aRegionName, aChoiceFnAbove, aPeriod) {
  # TODO: handle data better
  PROFIT <- suppressMessages(read_csv("./inst/extdata/temp-data/LANDNODE_PROFIT.csv"))
  SHARES <- suppressMessages(read_csv("./inst/extdata/temp-data/LANDNODE_SHARES.csv"))

  # TODO: move output cost to a member variable
  SHARE_WEIGHT <- RelativeCostLogit_calcShareWeight(SHARES$share, PROFIT$profit, aPeriod, UNMANAGED_LAND_VALUE)

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

  # Note: Node share weights should always be 1, so we aren't going to print them
  # TODO: Write an assert to check this
}

