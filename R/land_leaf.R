# land_leaf.R

#' LandLeaf_initCalc
#'
#' @param aRegionName Name of the region
#' @param aPeriod Current time period
#' @details Initial calculations needed for the land leaf.
#'          Currently, this just copies shareweights forward.
#' @author KVC September 2017
LandLeaf_initCalc <- function(aRegionName, aPeriod ) {
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
#' @param aRegionName Region.
#' @param aLandAllocationAbove Land allocation of the parent node
#' @param aPeriod Model period
#' @importFrom readr write_csv
#' @author KVC September 2017
LandLeaf_setInitShares <- function(aRegionName, aLandAllocationAbove, aPeriod) {
  if(DEBUG){
    print(paste("LandLeaf_setInitShares for ", aRegionName, aPeriod))
  }

  # If there is no land allocation for the parent land type, set the share to a small number.
  # Otherwise, set the share of this node.
  # TODO: Save this somehow/somewhere. Also, split into individual leafs
  LANDLEAF_CALDATA %>%
    mutate(land_above = aLandAllocationAbove) %>%
    mutate(share = if_else(land_above <=0, 0, area / land_above)) %>%
    select(name, share) ->
    LANDLEAF_SHARES

  if(DEBUG) {
    print(LANDLEAF_SHARES)
  }

  # TODO: Change how we store data
  write_csv(LANDLEAF_SHARES, "./inst/extdata/temp-data/LANDLEAF_SHARES.csv")
}

#' LandLeaf_calcLandShares
#'
#' @details This method implements the logit function. A land type's
#'          share of land is based on its profit rate and the
#'          distribution assumed for the parent node ( aLogitExpAbove )
#' @param aRegionName Region.
#' @param aChoiceFnAbove The discrete choice function from the level above.
#' @param aPeriod Model period
#' @return Unormalized land shares
#' @author KVC September 2017
LandLeaf_calcLandShares <- function(aRegionName, aChoiceFnAbove, aPeriod) {
  if(DEBUG){
    print(paste("LandLeaf_calcLandShares for ", aRegionName, aPeriod))
  }

  # Calculate the unnormalized share for this leaf
  # The unnormalized share is used by the parent node to
  # calculate the leaf's share of the parent's land
  # TODO: do this right
  PROFIT <- read_csv("./inst/extdata/temp-data/LANDLEAF_PROFIT.csv")
  SHARES <- read_csv("./inst/extdata/temp-data/LANDLEAF_SHARES.csv")

  # TODO: move output cost to a member variable; move the loop over leafs somewhere else
  SHARES %>%
    select(-share) %>%
    mutate(unnormalized.share = -SMALL_NUMBER) ->
    UNNORMALIZED_SHARES

  i <- 1
  while(i <= nrow(SHARES)) {
    UNNORMALIZED_SHARES$unnormalized.share[i] <- RelativeCostLogit_calcUnnormalizedShare(SHARES$share[i], PROFIT$profit[i], aPeriod)
    i <- i + 1
  }

  print(UNNORMALIZED_SHARES)

  return(UNNORMALIZED_SHARES)
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
#' @param aRegionName Region name.
#' @param aLandAllocationAbove Land allocated to the parent node
#' @param aPeriod Model period
#' @author KVC September 2017
LandLeaf_calcLandAllocation <- function(aRegionName, aLandAllocationAbove, aPeriod) {
#   assert( mShare[ aPeriod ] >= 0 &&
#   mShare[ aPeriod ] <= 1 );
#
#   if ( aLandAllocationAbove > 0.0 ) {
#   mLandAllocation[ aPeriod ] = aLandAllocationAbove * mShare[ aPeriod ];
#   }
#   else {
#   mLandAllocation[ aPeriod ] = 0.0;
#   }
}

#' LandLeaf_calculateShareWeight
#'
#' @details Calculates share weights for land leafs
#' @param aRegionName Region name
#' @param aChoiceFnAbove Type of logit
#' @param aPeriod Model time period
#' @importFrom readr read_csv write_csv
#' @author KVC September 2017
LandLeaf_calculateShareWeight <- function(aRegionName, aChoiceFnAbove, aPeriod) {
  if(DEBUG){
    print(paste("LandLeaf_calculateShareWeights for ", aRegionName, aPeriod))
  }

  # TODO: handle data better
  NODE_PROFIT <- read_csv("./inst/extdata/temp-data/LANDNODE_PROFIT.csv")
  PROFIT <- read_csv("./inst/extdata/temp-data/LANDLEAF_PROFIT.csv")
  SHARES <- read_csv("./inst/extdata/temp-data/LANDLEAF_SHARES.csv")

  # TODO: move output cost to a member variable; move the loop over leafs somewhere else
  SHARES %>%
    select(-share) %>%
    mutate(share.weight = -SMALL_NUMBER) ->
    SHARE_WEIGHT

  i <- 1
  while(i <= nrow(SHARES)) {
    SHARE_WEIGHT$share.weight[i] <- RelativeCostLogit_calcShareWeight(SHARES$share[i], PROFIT$profit[i], aPeriod, NODE_PROFIT$profit)
    i <- i + 1
  }

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

  print(SHARE_WEIGHT)

  write_csv(SHARE_WEIGHT, "./inst/extdata/temp-data/LANDLEAF_SHAREWEIGHT.csv")
}


