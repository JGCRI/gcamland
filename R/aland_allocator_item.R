# aland_allocator_item.R

#' ALandAllocatorItem_calculateShareWeights
#'
#' @details Calculates share weights for land leafs and land nodes
#' @param aRegionName Region name
#' @param aChoiceFnAbove Type of logit
#' @param aPeriod Model time period
#' @author KVC September 2017
ALandAllocatorItem_calculateShareWeights <- function(aRegionName, aChoiceFnAbove, aPeriod) {
  if(DEBUG){
    print(paste("ALandAllocatorItem_calculateShareWeights for ", aRegionName, aPeriod))
  }

#   mShareWeight[ aPeriod ] = aChoiceFnAbove->calcShareWeight( mShare[ aPeriod ], mProfitRate[ aPeriod ], aPeriod );
#
#   // if we are in the final calibration year and we have "ghost" share-weights to calculate
#   // we do that now with the current profit rate in the final calibration period.
#   const Modeltime* modeltime = scenario->getModeltime();
#   if( aPeriod == modeltime->getFinalCalibrationPeriod() ) {
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
#   }
}
