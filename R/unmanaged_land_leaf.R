# unmanaged_land_leaf.R

#' UnmanagedLandLeaf
#'
#' @details Initialize an Class called UnmanagedLandLeaf
#' @param aName Leaf name
#' @field mName Leaf name
#' @field mLandAllocation Land allocation for this leaf
#' @field mShare Share of land allocated to this leaf
#' @field mShareWeight Share weight of this leaf
#' @field mProfitRate Profit rate of this leaf
#'
#' @return New, initialized UnmanagedLandLeaf
#' @author KVC October 2017
UnmanagedLandLeaf <- function(aName) {
  mName = aName
  mLandAllocation = list()
  mShare = list()
  mShareWeight = NULL
  mProfitRate = list()
  greet = function() {
    cat(paste0("Hello, I am a UnmanagedLandLeaf named ", self$mName, ".\n"))
  }

  self <- environment()
  class(self) <- "UnmanagedLandLeaf"
  self
}

#' UnmanagedLandLeaf_setUnmanagedLandProfitRate
#'
#' @param aUnmanagedLandLeaf Leaf to set profit rate for
#' @param aAverageProfitRate Average profit rate
#' @param aPeriod Time period
#' @details This method adjusts the profit rate of an unmanaged land leaf
#'          to account for the carbon value of land if the ag subsidy is
#'          is active and a carbon price exists.
#' @author KVC October 2017
UnmanagedLandLeaf_setUnmanagedLandProfitRate <- function(aUnmanagedLandLeaf, aAverageProfitRate, aPeriod) {
  # TODO: Decide whether to implement this
  # Adjust profit rate for land expnasion costs if applicable
  # if ( mIsLandExpansionCost ) {
  # subtract off expansion cost from profit rate
  # expansionCost = marketplace->getPrice( mLandExpansionCostName, aRegionName, aPeriod );
  # adjustedProfitRate = adjustedProfitRate - expansionCost;
  # }

  # TODO: Implement carbon subsidy so we can do UCT
  # mProfitRate[ aPeriod ] = max( adjustedProfitRate + getCarbonSubsidy( aRegionName, aPeriod ), 0.0 );

  aUnmanagedLandLeaf$mProfitRate[[aPeriod]] <- aAverageProfitRate
}
