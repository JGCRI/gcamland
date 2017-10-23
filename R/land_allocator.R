# land_allocator.R

#' LandAllocator
#'
#' @details Initialize an Class called LandAllocator
#' @param aRegionName Region name
#' @field mRegionName Region name
#' @field mChoiceFunction Logit type & exponent for the top level of the nest
#' @field mLandAllocation Land allocation for this region
#' @field mShare Share of land
#' @field mChildren Children of the LandAllocator
#'
#' @return New, initialized LandAllocator
#' @author KVC September 2017
LandAllocator <- function(aRegionName) {
  mRegionName = aRegionName
  mChoiceFunction = ChoiceFunction("relative-cost", 0)
  mLandAllocation = NULL
  mShare = NULL
  mUnmanagedLandValue = 0.0
  mChildren = list()
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
  # Call land node's initCalc
  for ( child in aLandAllocator$mChildren ) {
    LandNode_initCalc(child, aPeriod)
  }

  if(aPeriod <= FINAL_CALIBRATION_PERIOD){
    LandAllocator_calibrateLandAllocator(aLandAllocator, aPeriod)
  }

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
  LandAllocator_setUnmanagedLandProfitRate(aLandAllocator, 0, aPeriod)

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
  aLandAllocator$mChoiceFunction$mOutputCost <- aLandAllocator$mUnmanagedLandValue
  LandAllocator_calculateNodeProfitRates(aLandAllocator, 0,
                                    aLandAllocator$mChoiceFunction, aPeriod)

  # /* Step 4. Calculate profit scalers. Because the calibration profit rate computed in Step 4
  # will most likely differ from the profit rate computed using the yield times price - cost, a
  # scaler or multiplier is solved for which makes the profit equal to the calibration profit. In
  # future periods, this scaler is then applied to the computed profit for use in the sharing
  # and node profit equations.  It is analagous to the share weight calibration approach. Also, it
  # will work the same for unmanaged land leafs with the land price being used as the profit.
  #
  # All of the calibration is captured in the leaves, so the share profit scalers for nodes are
  # set equal to 1.  */
  LandAllocator_calculateShareWeights(aLandAllocator, aLandAllocator$mChoiceFunction, aPeriod)
}


#' LandAllocator_calculateNodeProfitRates
#'
#' @param aLandAllocator LandAllocator
#' @param aUnmanagedLandValue Value of unmanaged land
#' @param aChoiceFunction Choice function
#' @param aPeriod Current model period
#' @details Loops through all of the children of the aLandAllocator
#'          and calls the calculateNodeProfitRates method on each
#' @author KVC October 2017
LandAllocator_calculateNodeProfitRates <- function(aLandAllocator, aUnmanagedLandValue, aChoiceFunction, aPeriod) {
  # Loop through all children
  for( child in aLandAllocator$mChildren ) {
    LandNode_calculateNodeProfitRates(child, aUnmanagedLandValue, aChoiceFunction, aPeriod)
  }
}

#' LandAllocator_calculateShareWeights
#'
#' @param aLandAllocator Land Allocator
#' @param aChoiceFunction Choice function
#' @param aPeriod Current time period
#' @details Loop through all children of the land allocator and
#'          call the calculateShareWeights method on each
#' @author KVC October 2017
LandAllocator_calculateShareWeights <- function(aLandAllocator, aChoiceFunction, aPeriod) {
  # Loop through all children
  for( child in aLandAllocator$mChildren ) {
    LandNode_calculateShareWeights(child, aChoiceFunction, aPeriod)
  }
}

#' LandAllocator_setInitShares
#'
#' @param aLandAllocator Land allocator for this region.
#' @param aPeriod model period.
#' @details Calculates and sets initial land shares
#' @author KVC September 2017
LandAllocator_setInitShares <- function(aLandAllocator, aPeriod) {
  # Call setInitShares for nodes
  for ( child in aLandAllocator$mChildren ) {
    LandNode_setInitShares(child, aLandAllocator$mLandAllocation, aPeriod)
  }
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
  # Step 1. set value of unmanaged land leaves
  LandAllocator_setUnmanagedLandProfitRate(aLandAllocator, aLandAllocator$mUnmanagedLandValue, aPeriod)

  # Step 2. Calculate the unnormalized shares.
  # These calls need to be made to initiate recursion into lower nests even
  # if the current node will have fixed shares.
  # Note: these are the log( unnormalized shares )
  i <- 1
  unNormalizedShares <- tibble::tibble(unnormalized.share = rep(NA, length(aLandAllocator$mChildren)))
  for( child in aLandAllocator$mChildren ) {
    if( class(child) == "LandNode") {
      unNormalizedShares$unnormalized.share[i] <- LandNode_calcLandShares(child, aLandAllocator$mChoiceFunction, aPeriod)
    } else {
      unNormalizedShares$unnormalized.share[i] <- LandLeaf_calcLandShares(child, aLandAllocator$mChoiceFunction, aPeriod)
    }
    i <- i + 1
  }

  # Step 3. Normalize and set the share of each child
  # The log( unnormalized ) shares will be normalizd after this call and it will
  # do it making an attempt to avoid numerical instabilities given the profit rates
  # may be large values.  The value returned is a pair<unnormalizedSum, log(scale factor)>
  # again in order to try to make calculations in a numerically stable way.
  normalizationInfo <- SectorUtils_normalizeLogShares( unNormalizedShares )

  i <- 1
  for ( child in aLandAllocator$mChildren ) {
    child$mShare[aPeriod] <- normalizationInfo$normalizedShares$share[ i ]
    i <- i + 1
  }


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
  for ( child in aLandAllocator$mChildren ) {
    LandNode_calcLandAllocation(child, aLandAllocator$mLandAllocation, aPeriod)
  }
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
  LandAllocator_calcLandShares(aLandAllocator, aLandAllocator$mChoiceFunction, aPeriod)

  # Calculate land allocation
  LandAllocator_calcLandAllocation(aLandAllocator, aPeriod)
}

#' LandAllocator_setUnmanagedLandProfitRate
#'
#' @param aLandAllocator Current land allocator
#' @param aUnmanagedLandValue Value of unmanaged land
#' @param aPeriod Current period
#' @details Initalize setting the unmanaged land value in all unmanaged land leafs,
#'          by calling `setUnmanagedLandProfitRate` on all children
#' @author KVC October 2017
LandAllocator_setUnmanagedLandProfitRate <- function(aLandAllocator, aUnmanagedLandValue, aPeriod) {
  # Call on all children
  for( child in aLandAllocator$mChildren ) {
    LandNode_setUnmanagedLandProfitRate(child, aUnmanagedLandValue, aPeriod)
  }
}

#' LandAllocator_readData
#'
#' @details Read in calibration data for the land allocator
#' @param aLandAllocator LandAllocator that needs data
#' @importFrom readr read_csv
#' @author KVC September 2017
LandAllocator_readData <- function(aLandAllocator) {
  # Silence package checks
  Period <- mLandAllocation <- NULL

  # Read in calibration data
  mgd.land.allocation <- suppressMessages(read_csv("./inst/extdata/calibration-data/managed_land_allocation.csv"))
  unmgd.land.allocation <- suppressMessages(read_csv("./inst/extdata/calibration-data/unmanaged_land_allocation.csv"))
  logit <- suppressMessages(read_csv("./inst/extdata/calibration-data/logit.csv"))

  # TODO: make this work on any land nest
  # TODO: make this work if you have multiple calibration periods
  # First, fill in land allocation for LandLeaf
  for ( per in PERIODS ) {
    # Only read in data for calibration periods
    if ( per <= FINAL_CALIBRATION_PERIOD ) {
      # First, put both managed & unmanaged land in the same
      # tibble, so we can calculate land allocation for nodes and the allocator
      unmgd.land.allocation %>%
        rename(LandLeaf = UnmanagedLandLeaf) %>%
        bind_rows(mgd.land.allocation) ->
        land.allocation

      # First, fill in total land allocation
      land.allocation %>%
        summarize(mLandAllocation = sum(mLandAllocation)) ->
        totalLand

      aLandAllocator$mLandAllocation <- totalLand

      # Next, fill in land allocation for nodes
      # TODO: create new nodes as we find them
      land.allocation %>%
        filter(Period == per) %>%
        group_by(LandNode, Period) %>%
        summarize(mLandAllocation = sum(mLandAllocation)) ->
        currNodeLand

      name <- currNodeLand[[c("LandNode")]]
      land <- currNodeLand[[c("mLandAllocation")]]

      # Read in logit exponent for this node
      logit %>%
        filter(LandNode == name) ->
        currLogit

      exponent <- currLogit[[c("mLogitExponent")]]
      choiceFunction <- ChoiceFunction("relative-cost", exponent)

      newNode <- LandNode(name, choiceFunction, land)

      # Now, Loop through all LandLeaf and fill in land allocation
      mgd.land.allocation %>%
        filter(Period == per) ->
        currLand

      # TODO: check that each leaf only exists once
      i <- 1
      children <- list()
      while ( i <= nrow(currLand) ) {
        # Get data and initialize a new leaf
        name <- currLand[[i, c("LandLeaf")]]
        land <- currLand[i, c("mLandAllocation")]
        newLeaf <- LandLeaf(name)
        newLeaf$mLandAllocation[[per]] <- land

        # Read-in yield, cost, tech change
        AgProductionTechnology_readData(newLeaf)

        # Add this leaf to the land allocator
        newNode$mChildren[[i]] <- newLeaf
        i <- i + 1
      }

      # Finally, Loop through all UnmanagedLandLeaf and fill in land allocation
      unmgd.land.allocation %>%
        filter(Period == per) ->
        currLand

      j <- 1
      while ( j <= nrow(currLand) ) {
        # Get data and initialize a new unmanaged leaf
        name <- currLand[[j, c("UnmanagedLandLeaf")]]
        land <- currLand[j, c("mLandAllocation")]
        newLeaf <- UnmanagedLandLeaf(name)
        newLeaf$mLandAllocation[[per]] <- land

        # Add this leaf to the land allocator
        newNode$mChildren[[j+i-1]] <- newLeaf
        j <- j + 1
      }

      aLandAllocator$mChildren[[1]] <- newNode
    }
  }
}

#' LandAllocator_addToNest
#'
#' @param aLandAllocator Land allocator
#' @param aNest Current nest
#' @details Determine all of the parent/child relationships
#'          in the land allocator.
#' @author KVC October 2017
LandAllocator_addToNest <- function(aLandAllocator, aNest) {
  nest <- aNest

  # TODO: Make this loop over children once there are multiple
  for(child in aLandAllocator$mChildren) {
    tibble::tibble(parent = "root",
                   node = child$mName) %>%
      bind_rows(nest) ->
      nest

    # Now, call addToNest on each of the children
    nest <- LandNode_addToNest(child, nest)
  }

  return(nest)
}

