# relative_cost_logit.R

#' ChoiceFunction
#'
#' @details Logit choice function, both type and exponent
#' @param aType Type of logit (currently only "relative-cost" is supported)
#' @param aLogitExponent Logt exponent
#' @author KVC September 2017
ChoiceFunction <- function(aType, aLogitExponent){
  mType = aType
  mLogitExponent = aLogitExponent
  mOutputCost = NULL

  self <- environment()
  class(self) <- "ChoiceFunction"
  self

}

#' RelativeCostLogit_calcUnnormalizedShare
#'
#' @details Calculate the  numerator of the discrete choice (i.e., the unnormalized version)
#'          function being used to calculate child shares in this node. The normalization
#'          factor will be calculated later.
#'          Note: Negative costs can not be used in this logit formulation. Instead,
#'          the profit is capped at RelativeCostLogit::getMinCostThreshold.  This implies
#'          no behavior once profits have crossed this threshold.
#'          Note: this is modified to match the equations in GCAM4.3 (i.e., no log) but uses structure
#'          from versions of GCAM with the absolute cost logit.
#' @param aChoiceFnAbove Choice function (logit type and exponent from node above)
#' @param aShareWeight share weight for the choice for which the share is being calculated.
#' @param aProfit Profit for the choice for which the share is being calculated.
#' @param aPeriod model time period for the calculation.
#' @return log of the unnormalized share.
#' @author KVC September 2017
RelativeCostLogit_calcUnnormalizedShare <- function(aChoiceFnAbove, aShareWeight, aProfit, aPeriod) {
  # Negative profits are not allowed so they are instead capped at getMinCostThreshold()
  cappedProfit <- max(aProfit, RelativeCostLogit_getMinCostThreshold())

  return((aShareWeight*cappedProfit)^(aChoiceFnAbove$mLogitExponent))
}

#' RelativeCostLogit_calcShareWeight
#'
#' @details Calculate the share weight using the relative cost logit
#' @param aChoiceFnAbove Choice function (logit type and exponent from node above)
#' @param aShare Share leaf has of node's land
#' @param aProfit Observed profit rate
#' @param aPeriod Model time period
#' @return Share weight
#' @author KVC September 2017
RelativeCostLogit_calcShareWeight <- function(aChoiceFnAbove, aShare, aProfit, aPeriod) {
  # Negative costs are not allowed so they are instead capped at getMinCostThreshold()
  cappedProfit <- max(aProfit, RelativeCostLogit_getMinCostThreshold())

  # Guard against numerical instability in the pow when the share was zero anyway
  if (aShare == 0.0) {
    sharewt <- 0.0
  } else {
    calibrationProfit <- aChoiceFnAbove$mOutputCost * aShare^(1.0 / aChoiceFnAbove$mLogitExponent)
    sharewt <- calibrationProfit / cappedProfit
  }

  return(sharewt)
}

#' RelativeCostLogit_calcImpliedCost
#'
#' @details Calculate node profit rate (not sure why this is called "implied cost")
#' @param aChoiceFnAbove Choice function (logit type and exponent from node above)
#' @param aShare TODO
#' @param aCost TODO
#' @param aPeriod Model time period
#' @return Node profit rate (called IMPLIED_COST in this method)
#' @author KVC September 2017
RelativeCostLogit_calcImpliedCost <- function(aChoiceFnAbove, aShare, aCost, aPeriod) {
  # TODO: fix the way we store data
  if(aChoiceFnAbove$mLogitExponent == 0) {
    IMPLIED_COST <- aCost
  } else if(aShare == 0.0 & aChoiceFnAbove$mLogitExponent < 0) {
    IMPLIED_COST <- LARGE_NUMBER
  } else if(aShare == 0.0 & aChoiceFnAbove$mLogitExponent > 0) {
    IMPLIED_COST <- -LARGE_NUMBER
  } else {
     # Negative costs are not allowed so they are instead capped at getMinCostThreshold()
    cappedCost <- max(aCost, RelativeCostLogit_getMinCostThreshold())
    IMPLIED_COST <- cappedCost * (aShare ^ (1.0 / aChoiceFnAbove$mLogitExponent))
  }

  return(IMPLIED_COST)
}

#' RelativeCostLogit_getMinCostThreshold
#'
#' @details Get the minimum cost threshold value that may be used in this logit share equation.
#' @return The threshold value.
#' @author KVC September 2017
RelativeCostLogit_getMinCostThreshold <- function() {
  MIN_COST <- 0.001
  return(MIN_COST)
}
