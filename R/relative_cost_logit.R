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

  self <- environment()
  class(self) <- "ChoiceFunction"
  self

}

#' RelativeCostLogit_calcUnnormalizedShare
#'
#' @details Calculate the log of the numerator of the discrete choice (i.e., the unnormalized version)
#'          function being used to calculate subsector shares in this sector.  The normalization
#'          factor will be calculated later.
#'          Note: Negative costs can not be used in this logit formulation.  Instead the cost
#'          the cost is capped at RelativeCostLogit::getMinCostThreshold.  This implies
#'          no behavior once costs have crossed this threshold.
#' @param aShareWeight share weight for the choice for which the share is being calculated.
#' @param aCost cost for the choice for which the share is being calculated.
#' @param aPeriod model time period for the calculation.
#' @return log of the unnormalized share.
#' @author KVC September 2017
RelativeCostLogit_calcUnnormalizedShare <- function(aChoiceFnAbove, aShareWeight, aCost, aPeriod) {
  # Zero share weight implies no share which is signaled by negative infinity.
  if (aShareWeight > 0.0) {
    logShareWeight <- log(aShareWeight)
  } else {
    logShareWeight <- -Inf
  }

  # Negative costs are not allowed so they are instead capped at getMinCostThreshold()
  cappedCost <- max(aCost, RelativeCostLogit_getMinCostThreshold())

  # TODO: figure out how to store logit exponents
  return(logShareWeight + aChoiceFnAbove$mLogitExponent * log(cappedCost))
}

# double RelativeCostLogit::calcAverageCost( const double aUnnormalizedShareSum,
#                                            const double aLogShareFac,
#                                            const int aPeriod ) const
# {
#   double ret;
#   if( mLogitExponent[ aPeriod ] == 0.0 ) {
#     // TODO: what to do with zero logit?
#     ret = aUnnormalizedShareSum * exp( aLogShareFac ) * mOutputCost;
#   }
#   else if( aUnnormalizedShareSum == 0 && mLogitExponent[ aPeriod ] < 0 ) {
#     // No Valid options and negative logit so return a large cost so a nested
#     // logit would not want to choose this nest.
#     ret = util::getLargeNumber();
#   }
#   else if( aUnnormalizedShareSum == 0 && mLogitExponent[ aPeriod ] > 0 ) {
#     // No Valid options and positive logit so return a large negative cost
#     // so a nested logit would not want to choose this nest.
#     ret = -util::getLargeNumber();
#   }
#   else {
#     ret = exp( aLogShareFac / mLogitExponent[ aPeriod ] )
#     * pow( aUnnormalizedShareSum, 1.0 / mLogitExponent[ aPeriod ] );
#   }
#
#   return ret;
# }

# * \details  Given an an "anchor" choice with observed share and price and another choice
# *           also with observed share and price, compute the inverse of the discrete choice function
# *           to produce a share weight.
# * \param aShare observed share for the current choice.
# * \param aCost observed cost for the current choice.
# * \param aAnchorShare observed share for the anchor choice.
# * \param aAnchorCost observed cost for the anchor choice.
# * \param aPeriod model time period for the calculation.
# * \return share weight for the current choice.
# */
#   double RelativeCostLogit::calcShareWeight( const double aShare, const double aCost, const double aAnchorShare,
#                                              const double aAnchorCost, const int aPeriod ) const
# {
#   // Negative costs are not allowed so they are instead capped at getMinCostThreshold()
#   double cappedCost = std::max( aCost, getMinCostThreshold() );
#   double cappedAnchorCost = std::max( aAnchorCost, getMinCostThreshold() );
#
#   return ( aShare / aAnchorShare ) * pow( cappedAnchorCost / cappedCost, mLogitExponent[ aPeriod ] );
# }
#


#' RelativeCostLogit_calcShareWeight
#'
#' @details Calculate the share weight using the relative cost logit
#' @param aShare TODO
#' @param aCost TODO
#' @param aPeriod Model time period
#' @param OUTPUT_COST Node profit
#' @return Share weight
#' @author KVC September 2017
RelativeCostLogit_calcShareWeight <- function(aChoiceFnAbove, aShare, aCost, aPeriod, OUTPUT_COST) {
  # TODO: Move OUTPUT_COST to be a member variable instead of passed in, document/name better
  # Negative costs are not allowed so they are instead capped at getMinCostThreshold()
  cappedCost <- max(aCost, RelativeCostLogit_getMinCostThreshold())

  # Guard against numerical instability in the pow when the share was zero anyway
  if (aShare == 0.0) {
    SHARE_WEIGHT <- 0.0
  } else {
    SHARE_WEIGHT <- aShare * (OUTPUT_COST / cappedCost)^aChoiceFnAbove$mLogitExponent
  }

  return(SHARE_WEIGHT)
}

#' RelativeCostLogit_calcImpliedCost
#'
#' @details Calculate node profit rate (not sure why this is called "implied cost")
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

#
# void RelativeCostLogit::setOutputCost( const double aCost ) {
#   // Negative costs are not allowed so they are instead capped at getMinCostThreshold()
#   mOutputCost = std::max( aCost, getMinCostThreshold() );
# }
#
# void RelativeCostLogit::setBaseCost( const double aBaseCost, const std::string &aFailMsg ) {
#   // the relative cost logit does not utilize this parameter, so this function is a no-op
# }
#

#' RelativeCostLogit_getMinCostThreshold
#'
#' @details Get the minimum cost threshold value that may be used in this logit share equation.
#' @return The threshold value.
#' @author KVC September 2017
RelativeCostLogit_getMinCostThreshold <- function() {
  MIN_COST <- 0.001
  return(MIN_COST)
}
