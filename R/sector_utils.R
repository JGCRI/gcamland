# sector_utils.R

#' SectorUtils_normalizeLogShares
#'
#' @details Normalize log shares so they add up to one.
#'          Also, calculate parameters used to compute node profit.
#' @param aShares Unnormalized shares
#' @return Normalized shares and parameters needed to calculate node profit
#' @importFrom dplyr summarize
#' @author KVC September 2017
SectorUtils_normalizeLogShares <- function(aShares) {
  # First, find the log of the largest unnormalized share
  aShares %>%
    summarize(lfac = max(unnormalized.share)) ->
    lfac

  # Check for all zero prices
  if(lfac$lfac == -Inf) {
    # TODO: Implement something here
#     // In this case, set all shares to zero and return.
#     // This is arguably wrong, but the rest of the code seems to expect it.
#     for( size_t i = 0; i < alogShares.size(); ++i ) {
#       alogShares[ i ] = 0.0;
#     }
#     return make_pair( 0.0, 0.0 );
  }

  # In theory we could check for lfac == +Inf here, but in light of how the log
  # shares are calculated, it would seem like that can't happen.

  # Rescale and get normalization sum
  sum <- 0.0
  aShares %>%
    select(-unnormalized.share) %>%
    mutate(share = -1) ->
    NORMALIZED_SHARES
  i <- 1
  while(i <= nrow(NORMALIZED_SHARES)) {
    NORMALIZED_SHARES$share[i] <- NORMALIZED_SHARES$share[i] - lfac$lfac
    sum <- sum + exp(NORMALIZED_SHARES$share[i])
    i <- i + 1
  }

  # Double check the normalization
  unnormAdjustedSum <- sum;
  norm <- log(sum)
  sum <- 0.0
  i <- 1
  while(i <= nrow(NORMALIZED_SHARES)) {
    NORMALIZED_SHARES$share[i] <- exp(NORMALIZED_SHARES$share[i] - norm) # Divide by norm constant and unlog
    sum <- sum + NORMALIZED_SHARES$share[i] # Accumulate sum of normalizes shares (should be 1 when we are done)
    i <- i + 1
  }

  # TODO: check to make sure sum is 1

  # Return normalized shares, and two parameters (unnormalizedSum, lfac) that are used to calculate node profit
  return(list(normalizedShares = NORMALIZED_SHARES,
           unnormalizedSum = unnormAdjustedSum,
           lfac = lfac$lfac))
}
