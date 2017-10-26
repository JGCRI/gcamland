# sector_utils.R

#' SectorUtils_normalizeShares
#'
#' @details Normalize shares so they add up to one.
#' @param aShares Unnormalized shares
#' @return Normalized shares and parameters needed to calculate node profit
#' @importFrom dplyr summarize
#' @author KVC September 2017
SectorUtils_normalizeShares <- function(aShares) {
  # Silence package checks
  unnormalized.share <- totalValue <- NULL

  # Calculate total
  aShares %>%
    summarize(total = sum(unnormalized.share)) ->
    totalValue

  # Set up normalized share dataframe
  aShares %>%
    rename(share = unnormalized.share) ->
    normalizedShares

  unnormalizedSum <- 0.0
  i <- 1
  while(i <= nrow(normalizedShares)) {
    unnormalizedSum <- unnormalizedSum + normalizedShares$share[i]
    normalizedShares$share[i] <- normalizedShares$share[i] / totalValue[[c("total")]]
    i <- i + 1
  }

  # TODO: check to make sure sum is 1

  # Return normalized shares
  return(list(normalizedShares = normalizedShares,
              unnormalizedSum = unnormalizedSum))
}
