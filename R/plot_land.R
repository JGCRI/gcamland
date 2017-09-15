# plot_land.R

#' plot_LandAllocation
#'
#' @details Plots land allocation over time
#' @param aLandAllocator Land Allocator
#' @author KVC September 2017
#' @import ggplot2
#' @export
plot_LandAllocation <- function(aLandAllocator) {
  # TODO: Figure out how to loop through bigger nests
  landNode <- aLandAllocator$mChild

  # Get data into a data frame
  tibble::tibble(name = rep(NA, length(landNode$mChildren)),
                 land.allocation = rep(NA, length(landNode$mChildren))) %>%
    repeat_add_columns(tibble::tibble(period = PERIODS)) ->
    allLand

  i <- 1
  for( leaf in landNode$mChildren ) {
    for (per in PERIODS) {
      allLand$name[i] <- leaf$mName
      allLand$period[i] <- per
      allLand$land.allocation[i] <- leaf$mLandAllocation[[per]]
      i <- i + 1
    }
  }

  # Now, plot land allocation over time
  p <- ggplot() + geom_area(data = allLand, aes(period, land.allocation, fill=name))
  print(p)

}
