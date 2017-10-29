# plot_land.R

#' plot_Nest
#'
#' @param aLandAllocator Land allocator
#' @details Plot the nesting structure for the land allocator
#' @importFrom igraph graph.data.frame layout.reingold.tilford
#' @author KVC October 2017
#' @export
plot_Nest <- function(aLandAllocator) {
  # Silence package checks
  plot <- NULL

  # Read nest
  nest <- suppressMessages(read_csv("./outputs/landNest.csv"))

  # Convert to plottable format
  g <- graph.data.frame(nest)

  # Plot graph
  plot(g, layout = layout.reingold.tilford)
}

#' plot_LandAllocation
#'
#' @details Plots land allocation over time
#' @param aLandAllocator Land Allocator
#' @author KVC September 2017
#' @importFrom readr read_csv
#' @import ggplot2
#' @export
plot_LandAllocation <- function(aLandAllocator) {
  # Silence package checks
  land.allocation <- year <- NULL

  # Read land allocation
  allLand <- suppressMessages(read_csv("./outputs/landAllocation.csv"))

  # TODO: add error message if output doesn't exist

  # Now, plot land allocation over time
  p <- ggplot() + geom_area(data = allLand, aes(year, land.allocation, fill=name))
  print(p)

}

#' plot_RegionalLandAllocation
#'
#' @details Plots regional land allocation over time.
#'          Aggregates land by region (i.e., adds all AEZs together)
#'          and then plots land.
#' @param aLandAllocator Land Allocator
#' @author KVC September 2017
#' @importFrom readr read_csv
#' @importFrom tidyr separate
#' @importFrom dplyr group_by summarize
#' @import ggplot2
#' @author KVC October 2017
#' @export
plot_RegionalLandAllocation <- function(aLandAllocator) {
  # Silence package checks
  land.allocation <- year <- land.type <- NULL

  # Read land allocation
  allLand <- suppressMessages(read_csv("./outputs/landAllocation.csv"))

  # TODO: add error message if output doesn't exist

  # Aggregate land
  allLand %>%
    separate(name, into=c("land.type", "AEZ"), sep="AEZ") %>%
    group_by(land.type, year) %>%
    summarize(land.allocation = sum(land.allocation)) ->
    regionalLand

  # Now, plot regional land allocation over time
  p <- ggplot() + geom_area(data = regionalLand, aes(year, land.allocation, fill=land.type))
  print(p)

}

