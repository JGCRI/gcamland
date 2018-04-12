# plot_land.R

#' plotNest
#'
#' @param aLandAllocator Land allocator
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @details Plot the nesting structure for the land allocator
#' @importFrom igraph graph.data.frame layout.reingold.tilford
#' @author KVC October 2017
#' @export
plotNest <- function(aLandAllocator, aScenarioInfo) {
  # Silence package checks
  plot <- NULL

  # Get nest
  nest <- printNest(aLandAllocator, aScenarioInfo)

  # Convert to plottable format
  g <- graph.data.frame(nest)

  # Plot graph
  plot(g, layout = layout.reingold.tilford)
}

#' plotLandAllocation
#'
#' @details Plots land allocation over time
#' @param aLandAllocator Land Allocator
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @author KVC September 2017
#' @importFrom readr read_csv
#' @import ggplot2
#' @export
plotLandAllocation <- function(aLandAllocator, aScenarioInfo) {
  # Silence package checks
  land.allocation <- year <- name <- NULL

  # Get file name
  file <- paste0(aScenarioInfo$mOutputDir, "/land/landAllocation_", aScenarioInfo$mFileName, ".csv")

  # Read land allocation
  allLand <- suppressMessages(read_csv(normalizePath(file)))

  # TODO: add error message if output doesn't exist

  # Now, plot land allocation over time
  p <- ggplot() + geom_area(data = allLand, aes(year, land.allocation, fill=name))

  ## TODO:  Consider returning the plot instead of printing it.  That gives the user the
  ##        option of either printing it or saving it for later, as well as providing a
  ##        useful return value.  (The return value of print.ggplot is probably not
  ##        useful for most users.)
  print(p)
}

#' plotRegionalLandAllocation
#'
#' @details Plots regional land allocation over time.
#'          Aggregates land by region (i.e., adds all AEZs together)
#'          and then plots land.
#' @param aLandAllocator Land Allocator
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @importFrom readr read_csv
#' @importFrom tidyr separate
#' @importFrom dplyr group_by summarize
#' @import ggplot2
#' @author KVC October 2017
#' @export
plotRegionalLandAllocation <- function(aLandAllocator, aScenarioInfo) {
  # Silence package checks
  land.allocation <- year <- land.type <- name <- NULL

  # Get file name
  file <- paste0(aScenarioInfo$mOutputDir, "/land/landAllocation_", aScenarioInfo$mFileName, ".csv")

  # Read land allocation
  allLand <- suppressMessages(read_csv(normalizePath(file)))

  # TODO: add error message if output doesn't exist

  # Aggregate land
  allLand %>%
    separate(name, into=c("land.type", "AEZ"), sep="AEZ") %>%
    group_by(land.type, year) %>%
    summarize(land.allocation = sum(land.allocation)) ->
    regionalLand

  # Now, plot regional land allocation over time
  p <- ggplot() + geom_area(data = regionalLand, aes(year, land.allocation, fill=land.type))

  ## TODO:  return plot instead of printing it.
  print(p)
}

