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
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @author KVC September 2017
#' @export
plotLandAllocation <- function(aScenarioInfo) {
  # Silence package checks
  land.allocation <- year <- name <- NULL

  # Get file name
  file <- paste0(aScenarioInfo$mOutputDir, "/output_", aScenarioInfo$mFileName, ".rds")

  # Read land allocation
  allLand <- suppressMessages(readRDS(normalizePath(file)))

  # TODO: add error message if output doesn't exist

  # Now, plot land allocation over time
  ggplot2::ggplot() + ggplot2::geom_area(data = allLand, ggplot2::aes(year, land.allocation, fill=name))
}

#' plotRegionalLandAllocation
#'
#' @details Plots regional land allocation over time.
#'          Aggregates land by region (i.e., adds all AEZs together)
#'          and then plots land.
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @importFrom tidyr separate
#' @importFrom dplyr group_by summarize
#' @author KVC October 2017
#' @export
plotRegionalLandAllocation <- function(aScenarioInfo) {
  # Silence package checks
  land.allocation <- year <- land.type <- name <- NULL

  # Get file name
  file <- paste0(aScenarioInfo$mOutputDir, "/output_", aScenarioInfo$mFileName, ".rds")

  # Read land allocation
  allLand <- suppressMessages(readRDS(normalizePath(file)))

  # TODO: add error message if output doesn't exist

  # Aggregate land
  allLand %>%
    separate(name, into=c("land.type", "AEZ"), sep="AEZ") %>%
    group_by(land.type, year) %>%
    summarize(land.allocation = sum(land.allocation)) ->
    regionalLand

  # Now, plot regional land allocation over time
  ggplot2::ggplot() + ggplot2::geom_area(data = regionalLand, ggplot2::aes(year, land.allocation, fill=land.type))
}

