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
  fileName <- paste0("output_", aScenarioInfo$mFileName, ".rds")
  filePath <- file.path(aScenarioInfo$mOutputDir, fileName)

  # Read land allocation
  allLand <- suppressMessages(readRDS(normalizePath(filePath)))

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
  fileName <- paste0("output_", aScenarioInfo$mFileName, ".rds")
  filePath <- file.path(aScenarioInfo$mOutputDir, fileName)

  # Read land allocation
  allLand <- suppressMessages(readRDS(normalizePath(filePath)))

  # TODO: add error message if output doesn't exist

  # Aggregate land
  allLand %>%
    separate(name, into=c("land.type", "AEZ"), sep="AEZ") %>%
    group_by(land.type, year) %>%
    summarize(land.allocation = sum(land.allocation)) %>%
    ungroup() ->
    regionalLand

  # Define a color palette (default for ggplot2 is rainbow which is hard to read)
  my_palette <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928',
                  '#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffed6f')

  # Now, plot regional land allocation over time
  ggplot2::ggplot() + ggplot2::geom_area(data = regionalLand, ggplot2::aes(year, land.allocation, fill=land.type)) +
    ggplot2::scale_fill_manual(values=my_palette)

}

