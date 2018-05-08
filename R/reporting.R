# reporting.R

# Note: this file serves the function of `xml_db_outputter` in GCAM4.3

#' printLandAllocation
#'
#' @details Prints all outputs
#' @param aLandAllocator Land allocator
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @author KVC October 2017
printOutput <- function(aLandAllocator, aScenarioInfo) {
  nest <- getNest(aLandAllocator)
  printAllOutputs(aLandAllocator, aScenarioInfo, nest)
}

#' Write out additional information for debugging
#'
#' Write the land shares and land nests to their respective output files.  This
#' information will be written out if the \code{aVerbose} argument to
#' \code{\link{run_model}} is \code{TRUE}.
#'
#' @param aLandAllocator Land allocator structure
#' @param aScenarioInfo Scenario parameter structure
printDebug <- function(aLandAllocator, aScenarioInfo)
{
    nest <- printNest(aLandAllocator, aScenarioInfo)
    printLandShares(aLandAllocator, aScenarioInfo, nest)
    printPrices(aScenarioInfo)
    invisible(NULL)
}

#' printPrices
#'
#' @details Print prices
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @author KVC November 2017
printPrices <- function(aScenarioInfo) {
    file <- file.path(aScenarioInfo$mOutputDir, "prices.csv")
    type <- aScenarioInfo$mScenarioType
    write_csv(PRICES[[type]], file)
}

#' printAllOutputs
#'
#' @details Prints all outputs (land allocation, expected price, expected yield, yield) by land leaf
#' @param aLandAllocator Land allocator
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @param aNest Nest to fill in
#' @importFrom readr write_csv read_csv
#' @author KVC May 2018
printAllOutputs <- function(aLandAllocator, aScenarioInfo, aNest) {
  # Silence package checks
  node <- parent <- uniqueJoinField <- NULL

  scenType <- aScenarioInfo$mScenarioType

  # Get a list of leafs
  nodes <- unique(aNest$parent)
  aNest %>%
    filter(node %!in% nodes) ->
    leafs

  # Some leafs have the same parent node name. We need to add those
  aNest %>%
    filter(parent == node) %>%
    bind_rows(leafs) ->
    leafs

  # Get data into a data frame
  tibble::tibble(name = leafs$node,
                 land.allocation = rep(NA, length(leafs$node)),
                 yield = rep(NA, length(leafs$node)),
                 expectedYield = rep(NA, length(leafs$node)),
                 expectedPrice = rep(NA, length(leafs$node))) %>%
    mutate(uniqueJoinField = 1) %>%
    full_join(mutate(tibble(year = YEARS[[scenType]]), uniqueJoinField = 1),
              by = "uniqueJoinField") %>%
    select(-uniqueJoinField) ->
    allOutput

  allOutput <- LandAllocator_getOutputs(aLandAllocator, allOutput, scenType)

  # Add information on scenario and expectation type
  allOutput$scenario <- aScenarioInfo$mScenarioName

  file <- paste0(aScenarioInfo$mOutputDir, "/output_", aScenarioInfo$mFileName, ".csv")
  write_csv(allOutput, file)
}

#' LandAllocator_getOutputs
#'
#' @details Calculates and returns all outputs for the land allocator
#' @param aLandAllocator LandAllocator
#' @param aAllOutputs Data frame to fill in outputs
#' @param aScenType Scenario type: either "Reference" or "Hindcast"
#'
#' @return Output table
#' @author KVC May 2018
LandAllocator_getOutputs <- function(aLandAllocator, aAllOutputs, aScenType) {
  for(child in aLandAllocator$mChildren) {
    if(class(child) == "LandNode") {
      aAllOutputs <- LandNode_getOutputs(child, aAllOutputs, aScenType)
    } else {
      aAllOutputs <- LandLeaf_getOutputs(child, aAllOutputs, aScenType)
    }
  }

  return(aAllOutputs)
}

#' LandNode_getOutputs
#'
#' @details Calculates and returns all outputs for this node
#' @param aLandNode LandNode
#' @param aAllOutputs Data frame to fill in outputs
#' @param aScenType Scenario type: either "Reference" or "Hindcast"
#'
#' @return Output table
#' @author KVC May 2018
LandNode_getOutputs <- function(aLandNode, aAllOutputs, aScenType) {

  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      aAllOutputs <- LandNode_getOutputs(child, aAllOutputs, aScenType)
    } else {
      aAllOutputs <- LandLeaf_getOutputs(child, aAllOutputs, aScenType)
    }
  }

  return(aAllOutputs)
}

#' LandLeaf_getOutputs
#'
#' @details Calculates and returns outputs for this leaf in all periods
#' @param aLandLeaf LandLeaf
#' @param aAllOutputs Data frame to fill in outputs
#' @param aScenType Scenario type: either "Reference" or "Hindcast"
#'
#' @return Output table
#' @author KVC May 2018
LandLeaf_getOutputs <- function(aLandLeaf, aAllOutputs, aScenType) {
  for(per in seq_along(aLandLeaf$mLandAllocation)) {
    currName <- aLandLeaf$mName[1]
    currYear <- get_per_to_yr(per, aScenType)
    aAllOutputs$land.allocation[aAllOutputs$year == currYear &
                                  aAllOutputs$name == currName] <- aLandLeaf$mLandAllocation[[per]]

    # Only get yield and price information for LandLeafs
    if(class(aLandLeaf) == "LandLeaf") {
      aAllOutputs$yield[aAllOutputs$name == currName &
                          aAllOutputs$year == currYear] <- aLandLeaf$mYield[[per]]
      aAllOutputs$expectedYield[aAllOutputs$name == currName &
                                  aAllOutputs$year == currYear] <- aLandLeaf$mExpectedYield[[per]]
      aAllOutputs$expectedPrice[aAllOutputs$name == currName &
                                  aAllOutputs$year == currYear] <- aLandLeaf$mExpectedPrice[[per]]
    }
  }

  return(aAllOutputs)
}

#' printLandShares
#'
#' @details Prints land share for all nodes and leafs
#' @param aLandAllocator Land allocator
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @param aNest Current land nest
#' @importFrom readr write_csv read_csv
#' @author KVC November 2017
printLandShares <- function(aLandAllocator, aScenarioInfo, aNest) {
  # Silence package checks
  node <- parent <- uniqueJoinField <- year <-share <- NULL

  scentype <- aScenarioInfo$mScenarioType

  # Get a list of leafs
  nodes <- unique(aNest$parent)
  aNest %>%
    filter(node %!in% nodes) ->
    leafs

  # Some leafs have the same parent node name. We need to add those
  aNest %>%
    filter(parent == node) %>%
    bind_rows(leafs) ->
    leafs

  # Get data into a data frame
  tibble::tibble(parent = leafs$parent,
                 name = leafs$node,
                 share = rep(NA, length(leafs$node))) %>%
    mutate(uniqueJoinField = 1) %>%
    full_join(mutate(tibble(year = YEARS[[scentype]]), uniqueJoinField = 1),
              by = "uniqueJoinField") %>%
    select(-uniqueJoinField) ->
    allLandShares

  # Loop over all periods and get shares
  allLandShares <- LandAllocator_getLandShares(aLandAllocator, allLandShares,
                                               scentype)

  # Convert year to integer
  allLandShares %>%
    mutate(year = as.integer(year),
           share = as.numeric(share)) ->
    allLandShares

  # Write data to a file
  path <- normalizePath(aScenarioInfo$mOutputDir)
  file <- paste0(path,"/landShares.csv")
  write_csv(allLandShares, file)
}

#' LandAllocator_getLandShares
#'
#' @details Calculates and returns land allocation for a particular leaf
#' @param aLandAllocator LandAllocator
#' @param aShares Table of shares to append information to
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#'
#' @return Table of land shares
#' @author KVC November 2017
LandAllocator_getLandShares <- function(aLandAllocator, aShares, scentype) {

  for(child in aLandAllocator$mChildren) {
    if(class(child) == "LandNode") {
      aShares <- LandNode_getLandShares(child, aShares, scentype)
    } else {
      for(per in seq_along(child$mShare)) {
        currParent <- "root"
        currName <- child$mName[1]
        currYear <- get_per_to_yr(per, scentype)
        aShares$share[aShares$year == currYear &
                        aShares$name == currName &
                        aShares$parent == currParent] <- child$mShare[[per]]
      }
    }
  }

  return(aShares)
}

#' LandNode_getLandShares
#'
#' @details Calculates and returns land share for all children.
#' @param aLandNode LandNode
#' @param aShares Table of shares to add to
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#'
#' @return Table of land shares
#' @author KVC November 2017
LandNode_getLandShares <- function(aLandNode, aShares, scentype) {
  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      aShares <- LandNode_getLandShares(child, aShares, scentype)
    } else {
      for(per in seq_along(child$mShare)) {
        currParent <- aLandNode$mName[1]
        currName <- child$mName[1]
        currYear <- get_per_to_yr(per, scentype)
        aShares$share[aShares$year == currYear &
                        aShares$name == currName &
                        aShares$parent == currParent] <- child$mShare[[per]]
      }
    }
  }

  return(aShares)
}

#' printNest
#'
#' @details Prints the land allocator nest, which can
#'          be used to plot tree structure or to query
#'          leaf data
#' @param aLandAllocator Land allocator
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @return Table of land node nestings, constructed by \code{\link{getNest}}.
#' @importFrom readr write_csv
#' @author KVC October 2017
printNest <- function(aLandAllocator, aScenarioInfo) {
  nest <- getNest(aLandAllocator)

  # Write to file
  path <- normalizePath(aScenarioInfo$mOutputDir)
  file <- paste0(path, "/landNest.csv")
  write_csv(nest, file)

  # Return the current nest
  return(nest)
}

#' Construct a table of land node nestings
#'
#' The table describes the graph of node relationships.  There is one row for
#' each edge in the graph.  The columns are "parent" and "node", and they
#' name the parent and child in the edge.
#'
#' @param aLandAllocator Land allocator structure
getNest <- function(aLandAllocator)
{
    tibble::tibble(parent = "TEMP",
                   node = "TEMP") -> nest

    # Map out nest
    nest <- LandAllocator_addToNest(aLandAllocator, nest)

    # Remove temporary link
    nest[nest$parent != "TEMP",]
}

#' LandAllocator_addToNest
#'
#' @param aLandAllocator Land allocator
#' @param aNest Current nest
#' @details Determine all of the parent/child relationships
#'          in the land allocator.
#' @return Updated nest
#' @author KVC October 2017
LandAllocator_addToNest <- function(aLandAllocator, aNest) {
  nest <- aNest

  # Loop over all of the children, adding the link and calling the child's LandNode_addToNest
  for(child in aLandAllocator$mChildren) {
    tibble::tibble(parent = "root",
                   node = child$mName) %>%
      bind_rows(nest) ->
      nest

    # Now, call addToNest on each of the child nodes
    # Note: we don't need to call this on children that are leafs
    if (class(child) == "LandNode") {
      nest <- LandNode_addToNest(child, nest)
    }
  }

  return(nest)
}

#' LandNode_addToNest
#'
#' @param aLandNode Land node
#' @param aNest Current nest
#' @details Determine all of the parent/child relationships
#'          for this particular node.
#' @return Updated nest
#' @author KVC October 2017
LandNode_addToNest <- function(aLandNode, aNest) {

  nest <- aNest

  for (child in aLandNode$mChildren) {
    tibble::tibble(parent = aLandNode$mName[[1]],
                   node = child$mName[[1]]) %>%
      bind_rows(nest) ->
      nest

    # Now, call addToNest on each of the child nodes
    # Note: we don't need to call this on children that are leafs
    if (class(child) == "LandNode") {
      nest <- LandNode_addToNest(child, nest)
    }
  }

  return(nest)
}
