# reporting.R

# Note: this file serves the function of `xml_db_outputter` in GCAM4.3

#' printLandAllocation
#'
#' @details Prints all outputs
#' @param aLandAllocator Land allocator
#' @author KVC October 2017
printOutput <- function(aLandAllocator) {
  printNest(aLandAllocator)
  printLandAllocation(aLandAllocator)
}


#' printLandAllocation
#'
#' @details Prints land allocation by land leaf
#' @param aLandAllocator Land allocator
#' @importFrom readr write_csv read_csv
#' @author KVC October 2017
printLandAllocation <- function(aLandAllocator) {
  # Read nest
  nest <- suppressMessages(read_csv("./outputs/landNest.csv"))

  # Get a list of leafs
  nodes <- unique(nest$parent)
  nest %>%
    filter(node %!in% nodes) ->
    leafs

  # Some leafs have the same parent node name. We need to add those
  nest %>%
    filter(parent == node) %>%
    bind_rows(leafs) ->
    leafs

  # Get data into a data frame
  tibble::tibble(name = rep(NA, length(leafs$node)),
                 land.allocation = rep(NA, length(leafs$node))) %>%
    repeat_add_columns(tibble::tibble(year = YEARS)) ->
    allLand

  i <- 1
  for(leaf in leafs$node) {
    for (per in PERIODS) {
      allLand$name[i] <- leaf
      allLand$year[i] <- get_per_to_yr(per)
      allLand$land.allocation[i] <- LandAllocator_getLandAllocation(aLandAllocator, leaf, per)
      i <- i + 1
    }
  }

  write_csv(allLand, "./outputs/landAllocation.csv")

}

#' LandAllocator_getLandAllocation
#'
#' @details Calculates and returns land allocation for a particular leaf
#' @param aLandAllocator LandAllocator
#' @param aName Name of leaf we want allocation for
#' @param aPeriod Model period
#'
#' @return Land allocation
#' @author KVC October 2017
LandAllocator_getLandAllocation <- function(aLandAllocator, aName, aPeriod) {
  land <- 0.0
  for(child in aLandAllocator$mChildren) {
    if(class(child) == "LandNode") {
      land <- land + LandNode_getLandAllocation(child, aName, aPeriod)
    } else {
      if(child$mName[1] == aName) {
        land <- land + child$mLandAllocation[[aPeriod]]
      }
    }
  }

  return(land)
}

#' LandNode_getLandAllocation
#'
#' @details Calculates and returns total land allocation of a given type.
#' @param aLandNode LandNode
#' @param aName Name of leaf we want allocation for
#' @param aPeriod Model period
#'
#' @return Land allocation for this node
#' @author KVC October 2017
LandNode_getLandAllocation <- function(aLandNode, aName, aPeriod) {
  land <- 0.0
  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      land <- land + LandNode_getLandAllocation(child, aName, aPeriod)
    } else {
      if(child$mName[1] == aName) {
        land <- land + child$mLandAllocation[[aPeriod]]
      }
    }
  }

  return(land)
}


#' printNest
#'
#' @details Prints the land allocator nest, which can
#'          be used to plot tree structure or to query
#'          leaf data
#' @param aLandAllocator Land allocator
#' @importFrom readr write_csv
#' @author KVC October 2017
printNest <- function(aLandAllocator) {
  # Silence package checks
  parent <- node <- NULL

  tibble::tibble(parent = "TEMP",
                 node = "TEMP") -> nest

  # Map out nest
  nest <- LandAllocator_addToNest(aLandAllocator, nest)

  # Remove temporary link
  nest %>%
    filter(parent != "TEMP") ->
    nest

  write_csv(nest, "./outputs/landNest.csv")
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

  # TODO: Make this loop over children once there are multiple
  for(child in aLandAllocator$mChildren) {
    tibble::tibble(parent = "root",
                   node = child$mName) %>%
      bind_rows(nest) ->
      nest

    # Now, call addToNest on each of the children
    nest <- LandNode_addToNest(child, nest)
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
