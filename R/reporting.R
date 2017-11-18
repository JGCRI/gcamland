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
  printLandShares(aLandAllocator)
  printYield(aLandAllocator)
}


#' printLandAllocation
#'
#' @details Prints land allocation by land leaf
#' @param aLandAllocator Land allocator
#' @importFrom readr write_csv read_csv
#' @author KVC October 2017
printLandAllocation <- function(aLandAllocator) {
  # Silence package checks
  node <- parent <- NULL

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
    mutate(uniqueJoinField = 1) %>%
    full_join(mutate(tibble(year = YEARS), uniqueJoinField = 1), by = "uniqueJoinField") %>%
    select(-uniqueJoinField) ->
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

  # Add information on scenario and expectation type
  allLand$scenario <- SCENARIO
  if(EXPECTATION.TYPE == "Linear") {
    expectations <- paste(EXPECTATION.TYPE, LINEAR.YEARS, sep="")

  } else {
    expectations <- EXPECTATION.TYPE
  }
  allLand$expectations <- expectations

  file <- paste("./outputs/landAllocation_", SCENARIO, "_", expectations, ".csv", sep="")
  write_csv(allLand, file)

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

#' printLandShares
#'
#' @details Prints land share for all nodes and leafs
#' @param aLandAllocator Land allocator
#' @importFrom readr write_csv read_csv
#' @author KVC November 2017
printLandShares <- function(aLandAllocator) {
  # Silence package checks
  node <- parent <- NULL

  # Set up a data frame
  tibble::tibble(parent = "TEMP",
                 name = "TEMP",
                 share = NA) %>%
    mutate(uniqueJoinField = 1) %>%
    full_join(mutate(tibble(year = YEARS), uniqueJoinField = 1), by = "uniqueJoinField") %>%
    select(-uniqueJoinField) ->
    allLandShares

  # Loop over all periods and get shares
  for ( per in PERIODS ) {
    allLandShares <- LandAllocator_getLandShares(aLandAllocator, allLandShares, per)
  }

  # Remove temporary data & convert year to integer
  allLandShares %>%
    filter(parent != "TEMP") %>%
    mutate(year = as.integer(year)) ->
    allLandShares

  # Write data to a file
  write_csv(allLandShares, "./outputs/landShares.csv")

}

#' LandAllocator_getLandShares
#'
#' @details Calculates and returns land allocation for a particular leaf
#' @param aLandAllocator LandAllocator
#' @param aShares Table of shares to append information to
#' @param aPeriod Model period
#'
#' @return Table of land shares
#' @author KVC November 2017
LandAllocator_getLandShares <- function(aLandAllocator, aShares, aPeriod) {

  for(child in aLandAllocator$mChildren) {
    if(class(child) == "LandNode") {
      aShares <- LandNode_getLandShares(child, aShares, aPeriod)
    } else {
      TEMP <- tibble::tibble(parent = "root",
                             name = child$mName[1],
                             share = child$mShare[[aPeriod]],
                             year = get_per_to_yr(aPeriod))
      aShares %>%
        bind_rows(TEMP) ->
        aShares
    }
  }

  return(aShares)
}

#' LandNode_getLandShares
#'
#' @details Calculates and returns land share for all children.
#' @param aLandNode LandNode
#' @param aShares Table of shares to add to
#' @param aPeriod Model period
#'
#' @return Table of land shares
#' @author KVC November 2017
LandNode_getLandShares <- function(aLandNode, aShares, aPeriod) {
  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      aShares <- LandNode_getLandShares(child, aShares, aPeriod)
    } else {
      TEMP <- tibble::tibble(parent = aLandNode$mName[1],
                             name = child$mName[1],
                             share = child$mShare[[aPeriod]],
                             year = get_per_to_yr(aPeriod))
      aShares %>%
        bind_rows(TEMP) ->
        aShares
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

#' printYield
#'
#' @details Prints yield by land leaf
#' @param aLandAllocator Land allocator
#' @importFrom readr write_csv read_csv
#' @author KVC October 2017
printYield <- function(aLandAllocator) {
  # Silence package checks
  node <- parent <- NULL

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
                 yield = rep(NA, length(leafs$node))) %>%
    mutate(uniqueJoinField = 1) %>%
    full_join(mutate(tibble(year = YEARS), uniqueJoinField = 1), by = "uniqueJoinField") %>%
    select(-uniqueJoinField) ->
    allYield

  i <- 1
  for(leaf in leafs$node) {
    for (per in PERIODS) {
      allYield$name[i] <- leaf
      allYield$year[i] <- get_per_to_yr(per)
      allYield$yield[i] <- LandAllocator_getYield(aLandAllocator, leaf, per)
      i <- i + 1
    }
  }

  # Add information on scenario and expectation type
  allYield$scenario <- SCENARIO
  if(EXPECTATION.TYPE == "Linear") {
    expectations <- paste(EXPECTATION.TYPE, LINEAR.YEARS, sep="")

  } else {
    expectations <- EXPECTATION.TYPE
  }
  allYield$expectations <- expectations

  write_csv(allYield, "./outputs/yield.csv")

}

#' LandAllocator_getYield
#'
#' @details Calculates and returns yield for a particular leaf
#' @param aLandAllocator LandAllocator
#' @param aName Name of leaf we want yield for
#' @param aPeriod Model period
#'
#' @return Yield
#' @author KVC October 2017
LandAllocator_getYield <- function(aLandAllocator, aName, aPeriod) {
  yield <- 0.0
  for(child in aLandAllocator$mChildren) {
    if(class(child) == "LandNode") {
      yield <- yield + LandNode_getYield(child, aName, aPeriod)
    } else if(class(child) == "LandLeaf") {
      if(child$mName[1] == aName) {
        yield <- yield + child$mYield[[aPeriod]]
      }
    }
  }

  return(yield)
}

#' LandNode_getYield
#'
#' @details Calculates and returns yield for a given leaf
#' @param aLandNode LandNode
#' @param aName Name of leaf we want yield for
#' @param aPeriod Model period
#'
#' @return Yield for leaf named `aName`
#' @author KVC October 2017
LandNode_getYield <- function(aLandNode, aName, aPeriod) {
  yield <- 0.0
  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      yield <- yield + LandNode_getYield(child, aName, aPeriod)
    } else if(class(child) == "LandLeaf") {
      if(child$mName[1] == aName) {
        yield <- yield + child$mYield[[aPeriod]]
      }
    }
  }

  return(yield)
}

