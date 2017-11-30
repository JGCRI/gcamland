# reporting.R

# Note: this file serves the function of `xml_db_outputter` in GCAM4.3

#' printLandAllocation
#'
#' @details Prints all outputs
#' @param aLandAllocator Land allocator
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @author KVC October 2017
printOutput <- function(aLandAllocator, aScenarioInfo) {
  printNest(aLandAllocator)
  printLandAllocation(aLandAllocator, aScenarioInfo)
  printLandShares(aLandAllocator)
  printYield(aLandAllocator, aScenarioInfo)
  printExpectedYield(aLandAllocator, aScenarioInfo)
  printExpectedPrices(aScenarioInfo)
}

#' printExpectedPrices
#'
#' @details Print expected prices
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @author KVC November 2017
printExpectedPrices <- function(aScenarioInfo) {
  EXPECTED_PRICES %>%
    mutate(scenario = aScenarioInfo$mScenarioName) ->
    expectedPrices

  file <- paste("./outputs/expectedPrice/expectedPrice_", aScenarioInfo$mFileName, ".csv", sep="")

  write_csv(expectedPrices, file)
}

#' printLandAllocation
#'
#' @details Prints land allocation by land leaf
#' @param aLandAllocator Land allocator
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @importFrom readr write_csv read_csv
#' @author KVC October 2017
printLandAllocation <- function(aLandAllocator, aScenarioInfo) {
  # Silence package checks
  node <- parent <- uniqueJoinField <- NULL

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
  tibble::tibble(name = leafs$node,
                 land.allocation = rep(NA, length(leafs$node))) %>%
    mutate(uniqueJoinField = 1) %>%
    full_join(mutate(tibble(year = YEARS), uniqueJoinField = 1), by = "uniqueJoinField") %>%
    select(-uniqueJoinField) ->
    allLand

  allLand <- LandAllocator_getLandAllocation(aLandAllocator, allLand)

  # Add information on scenario and expectation type
  allLand$scenario <- aScenarioInfo$mScenarioName

  file <- paste("./outputs/land/landAllocation_", aScenarioInfo$mFileName, ".csv", sep="")
  write_csv(allLand, file)

}

#' LandAllocator_getLandAllocation
#'
#' @details Calculates and returns land allocation for a particular leaf
#' @param aLandAllocator LandAllocator
#' @param allLand Data frame to fill in land allocation
#'
#' @return Land allocation table
#' @author KVC October 2017
LandAllocator_getLandAllocation <- function(aLandAllocator, allLand) {
  for(child in aLandAllocator$mChildren) {
    if(class(child) == "LandNode") {
      allLand <- LandNode_getLandAllocation(child, allLand)
    } else {
      allLand <- LandLeaf_getLandAllocation(child, allLand)
    }
  }

  return(allLand)
}

#' LandNode_getLandAllocation
#'
#' @details Calculates and returns total land allocation for types and periods
#' @param aLandNode LandNode
#' @param allLand Data frame to fill in land allocation
#'
#' @return Land allocation table
#' @author KVC October 2017
LandNode_getLandAllocation <- function(aLandNode, allLand) {

  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      allLand <- LandNode_getLandAllocation(child, allLand)
    } else {
      allLand <- LandLeaf_getLandAllocation(child, allLand)
    }
  }

  return(allLand)
}

#' LandLeaf_getLandAllocation
#'
#' @details Calculates and returns total land allocation for types and periods
#' @param aLandLeaf LandLeaf
#' @param allLand Data frame to fill in land allocation
#'
#' @return Land allocation table
#' @author KVC October 2017
LandLeaf_getLandAllocation <- function(aLandLeaf, allLand) {

  for(per in PERIODS) {
    currName <- aLandLeaf$mName[1]
    currYear <- get_per_to_yr(per)
    allLand$land.allocation[allLand$year == currYear &
                              allLand$name == currName] <- aLandLeaf$mLandAllocation[[per]]
  }

  return(allLand)
}

#' printLandShares
#'
#' @details Prints land share for all nodes and leafs
#' @param aLandAllocator Land allocator
#' @importFrom readr write_csv read_csv
#' @author KVC November 2017
printLandShares <- function(aLandAllocator) {
  # Silence package checks
  node <- parent <- uniqueJoinField <- year <- NULL

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
  tibble::tibble(parent = leafs$parent,
                 name = leafs$node,
                 share = rep(NA, length(leafs$node))) %>%
    mutate(uniqueJoinField = 1) %>%
    full_join(mutate(tibble(year = YEARS), uniqueJoinField = 1), by = "uniqueJoinField") %>%
    select(-uniqueJoinField) ->
    allLandShares

  # Loop over all periods and get shares
  allLandShares <- LandAllocator_getLandShares(aLandAllocator, allLandShares)

  # Convert year to integer
  allLandShares %>%
    mutate(year = as.integer(year),
           share = as.numeric(share)) ->
    allLandShares

  # Write data to a file
  write_csv(allLandShares, "./outputs/landShares.csv")

}

#' LandAllocator_getLandShares
#'
#' @details Calculates and returns land allocation for a particular leaf
#' @param aLandAllocator LandAllocator
#' @param aShares Table of shares to append information to
#'
#' @return Table of land shares
#' @author KVC November 2017
LandAllocator_getLandShares <- function(aLandAllocator, aShares) {

  for(child in aLandAllocator$mChildren) {
    if(class(child) == "LandNode") {
      aShares <- LandNode_getLandShares(child, aShares)
    } else {
      for(per in PERIODS) {
        currParent <- "root"
        currName <- child$mName[1]
        currYear <- get_per_to_yr(per)
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
#'
#' @return Table of land shares
#' @author KVC November 2017
LandNode_getLandShares <- function(aLandNode, aShares) {
  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      aShares <- LandNode_getLandShares(child, aShares)
    } else {
      for(per in PERIODS) {
        currParent <- aLandNode$mName[1]
        currName <- child$mName[1]
        currYear <- get_per_to_yr(per)
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
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @importFrom readr write_csv read_csv
#' @author KVC October 2017
printYield <- function(aLandAllocator, aScenarioInfo) {
  # Silence package checks
  node <- parent <- uniqueJoinField <- NULL

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
  allYield$scenario <- aScenarioInfo$mScenarioName

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

#' printExpectedYield
#'
#' @details Prints expected yield by land leaf
#' @param aLandAllocator Land allocator
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @importFrom readr write_csv read_csv
#' @author KVC October 2017
printExpectedYield <- function(aLandAllocator, aScenarioInfo) {
  # Silence package checks
  node <- parent <- uniqueJoinField <- NULL

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
                 expectedYield = rep(NA, length(leafs$node))) %>%
    mutate(uniqueJoinField = 1) %>%
    full_join(mutate(tibble(year = YEARS), uniqueJoinField = 1), by = "uniqueJoinField") %>%
    select(-uniqueJoinField) ->
    allYield

  i <- 1
  for(leaf in leafs$node) {
    for (per in PERIODS) {
      allYield$name[i] <- leaf
      allYield$year[i] <- get_per_to_yr(per)
      allYield$expectedYield[i] <- LandAllocator_getExpectedYield(aLandAllocator, leaf, per)
      i <- i + 1
    }
  }

  # Add information on scenario and expectation type
  allYield$scenario <- aScenarioInfo$mScenarioName

  # Save information
  file <- paste("./outputs/expectedYield/expectedYield_", aScenarioInfo$mFileName, ".csv", sep="")
  write_csv(allYield, file)
}

#' LandAllocator_getExpectedYield
#'
#' @details Calculates and returns expected yield for a particular leaf
#' @param aLandAllocator LandAllocator
#' @param aName Name of leaf we want expected yield for
#' @param aPeriod Model period
#'
#' @return Expected yield
#' @author KVC October 2017
LandAllocator_getExpectedYield <- function(aLandAllocator, aName, aPeriod) {
  yield <- 0.0
  for(child in aLandAllocator$mChildren) {
    if(class(child) == "LandNode") {
      yield <- yield + LandNode_getExpectedYield(child, aName, aPeriod)
    } else if(class(child) == "LandLeaf") {
      if(child$mName[1] == aName) {
        yield <- yield + child$mExpectedYield[[aPeriod]]
      }
    }
  }

  return(yield)
}

#' LandNode_getExpectedYield
#'
#' @details Calculates and returns expected yield for a given leaf
#' @param aLandNode LandNode
#' @param aName Name of leaf we want expected yield for
#' @param aPeriod Model period
#'
#' @return Expected yield for leaf named `aName`
#' @author KVC October 2017
LandNode_getExpectedYield <- function(aLandNode, aName, aPeriod) {
  yield <- 0.0
  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      yield <- yield + LandNode_getExpectedYield(child, aName, aPeriod)
    } else if(class(child) == "LandLeaf") {
      if(child$mName[1] == aName) {
        yield <- yield + child$mExpectedYield[[aPeriod]]
      }
    }
  }

  return(yield)
}

