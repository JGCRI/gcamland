# reporting.R

# Note: this file serves the function of `xml_db_outputter` in GCAM4.3

#' printLandAllocation
#'
#' @details Prints all outputs
#' @param aLandAllocator Land allocator
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @author KVC October 2017
printOutput <- function(aLandAllocator, aScenarioInfo) {
  nest <- printNest(aLandAllocator, aScenarioInfo)
  printLandAllocation(aLandAllocator, aScenarioInfo, nest)
  printLandShares(aLandAllocator, aScenarioInfo, nest)
  printYield(aLandAllocator, aScenarioInfo, nest)
  printExpectedYield(aLandAllocator, aScenarioInfo, nest)
  printExpectedPrice(aLandAllocator, aScenarioInfo, nest)
  printPrices(aScenarioInfo)
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

#' printLandAllocation
#'
#' @details Prints land allocation by land leaf
#' @param aLandAllocator Land allocator
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @param aNest Nest to fill in
#' @importFrom readr write_csv read_csv
#' @author KVC October 2017
printLandAllocation <- function(aLandAllocator, aScenarioInfo, aNest) {
  # Silence package checks
  node <- parent <- uniqueJoinField <- NULL

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
  tibble::tibble(name = leafs$node,
                 land.allocation = rep(NA, length(leafs$node))) %>%
    mutate(uniqueJoinField = 1) %>%
    full_join(mutate(tibble(year = YEARS[[scentype]]), uniqueJoinField = 1),
              by = "uniqueJoinField") %>%
    select(-uniqueJoinField) ->
    allLand

  allLand <- LandAllocator_getLandAllocation(aLandAllocator, allLand, scentype)

  # Add information on scenario and expectation type
  allLand$scenario <- aScenarioInfo$mScenarioName

  path <- normalizePath(paste0(aScenarioInfo$mOutputDir, "/land/"))
  file <- paste0(path, "/landAllocation_", aScenarioInfo$mFileName, ".csv")
  write_csv(allLand, file)
}

#' LandAllocator_getLandAllocation
#'
#' @details Calculates and returns land allocation for a particular leaf
#' @param aLandAllocator LandAllocator
#' @param allLand Data frame to fill in land allocation
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#'
#' @return Land allocation table
#' @author KVC October 2017
LandAllocator_getLandAllocation <- function(aLandAllocator, allLand, scentype) {
  for(child in aLandAllocator$mChildren) {
    if(class(child) == "LandNode") {
      allLand <- LandNode_getLandAllocation(child, allLand, scentype)
    } else {
      allLand <- LandLeaf_getLandAllocation(child, allLand, scentype)
    }
  }

  return(allLand)
}

#' LandNode_getLandAllocation
#'
#' @details Calculates and returns total land allocation for types and periods
#' @param aLandNode LandNode
#' @param allLand Data frame to fill in land allocation
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#'
#' @return Land allocation table
#' @author KVC October 2017
LandNode_getLandAllocation <- function(aLandNode, allLand, scentype) {

  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      allLand <- LandNode_getLandAllocation(child, allLand, scentype)
    } else {
      allLand <- LandLeaf_getLandAllocation(child, allLand, scentype)
    }
  }

  return(allLand)
}

#' LandLeaf_getLandAllocation
#'
#' @details Calculates and returns total land allocation for types and periods
#' @param aLandLeaf LandLeaf
#' @param allLand Data frame to fill in land allocation
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#'
#' @return Land allocation table
#' @author KVC October 2017
LandLeaf_getLandAllocation <- function(aLandLeaf, allLand, scentype) {

  for(per in seq_along(aLandLeaf$mLandAllocation)) {
    currName <- aLandLeaf$mName[1]
    currYear <- get_per_to_yr(per, scentype)
    allLand$land.allocation[allLand$year == currYear &
                              allLand$name == currName] <- aLandLeaf$mLandAllocation[[per]]
  }

  return(allLand)
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
#' @return Nest structure
#' @importFrom readr write_csv
#' @author KVC October 2017
printNest <- function(aLandAllocator, aScenarioInfo) {
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

  # Write to file
  path <- normalizePath(aScenarioInfo$mOutputDir)
  file <- paste0(path, "/landNest.csv")
  write_csv(nest, file)

  # Return the current nest
  return(nest)
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
#' @param aNest Land allocator nest
#' @importFrom readr write_csv read_csv
#' @author KVC October 2017
printYield <- function(aLandAllocator, aScenarioInfo, aNest) {
  # Silence package checks
  node <- parent <- uniqueJoinField <- NULL

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
  tibble::tibble(name = leafs$node,
                 yield = rep(NA, length(leafs$node))) %>%
    mutate(uniqueJoinField = 1) %>%
    full_join(mutate(tibble(year = YEARS[[scentype]]), uniqueJoinField = 1),
              by = "uniqueJoinField") %>%
    select(-uniqueJoinField) ->
    allYield

  allYield <- LandAllocator_getYield(aLandAllocator, allYield, scentype)

  # Add information on scenario and expectation type
  allYield$scenario <- aScenarioInfo$mScenarioName

  # Write to file
  path <- normalizePath(aScenarioInfo$mOutputDir)
  file <- paste0(path, "/yield.csv")
  write_csv(allYield, file)
}

#' LandAllocator_getYield
#'
#' @details Calculates and returns yield for all leafs
#' @param aLandAllocator LandAllocator
#' @param aData Table to store data in
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#'
#' @return Yield for all leafs
#' @author KVC October 2017
LandAllocator_getYield <- function(aLandAllocator, aData, scentype) {
  for(child in aLandAllocator$mChildren) {
    if(class(child) == "LandNode") {
      aData <- LandNode_getYield(child, aData, scentype)
    } else if(class(child) == "LandLeaf") {
      aData <- LandLeaf_getYield(child, aData, scentype)
    }
  }

  return(aData)
}

#' LandNode_getYield
#'
#' @details Calculates and returns yield for all leafs in a node
#' @param aLandNode LandNode
#' @param aData Table to store data in
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#'
#' @return Yield for all leafs in the node
#' @author KVC October 2017
LandNode_getYield <- function(aLandNode, aData, scentype) {
  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      aData <- LandNode_getYield(child, aData, scentype)
    } else if(class(child) == "LandLeaf") {
      aData <- LandLeaf_getYield(child, aData, scentype)
    }
  }

  return(aData)
}

#' LandLeaf_getYield
#'
#' @details Calculates and returns yield for this leaf
#' @param aLandLeaf LandLeaf
#' @param aData Table to store data in
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#'
#' @return Yield for all leafs in the node
#' @author KVC October 2017
LandLeaf_getYield <- function(aLandLeaf, aData, scentype) {
  for(per in seq_along(aLandLeaf$mYield)) {
    currName <- aLandLeaf$mName[1]
    currYear <- get_per_to_yr(per, scentype)
    aData$yield[aData$name == currName & aData$year == currYear] <- aLandLeaf$mYield[[per]]
  }

  return(aData)
}

#' printExpectedYield
#'
#' @details Prints expected yield by land leaf
#' @param aLandAllocator Land allocator
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @param aNest Land allocator nest
#' @importFrom readr write_csv read_csv
#' @author KVC October 2017
printExpectedYield <- function(aLandAllocator, aScenarioInfo, aNest) {
  # Silence package checks
  node <- parent <- uniqueJoinField <- NULL

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
  tibble::tibble(name = leafs$node,
                 expectedYield = rep(NA, length(leafs$node))) %>%
    mutate(uniqueJoinField = 1) %>%
    full_join(mutate(tibble(year = YEARS[[scentype]]), uniqueJoinField = 1),
              by = "uniqueJoinField") %>%
    select(-uniqueJoinField) ->
    allYield

  allYield <- LandAllocator_getExpectedYield(aLandAllocator, allYield, scentype)

  # Add information on scenario and expectation type
  allYield$scenario <- aScenarioInfo$mScenarioName

  # Write to file
  path <- normalizePath(paste0(aScenarioInfo$mOutputDir, "/expectedYield/"))
  file <- paste0(path, "/expectedYield_", aScenarioInfo$mFileName, ".csv")
  write_csv(allYield, file)
}

#' LandAllocator_getExpectedYield
#'
#' @details Calculates and returns expected yield for all leafs
#' @param aLandAllocator LandAllocator
#' @param aData Data table to store expected yield
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#'
#' @return Expected yield data
#' @author KVC October 2017
LandAllocator_getExpectedYield <- function(aLandAllocator, aData, scentype) {
  for(child in aLandAllocator$mChildren) {
    if(class(child) == "LandNode") {
      aData <- LandNode_getExpectedYield(child, aData, scentype)
    } else if(class(child) == "LandLeaf") {
      aData <- LandLeaf_getExpectedYield(child, aData, scentype)
    }
  }

  return(aData)
}

#' LandNode_getExpectedYield
#'
#' @details Calculates and returns expected yield for leafs in this node
#' @param aLandNode LandNode
#' @param aData Data table to store expected yield
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#'
#' @return Expected yield data
#' @author KVC October 2017
LandNode_getExpectedYield <- function(aLandNode, aData, scentype) {
  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      aData <- LandNode_getExpectedYield(child, aData, scentype)
    } else if(class(child) == "LandLeaf") {
      aData <- LandLeaf_getExpectedYield(child, aData, scentype)
    }
  }

  return(aData)
}

#' LandLeaf_getExpectedYield
#'
#' @details Calculates and returns expected yield for this leaf
#' @param aLandLeaf LandLeaf
#' @param aData Table to store data in
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#'
#' @return Expected yield data
#' @author KVC October 2017
LandLeaf_getExpectedYield <- function(aLandLeaf, aData, scentype) {
  for(per in seq_along(aLandLeaf$mExpectedYield)) {
    currName <- aLandLeaf$mName[1]
    currYear <- get_per_to_yr(per, scentype)
    aData$expectedYield[aData$name == currName & aData$year == currYear] <- aLandLeaf$mExpectedYield[[per]]
  }

  return(aData)
}


#' printExpectedPrice
#'
#' @details Prints expected price by land leaf
#' @param aLandAllocator Land allocator
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @param aNest Land allocator nest
#' @importFrom readr write_csv read_csv
#' @author KVC October 2017
printExpectedPrice <- function(aLandAllocator, aScenarioInfo, aNest) {
  # Silence package checks
  node <- parent <- uniqueJoinField <- NULL

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
  tibble::tibble(name = leafs$node,
                 expectedPrice = rep(NA, length(leafs$node))) %>%
    mutate(uniqueJoinField = 1) %>%
    full_join(mutate(tibble(year = YEARS[[scentype]]), uniqueJoinField = 1),
              by = "uniqueJoinField") %>%
    select(-uniqueJoinField) ->
    allPrice

  allPrice <- LandAllocator_getExpectedPrice(aLandAllocator, allPrice, scentype)

  # Add information on scenario and expectation type
  allPrice$scenario <- aScenarioInfo$mScenarioName

  # Write to file
  path <- normalizePath(paste0(aScenarioInfo$mOutputDir, "/expectedPrice/"))
  file <- paste0(path, "/expectedPrice_", aScenarioInfo$mFileName, ".csv")
  write_csv(allPrice, file)
}

#' LandAllocator_getExpectedPrice
#'
#' @details Calculates and returns expected price for all leafs
#' @param aLandAllocator LandAllocator
#' @param aData Data table to store expected price
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#'
#' @return Expected price data
#' @author KVC October 2017
LandAllocator_getExpectedPrice <- function(aLandAllocator, aData, scentype) {
  for(child in aLandAllocator$mChildren) {
    if(class(child) == "LandNode") {
      aData <- LandNode_getExpectedPrice(child, aData, scentype)
    } else if(class(child) == "LandLeaf") {
      aData <- LandLeaf_getExpectedPrice(child, aData, scentype)
    }
  }

  return(aData)
}

#' LandNode_getExpectedPrice
#'
#' @details Calculates and returns expected price for leafs in this node
#' @param aLandNode LandNode
#' @param aData Data table to store expected price
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#'
#' @return Expected price data
#' @author KVC November 2017
LandNode_getExpectedPrice <- function(aLandNode, aData, scentype) {
  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      aData <- LandNode_getExpectedPrice(child, aData, scentype)
    } else if(class(child) == "LandLeaf") {
      aData <- LandLeaf_getExpectedPrice(child, aData, scentype)
    }
  }

  return(aData)
}

#' LandLeaf_getExpectedPrice
#'
#' @details Calculates and returns expected price for this leaf
#' @param aLandLeaf LandLeaf
#' @param aData Table to store data in
#' @param scentype Scenario type: either "Reference" or "Hindcast"
#'
#' @return Expected price data
#' @author KVC November 2017
LandLeaf_getExpectedPrice <- function(aLandLeaf, aData, scentype) {
  for(per in seq_along(aLandLeaf$mExpectedPrice)) {
    currName <- aLandLeaf$mName[1]
    currYear <- get_per_to_yr(per, scentype)
    aData$expectedPrice[aData$name == currName & aData$year == currYear] <- aLandLeaf$mExpectedPrice[[per]]
  }

  return(aData)
}
