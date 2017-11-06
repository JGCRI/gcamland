# setup.R

#' LandAllocator_setup
#'
#' @details Setup the land allocator. This includes
#'          Reading in external data and organizing
#'          it in the node, leaf, technology structure
#' @param aLandAllocator LandAllocator that needs set up
#' @author KVC October 2017
LandAllocator_setup <- function(aLandAllocator) {
  print("Initializing LandAllocator")

  # Read ag data -- we'll use this for all leafs
  agData <- ReadData_AgProd(aLandAllocator$mRegionName)

  # Read in top-level information and save total land
  data <- ReadData_LN0(aLandAllocator$mRegionName)
  aLandAllocator$mLandAllocation <- as.numeric(data[[c("landAllocation")]])

  # Read information on LN1 nodes (children of land allocator)
  childrenData <- ReadData_LN1_Node(aLandAllocator$mRegionName)
  LN1_setup(aLandAllocator, aLandAllocator$mRegionName, childrenData, "LandNode1")

  # Read information on leaf children of LN1 nodes
  childrenData <- ReadData_LN1_LeafChildren(aLandAllocator$mRegionName)
  Leaf_setup(aLandAllocator, aLandAllocator$mRegionName, childrenData, "UnmanagedLandLeaf")

  # Read information on LN2 nodes
  childrenData <- ReadData_LN2_Node(aLandAllocator$mRegionName)
  LandNode_setup(aLandAllocator, aLandAllocator$mRegionName, childrenData, "LandNode2")

  # Read information on LandLeaf children of LN2 nodes
  childrenData <- ReadData_LN2_LandLeaf(aLandAllocator$mRegionName)
  Leaf_setup(aLandAllocator, aLandAllocator$mRegionName, childrenData, "LandLeaf", agData)

  # Read information on UnmanagedLandLeaf children of LN2 nodes
  childrenData <- ReadData_LN2_UnmanagedLandLeaf(aLandAllocator$mRegionName)
  Leaf_setup(aLandAllocator, aLandAllocator$mRegionName, childrenData, "UnmanagedLandLeaf")

  # Read information on LN3 nodes
  childrenData <- ReadData_LN3_Node(aLandAllocator$mRegionName)
  LandNode_setup(aLandAllocator, aLandAllocator$mRegionName, childrenData, "LandNode3")

  # Read information on LandLeaf children of LN3 nodes
  childrenData <- ReadData_LN3_LandLeaf(aLandAllocator$mRegionName)
  Leaf_setup(aLandAllocator, aLandAllocator$mRegionName, childrenData, "LandLeaf", agData)

  # Read information on UnmanagedLandLeaf children of LN2 nodes
  childrenData <- ReadData_LN3_UnmanagedLandLeaf(aLandAllocator$mRegionName)
  Leaf_setup(aLandAllocator, aLandAllocator$mRegionName, childrenData, "UnmanagedLandLeaf")
}

#' LN1_setup
#'
#' @details Setup LN1 of the land allocator. This includes
#'          reading in external data, creating LandNode1 nodes, and
#'          creating any leafs with only one parent node.
#' @param aLandAllocator LandAllocator that needs set up
#' @param aRegionName Region
#' @param aData Data needed for setting up this node
#' @param aColName Column name with the parent
#' @importFrom dplyr mutate_
#' @author KVC October 2017
LN1_setup <- function(aLandAllocator, aRegionName, aData, aColName) {

  # Create each child
  i <- 1
  for(childName in unique(aData[[aColName]])){
    # Get data for the node
    aData %>%
      mutate_(aColName=as.name(aColName)) %>%
      filter(aColName == childName) ->
      temp

    # Save values for arguments that need to be passed to the constructor
    name <- temp[[aColName]]
    exponent <- as.numeric(temp[[c("logit.exponent")]])
    choiceFunction <- ChoiceFunction("relative-cost", exponent)

    # Create the node
    newNode <- LandNode(name, choiceFunction, -1)
    newNode$mUnmanagedLandValue <- as.numeric(temp[[c("unManagedLandValue")]])

    # Add node to the list
    aLandAllocator$mChildren[[i]] <- newNode

    # Increment child counter
    i <- i + 1
  }
}

#' LandNode_setup
#'
#' @details Setup a LandNode level of the land allocator. This includes
#'          reading in external data, creating LandNode nodes.
#' @param aLandAllocator LandAllocator that needs set up
#' @param aRegionName Region
#' @param aData Data needed for setting up this node
#' @param aColumnName Column name with the parent
#' @importFrom dplyr distinct
#' @author KVC October 2017
LandNode_setup <- function(aLandAllocator, aRegionName, aData, aColumnName) {
  # Silence package checks
  region <- LandAllocatorRoot <- year.fillout <- logit.exponent <- NULL

  # Create each child node
  for(childName in unique(aData[[aColumnName]])){
    # Get data for the node
    aData %>%
      mutate_(aColumnName = as.name(aColumnName)) %>%
      filter(aColumnName == childName) ->
      temp

    # Save values for arguments that need to be passed to the constructor
    name <- temp[[aColumnName]]
    exponent <- as.numeric(temp[[c("logit.exponent")]])
    choiceFunction <- ChoiceFunction("relative-cost", exponent)

    # Create the node
    newNode <- LandNode(name, choiceFunction, -1)

    # Get the names of the land nodes that are parents to this node
    temp %>%
      select(-region, -LandAllocatorRoot, -year.fillout, -logit.exponent, -aColumnName) %>%
      distinct() ->
      parentNames

    # Remove the node name from the parent list.
    # TODO: find a more elegant way of doing this
    numCols <- ncol(parentNames) - 1
    parentNames <- parentNames[1:numCols]

    # Now, add the leaf to the node allocator
    LandAllocator_addChild(aLandAllocator, newNode, parentNames)
  }
}

#' Leaf_setup
#'
#' @details Setup land leafs for the land allocator. This includes
#'          searching for the right parent, creating and creating leafs.
#' @param aLandAllocator LandAllocator that needs set up
#' @param aRegionName Region
#' @param aData Data needed for setting up this node
#' @param aColName Column name with the parent
#' @param aAgData Agricultural technology data
#' @author KVC October 2017
Leaf_setup <- function(aLandAllocator, aRegionName, aData, aColName, aAgData = NULL) {
  region <- LandAllocatorRoot <- allocation <- year <- NULL

  # Loop over all children in the data set
  for(child.name in unique(aData[[aColName]])){
    # Get data for the leaf
    aData %>%
      mutate_(aColName = as.name(aColName)) %>%
      filter(aColName == child.name) ->
      temp

    if(aColName == "UnmanagedLandLeaf") {
      # Create an UnmanagedLandLeaf
      newLeaf <- UnmanagedLandLeaf(temp[[aColName]])

      # Get the names of the land nodes that are parents to this leaf
      temp %>%
        select(-region, -LandAllocatorRoot, -UnmanagedLandLeaf, -allocation, -year, -aColName) %>%
        distinct() ->
        parentNames

    } else if (aColName == "LandLeaf") {
      newLeaf <- LandLeaf(temp[[aColName]])

      # Get the names of the land nodes that are parents to this leaf
      temp %>%
        select(-region, -LandAllocatorRoot, -LandLeaf, -allocation, -year, -aColName) %>%
        distinct() ->
        parentNames

      # Read-in yield, cost, tech change
      AgProductionTechnology_setup(newLeaf, aAgData)
    }

    # Loop over years and add allocation
    for(y in YEARS) {
      # Filter for current year
      temp %>%
        filter(year == y) ->
        curr

      # Get period
      per <- get_yr_to_per(y)

      # Save land allocation
      if(per <= FINAL_CALIBRATION_PERIOD) {
        newLeaf$mCalLandAllocation[per] <- as.numeric(curr[[c("allocation")]])
      }
    }

    # Now, add the leaf to the land allocator
    LandAllocator_addChild(aLandAllocator, newLeaf, parentNames)
  }
}

#' LandAllocator_addChild
#'
#' @details Insert a land leaf into the land allocator
#'          Loop through all of the land allocator's children
#'          until we find the parent. Then, add the leaf.
#' @param aLandAllocator Land allocator
#' @param aChild Child (Land node or leaf) to insert
#' @param aParentNames Names of the parent nodes
#' @author KVC October 2017
LandAllocator_addChild <- function(aLandAllocator, aChild, aParentNames) {
  for(child in aLandAllocator$mChildren) {
    if(child$mName[1] == aParentNames[[1]]) {
      # Check to see whether this is the parent.
      if(ncol(aParentNames) == 1){
        # If this is the parent, then add to list of children.
        nextChild <- length(child$mChildren) + 1
        child$mChildren[[nextChild]] <- aChild
      } else{
        # Call add leaf on children
        LandNode_addChild(child, aChild, aParentNames[2:ncol(aParentNames)])
      }
    }
  }
}

#' LandNode_addChild
#'
#' @details Insert a land leaf or node into the land allocator
#'          Loop through all of this land node's children
#'          until we find the parent. Then, add the leaf or node.
#' @param aLandNode Current land node
#' @param aChild Child (land node or leaf) to insert
#' @param aParentNames Names of the parent nodes
#' @author KVC October 2017
LandNode_addChild <- function(aLandNode, aChild, aParentNames) {
  for(child in aLandNode$mChildren) {
    if(child$mName == aParentNames[[1]]) {
      # Check to see whether this is the parent.
      if(ncol(aParentNames) == 1){
        # If this is the parent, then add to list of children.
        nextChild <- length(child$mChildren) + 1
        child$mChildren[[nextChild]] <- aChild
      } else{
        # Call add leaf on children
        LandNode_addChild(child, aChild, aParentNames[2:ncol(aParentNames)])
      }
    }
  }
}

#' AgProductionTechnology_setup
#'
#' @details Setup technology (e.g., CalOutput, technical change, cost, etc.)
#' @param aLandLeaf Land leaf
#' @param aAgData Agricultural technology data
#' @author KVC October 2017
AgProductionTechnology_setup <- function(aLandLeaf, aAgData) {
  # Silence package checks
  per <- year <- AgProductionTechnology <- GCAM_commodity <- NULL

  # Separate data
  calOutput <- aAgData[[1]]
  agProdChange <- aAgData[[2]]
  cost <- aAgData[[3]]

  # Get name of leaf
  name <- aLandLeaf$mName[[1]]

  # Set product name
  # TODO: Find a better way to do this -- it will need updating when we go to irr/mgmt
  aLandLeaf$mProductName <- substr(aLandLeaf$mName[[1]], 1, nchar(aLandLeaf$mName[[1]]) - 5)
  if(aLandLeaf$mProductName %in% BIOMASS_TYPES) {
    aLandLeaf$mProductName <- "biomass"
  }

  # Loop through all periods and read in data
  for(y in YEARS) {
    per <- get_yr_to_per(y)

    # Only read in mCalOutput data for calibration periods
    if(per <= FINAL_CALIBRATION_PERIOD) {
      # Filter for this period/leaf combination
      calOutput %>%
        filter(year == y, AgProductionTechnology == name) ->
        currCalOutput

      # Set calOutput and agProdChange
      if(nrow(currCalOutput)) {
        aLandLeaf$mCalOutput[per] <- as.numeric(currCalOutput[[c("calOutputValue")]])
      } else {
        # TODO: Do we need to flag missing or is it okay to set to -1?
        aLandLeaf$mCalOutput[per] <- -1
      }

      # Set data that shouldn't exist in the past to 0
      aLandLeaf$mAgProdChange[per] <- 0
      aLandLeaf$mNonLandCostTechChange[per] <- 0
    } else{
      # Only read in technical change information for future periods
      if(SCENARIO == "Hindcast") {
        # We only have AgProdChange at region level for historical data
        agProdChange %>%
          filter(year == y, GCAM_commodity == aLandLeaf$mProductName[1]) ->
          currAgProdChange
      } else {
        agProdChange %>%
          filter(year == y, AgProductionTechnology == name) ->
          currAgProdChange
      }

      if(nrow(currAgProdChange)) {
        aLandLeaf$mAgProdChange[per] <- as.numeric(currAgProdChange[[c("AgProdChange")]])
      } else{
        aLandLeaf$mAgProdChange[per] <- 0.0
      }

      # Set data that shouldn't exist in the future to -1
      aLandLeaf$mCalOutput[[per]] <- -1

      # Set data that we aren't going to read in to 0
      # Note: we are including this parameter because it is in the C++ code, but GCAM doesn't use it
      aLandLeaf$mNonLandCostTechChange[[per]] <- 0
    }

    # Filter cost for current year
    cost %>%
      filter(year == y, AgProductionTechnology == name) ->
      currCost

    # Set cost in the LandLeaf
    if(nrow(currCost)) {
      aLandLeaf$mCost[[per]] <- as.numeric(currCost[[c("nonLandVariableCost")]])
    } else {
      aLandLeaf$mCost[[per]] <- 0
    }
  }
}
