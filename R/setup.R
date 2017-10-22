# setup.R

#' LandAllocator_setup
#'
#' @details Setup the land allocator. This includes
#'          Reading in external data and organizing
#'          it in the node, leaf, technology structure
#' @param aLandAllocator LandAllocator that needs set up
#' @author KVC October 2017
LandAllocator_setup <- function(aLandAllocator) {
  # Read in top-level information and save total land
  data <- ReadData_LN0(aLandAllocator$mRegionName)
  aLandAllocator$mLandAllocation <- data[[c("landAllocation")]]

  # Read information on LN1 nodes (children of land allocator)
  children.data <- ReadData_LN1_Node(aLandAllocator$mRegionName)
  LN1_setup(aLandAllocator, aLandAllocator$mRegionName, children.data, "LandNode1")

  # Read information on leaf children of LN1 nodes
  children.data <- ReadData_LN1_LeafChildren(aLandAllocator$mRegionName)
  Leaf_setup(aLandAllocator, aLandAllocator$mRegionName, children.data, "UnmanagedLandLeaf")

  # Read information on LN2 nodes
  children.data <- ReadData_LN2_Node(aLandAllocator$mRegionName)
  LandNode_setup(aLandAllocator, aLandAllocator$mRegionName, children.data, "LandNode2")

  # Read information on LandLeaf children of LN2 nodes
  children.data <- ReadData_LN2_LandLeaf(aLandAllocator$mRegionName)
  Leaf_setup(aLandAllocator, aLandAllocator$mRegionName, children.data, "LandLeaf")

  # Read information on UnmanagedLandLeaf children of LN2 nodes
  children.data <- ReadData_LN2_UnmanagedLandLeaf(aLandAllocator$mRegionName)
  Leaf_setup(aLandAllocator, aLandAllocator$mRegionName, children.data, "UnmanagedLandLeaf")

  # Read information on LN3 nodes
  children.data <- ReadData_LN3_Node(aLandAllocator$mRegionName)
  LandNode_setup(aLandAllocator, aLandAllocator$mRegionName, children.data, "LandNode3")

  # Read information on LandLeaf children of LN3 nodes
  children.data <- ReadData_LN3_LandLeaf(aLandAllocator$mRegionName)
  Leaf_setup(aLandAllocator, aLandAllocator$mRegionName, children.data, "LandLeaf")

  # Read information on UnmanagedLandLeaf children of LN2 nodes
  children.data <- ReadData_LN3_UnmanagedLandLeaf(aLandAllocator$mRegionName)
  Leaf_setup(aLandAllocator, aLandAllocator$mRegionName, children.data, "UnmanagedLandLeaf")

  plot_Nest(aLandAllocator)

}

#' LN1_setup
#'
#' @details Setup LN1 of the land allocator. This includes
#'          reading in external data, creating LandNode1 nodes, and
#'          creating any leafs with only one parent node.
#' @param aLandAllocator LandAllocator that needs set up
#' @author KVC October 2017
LN1_setup <- function(aLandAllocator, aRegionName, data, col.name) {

  # Create each child
  i <- 1
  for(child.name in unique(data[[col.name]])){
    # Get data for the node
    data %>%
      mutate_( col.name=as.name( col.name )) %>%
      filter(col.name == child.name) ->
      temp

    name <- temp[[col.name]]
    exponent <- temp[[c("logit.exponent")]]
    choiceFunction <- ChoiceFunction("relative-cost", exponent)

    # Create the node
    newNode <- LandNode(name, choiceFunction, -1)
    newNode$mUnmanagedLandValue <- temp[[c("unManagedLandValue")]]

    # Add node to the list
    aLandAllocator$mChildren[[i]] <- newNode

    # Increment child counter
    i <- i + 1
  }

}


#' LandNode_setup
#'
#' @details Setup a LandNode level of the land allocator. This includes
#'          reading in external data, creating LandNode* nodes.
#' @param aLandAllocator LandAllocator that needs set up
#' @author KVC October 2017
LandNode_setup <- function(aLandAllocator, aRegionName, data, col.name) {
  # Create each child node
  for(child.name in unique(data[[col.name]])){
    # Get data for the node
    data %>%
      mutate_(col.name = as.name(col.name)) %>%
      filter(col.name == child.name) ->
      temp

    name <- temp[[col.name]]
    exponent <- temp[[c("logit.exponent")]]
    choiceFunction <- ChoiceFunction("relative-cost", exponent)

    # Create the node
    newNode <- LandNode(name, choiceFunction, -1)

    # Get the names of the land nodes that are parents to this node
    temp %>%
      select(-region, -LandAllocatorRoot, -year.fillout, -logit.exponent, -col.name) %>%
      distinct() ->
      parent.names

    # Remove the node name from the parent list.
    # TODO: find a more elegant way of doing this
    num.cols <- ncol(parent.names) - 1
    parent.names <- parent.names[1:num.cols]

    # Now, add the leaf to the node allocator
    LandAllocator_addChild(aLandAllocator, newNode, parent.names)

  }
}

#' Leaf_setup
#'
#' @details Setup land leafs for the land allocator. This includes
#'          searching for the right parent, creating and creating leafs.
#' @param aLandAllocator LandAllocator that needs set up
#' @author KVC October 2017
Leaf_setup <- function(aLandAllocator, aRegionName, data, col.name) {
  # Loop over all children in the data set
  for(child.name in unique(data[[col.name]])){
    # Get data for the leaf
    data %>%
      mutate_(col.name = as.name(col.name)) %>%
      filter(col.name == child.name) ->
      temp

    if (col.name == "UnmanagedLandLeaf") {
      # Create an UnmanagedLandLeaf
      newLeaf <- UnmanagedLandLeaf(temp[[col.name]])

      # Get the names of the land nodes that are parents to this leaf
      temp %>%
        select(-region, -LandAllocatorRoot, -UnmanagedLandLeaf, -allocation, -year, -col.name) %>%
        distinct() ->
        parent.names
    } else if (col.name == "LandLeaf") {
      newLeaf <- LandLeaf(temp[[col.name]])

      # Get the names of the land nodes that are parents to this leaf
      temp %>%
        select(-region, -LandAllocatorRoot, -LandLeaf, -allocation, -year, -col.name) %>%
        distinct() ->
        parent.names
    }

    # loop over years
    for(y in YEARS) {
      temp %>%
        filter(year == y) ->
        curr

      # Get period
      per <- get_yr_to_per(y)

      # Save land allocation
      newLeaf$mLandAllocation[[per]] <- curr[[c("allocation")]]
    }

    # Now, add the leaf to the land allocator
    LandAllocator_addChild(aLandAllocator, newLeaf, parent.names)
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
