# setup.R

#' LandAllocator_setup
#'
#' @details Setup the land allocator. This includes
#'          Reading in external data and organizing
#'          it in the node, leaf, technology structure
#' @param aLandAllocator LandAllocator that needs set up
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @param agData Ag data read by \code{\link{ReadData_AgProd}}.  The data must
#' be for the same region and scenario type as the \code{aScenarioInfo} object.
#' @author KVC October 2017
#' @export
LandAllocator_setup <- function(aLandAllocator, aScenarioInfo, agData=NULL) {
  message("Initializing LandAllocator")

  scentype <- aScenarioInfo$mScenarioType

  # Read SubRegion data
  if(!is.null(aLandAllocator$mSubRegion)){
    message("Running a Subregional Model")
    # Read in calibration data
    subregionData <- suppressMessages(read_csv(system.file("extdata", "./initialization-data/LandUse_Nesting_SRB.csv", package = "gcamland")))
  } else {
    subregionData <- NULL
  }

  # Read ag data -- we'll use this for all leafs, including bioenergy
  if(!is.null(agData)) {
    ## Validate data, if supplied
    assert_that(has_attr(agData, 'rgn'))
    assert_that(attr(agData, 'rgn') == aScenarioInfo$mRegion)
    assert_that(has_attr(agData, 'scentype'))
    assert_that(attr(agData, 'scentype') == scentype)
  }
  else {
    ## Read data if not supplied.
    agData <- ReadData_AgProd(aLandAllocator$mRegionName, scentype, aLandAllocator$mSubRegion, subregionData)
  }

  # Read in top-level information and save total land
  data <- ReadData_LN0(aLandAllocator$mRegionName, aLandAllocator$mSubRegion, subregionData)
  aLandAllocator$mLandAllocation <- as.numeric(data[[c("landAllocation")]])

  # Read information on LN1 nodes (children of land allocator)
  childrenData <- ReadData_LN1_Node(aLandAllocator$mRegionName, aLandAllocator$mSubRegion, subregionData)
  LN1_setup(aLandAllocator, aLandAllocator$mRegionName, childrenData,
            "LandNode1", aScenarioInfo)

  # Read information on leaf children of LN1 nodes
  childrenData <- ReadData_LN1_LeafChildren(aLandAllocator$mRegionName, aLandAllocator$mSubRegion, subregionData, aScenarioInfo)
  Leaf_setup(aLandAllocator, aLandAllocator$mRegionName, childrenData,
             "UnmanagedLandLeaf", aScenarioInfo)

  # Read information on LN2 nodes
  childrenData <- ReadData_LN2_Node(aLandAllocator$mRegionName, aLandAllocator$mSubRegion, subregionData)
  LandNode_setup(aLandAllocator, aLandAllocator$mRegionName, childrenData,
                 "LandNode2", aScenarioInfo)

  # Read information on LandLeaf children of LN2 nodes
  childrenData <- ReadData_LN2_LandLeaf(aLandAllocator$mRegionName, aLandAllocator$mSubRegion, subregionData, aScenarioInfo)
  Leaf_setup(aLandAllocator, aLandAllocator$mRegionName, childrenData,
             "LandLeaf", aScenarioInfo, agData)

  # Read information on UnmanagedLandLeaf children of LN2 nodes
  childrenData <- ReadData_LN2_UnmanagedLandLeaf(aLandAllocator$mRegionName, aLandAllocator$mSubRegion, subregionData, aScenarioInfo)
  Leaf_setup(aLandAllocator, aLandAllocator$mRegionName, childrenData,
             "UnmanagedLandLeaf", aScenarioInfo)

  # Read information on LN3 nodes
  childrenData <- ReadData_LN3_Node(aLandAllocator$mRegionName, aLandAllocator$mSubRegion, subregionData)
  ghostShareData <- ReadData_LN3_GhostShare(aLandAllocator$mRegionName)
  LandNode_setup(aLandAllocator, aLandAllocator$mRegionName, childrenData,
                 "LandNode3", aScenarioInfo, ghostShareData)

  # Read information on LandLeaf children of LN3 nodes
  childrenData <- ReadData_LN3_LandLeaf(aLandAllocator$mRegionName, aLandAllocator$mSubRegion, subregionData, aScenarioInfo)
  newTechData <- ReadData_LN3_NewTech(aLandAllocator$mRegionName)
  Leaf_setup(aLandAllocator, aLandAllocator$mRegionName, childrenData,
             "LandLeaf", aScenarioInfo, agData, newTechData)

  # Read information on UnmanagedLandLeaf children of LN3 nodes
  childrenData <- ReadData_LN3_UnmanagedLandLeaf(aLandAllocator$mRegionName, aLandAllocator$mSubRegion, subregionData, aScenarioInfo)
  Leaf_setup(aLandAllocator, aLandAllocator$mRegionName, childrenData,
             "UnmanagedLandLeaf", aScenarioInfo)
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
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @importFrom dplyr mutate_
#' @author KVC October 2017
LN1_setup <- function(aLandAllocator, aRegionName, aData, aColName, aScenarioInfo) {

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
    if ( !aScenarioInfo$mLogitUseDefault & grepl("AgroForestLand", childName)) {
      exponent <- aScenarioInfo$mLogitAgroForest
    } else {
      exponent <- as.numeric(temp[[c("logit.exponent")]])
    }

    choiceFunction <- ChoiceFunction("relative-cost", exponent)

    # Create the node
    finalcalper <- TIME.PARAMS[[aScenarioInfo$mScenarioType]]$FINAL_CALIBRATION_PERIOD
    newNode <- LandNode(name, choiceFunction, -1, finalcalper)
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
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @param aGhostShareData Information on ghost shares
#' @importFrom dplyr distinct
#' @author KVC October 2017
LandNode_setup <- function(aLandAllocator, aRegionName, aData, aColumnName, aScenarioInfo, aGhostShareData = NULL) {
  # Silence package checks
  region <- LandAllocatorRoot <- year.fillout <- logit.exponent <- year <- NULL

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
    if ( !aScenarioInfo$mLogitUseDefault & grepl("AgroForest_NonPasture", childName)) {
      exponent <- aScenarioInfo$mLogitAgroForest_NonPasture
    } else if ( !aScenarioInfo$mLogitUseDefault & grepl("CropLand", childName) ) {
      exponent <- aScenarioInfo$mLogitCropland
    } else {
      exponent <- as.numeric(temp[[c("logit.exponent")]])
    }
    choiceFunction <- ChoiceFunction("relative-cost", exponent)

    # Create the node
    finalCalPer <- TIME.PARAMS[[aScenarioInfo$mScenarioType]]$FINAL_CALIBRATION_PERIOD
    newNode <- LandNode(name, choiceFunction, -1, finalCalPer)

    # If ghost share data exists, update it here
    if(!is.null(aGhostShareData)) {
      aGhostShareData %>%
        mutate_(aColumnName = as.name(aColumnName)) %>%
        filter(aColumnName == childName) ->
        tempGhostShare

      if(nrow(tempGhostShare)) {
        # Loop over years and add ghost share
        for(y in YEARS[[aScenarioInfo$mScenarioType]]) {
          # Get period
          per <- get_yr_to_per(y, aScenarioInfo$mScenarioType)

          # Save ghost share
          if(per <= finalCalPer) {
            # We only want ghost shares in future periods
            newNode$mGhostUnnormalizedShare[per] <- 0.0
          } else if(y %in% tempGhostShare$year) {
            newNode$mGhostUnnormalizedShare[per] <- as.numeric(tempGhostShare$default.share[tempGhostShare$year == y])
          } else {
            # If nothing is read in, set ghost share to zero
            newNode$mGhostUnnormalizedShare[per] <- 0.0
          }
        }
      }
    }

    # Get the names of the land nodes that are parents to this node
    parentNames <- temp[ , names(temp) %!in% c("region", "subregion", "LandAllocatorRoot", "year.fillout",
                                               "logit.exponent", "aColumnName")]

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
#' @param aScenarioInfo ScenarioInfo structure containing the scenario parameters
#' @param aAgData Agricultural technology data
#' @param newTechData Data about any new technologies (i.e., technologies without calibration information)
#' @author KVC October 2017
#' @export
Leaf_setup <- function(aLandAllocator, aRegionName, aData, aColName,
                       aScenarioInfo, aAgData = NULL, newTechData = NULL) {
  region <- LandAllocatorRoot <- allocation <- year <- NULL

  finalCalPer <-
    TIME.PARAMS[[aScenarioInfo$mScenarioType]]$FINAL_CALIBRATION_PERIOD

  finalPer <- max(PERIODS[[aScenarioInfo$mScenarioType]])

  # Set up column name used for filtering
  aData %>%
    mutate_(aColName = as.name(aColName)) ->
    allData

  # Loop over all children in the data set
  for(childName in unique(aData[[aColName]])){
    # Get data for the leaf
    temp <- subset(allData, aColName == childName)
    temp <- temp[ , names(temp) %!in% c("region", "subregion", "LandAllocatorRoot", "aColName")]

    if(aColName == "UnmanagedLandLeaf") {
      # Create an UnmanagedLandLeaf
      newLeaf <- UnmanagedLandLeaf(temp[[aColName]], finalCalPer)

      # Get the names of the land nodes that are parents to this leaf
      parentNames <- temp[ , names(temp) %!in% c("UnmanagedLandLeaf", "allocation", "year")]
      parentNames <- unique(parentNames)

    } else if (aColName == "LandLeaf") {
      newLeaf <- LandLeaf(temp[[aColName]], finalCalPer, finalPer)
      # Check whether leaf is a new technology
      if(!is.null(newTechData)) {
        if(childName %in% newTechData$LandLeaf) {
          newLeaf$mIsNewTech <- TRUE
        }
      }

      # Get the names of the land nodes that are parents to this leaf
      parentNames <- temp[ , names(temp) %!in% c("LandLeaf", "allocation", "year")]
      parentNames <- unique(parentNames)

      # Read-in yield, cost, tech change
      AgProductionTechnology_setup(newLeaf, aAgData, aScenarioInfo)
    }

    # Loop over years and add allocation
    for(y in YEARS[[aScenarioInfo$mScenarioType]]) {
      # Get period
      per <- get_yr_to_per(y, aScenarioInfo$mScenarioType)

      # Save land allocation
      if(per <= finalCalPer) {
        newLeaf$mCalLandAllocation[per] <- as.numeric(temp$allocation[temp$year == y])
      }
    }

    # Read in share weights, if applicable
    if(aScenarioInfo$mCalibrateShareWt == FALSE) {
      newLeaf$mShareWeight <- aScenarioInfo$mShareWeights[childName]
      assertthat::assert_that(!is.na(newLeaf$mShareWeight), msg=paste('Share weight for', childName, 'not found.'))
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
#' @param aScenarioInfo Scenario info object
#' @author KVC October 2017
AgProductionTechnology_setup <- function(aLandLeaf, aAgData, aScenarioInfo) {
  # Silence package checks
  per <- year <- AgProductionTechnology <- GCAM_commodity <- NULL

  # Separate data
  calOutput <- aAgData[[1]]
  agProdChange <- aAgData[[2]]
  cost <- aAgData[[3]]
  bioYield <- aAgData[[4]]
  HAtoCL <- aAgData[[5]]

  # Get name of leaf
  name <- aLandLeaf$mName[[1]]

  # Set product name
  # TODO: Find a better way to do this -- it will need updating when we go to irr/mgmt
  aLandLeaf$mProductName <- substr(aLandLeaf$mName[[1]], 1, nchar(aLandLeaf$mName[[1]]) - 5)
  if(aLandLeaf$mProductName %in% BIOMASS_TYPES) {
    aLandLeaf$mProductName <- "biomass"
  }

  if(aScenarioInfo$mScenarioType == "Hindcast") {
    # We only have AgProdChange at region level for historical data
    agProdChange <- filter(agProdChange, GCAM_commodity == aLandLeaf$mProductName[1])
  } else {
    agProdChange <- filter(agProdChange, AgProductionTechnology == name)
  }

  # Loop through all periods and read in data
  for(y in YEARS[[aScenarioInfo$mScenarioType]]) {
    per <- get_yr_to_per(y, aScenarioInfo$mScenarioType)

    # Only read in mCalOutput data for calibration periods
    if(per <= TIME.PARAMS[[aScenarioInfo$mScenarioType]]$FINAL_CALIBRATION_PERIOD) {

      # Set calOutput
      if(y %in% calOutput$year & name %in% calOutput$AgProductionTechnology) {
        aLandLeaf$mCalOutput[per] <- as.numeric(calOutput$calOutputValue[calOutput$year == y &
                                                                           calOutput$AgProductionTechnology == name])
      } else {
        # TODO: Do we need to flag missing or is it okay to set to -1?
        aLandLeaf$mCalOutput[per] <- -1
      }

      # Get yield for bioenergy for this period combination
      if(aLandLeaf$mProductName == "biomass") {
        # Set bioYield
        if(y %in% bioYield$year & name %in% bioYield$AgProductionTechnology) {
          aLandLeaf$mYield[per] <- as.numeric(bioYield$yield[bioYield$year == y &
                                                               bioYield$AgProductionTechnology == name])
        } else {
          aLandLeaf$mYield[per] <- -1
        }
      }

      # Set data that shouldn't exist in the past to 0
      aLandLeaf$mAgProdChange[per] <- 0
      aLandLeaf$mNonLandCostTechChange[per] <- 0
    } else{
      # Only read in technical change information for future periods
      if(y %in% agProdChange$year) {
        aLandLeaf$mAgProdChange[per] <- as.numeric(agProdChange$AgProdChange[agProdChange$year == y])
      } else {
        aLandLeaf$mAgProdChange[per] <- 0.0
      }

      # Set data that shouldn't exist in the future to -1
      aLandLeaf$mCalOutput[[per]] <- -1

      # Set data that we aren't going to read in to 0
      # Note: we are including this parameter because it is in the C++ code, but GCAM doesn't use it
      aLandLeaf$mNonLandCostTechChange[[per]] <- 0
    }

    # Set cost in the LandLeaf
    if(aScenarioInfo$mUseZeroCost == FALSE &
       name %in% cost$AgProductionTechnology & y %in% cost$year)  {
      aLandLeaf$mCost[per] <- as.numeric(cost$nonLandVariableCost[cost$year == y &
                                                                    cost$AgProductionTechnology == name])
    } else {
      aLandLeaf$mCost[per] <- 0
    }

    # Set harvested to cropped ratio in the LandLeaf
    if(name %in% HAtoCL$AgProductionTechnology & y %in% HAtoCL$year)  {
      aLandLeaf$mHAtoCL[per] <- as.numeric(HAtoCL$harvests.per.year[HAtoCL$year == y &
                                                                        HAtoCL$AgProductionTechnology == name])
    } else if (name %in% HAtoCL$AgProductionTechnology & aScenarioInfo$mScenarioType == "Hindcast") {
      # Note we only have data for model periods defined in the reference. For Hindcasts, we should
      # use 1975 values for all periods.
      aLandLeaf$mHAtoCL[per] <- as.numeric(HAtoCL$harvests.per.year[HAtoCL$year == min(YEARS[[aScenarioInfo$mScenarioType]]) &
                                                                      HAtoCL$AgProductionTechnology == name])
    } else {
      aLandLeaf$mHAtoCL[per] <- 1
    }

  }
}

#' Required subdirectories for output directory
#' @keywords internal
REQD.SUBDIRS <- c()


#' Set up output directories
#'
#' Check the existence of the output directory (supplied as an argument) and its
#' required subdirectories (generated internally).  If any do not exist, create them.
#' Failure to create any of the required directories is a fatal error.
#'
#' @param outdir Name of the output directory
#' @return Normalized name of the output directory (invisibly)
#' @export
outdir_setup <- function(outdir)
{
  alldirs <- c(outdir, file.path(outdir, REQD.SUBDIRS))
  for(dir in alldirs[!dir.exists(alldirs)]) {
    dir.create(dir)
  }
  if(!all(dir.exists(alldirs))) {
    fail <- paste(alldirs[!dir.exists(alldirs)], collapse=", ")
    stop("Unable to create the following directories: ", fail)
  }

  invisible(normalizePath(outdir))
}
