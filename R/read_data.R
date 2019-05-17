# read_data.R

# Notes:
# File type: C++ GCAM uses xml input files that are parsed within each
#            individual source code file. For ease of use, we will
#            read the csv files that are used to create those xml files
#            as input into the R-based gcamland.
# Files: This file reads in a set of pre-specified file names based on
#        GCAM 4.3's data system. These file names are hard-coded.
# Regions: This file also filters data to include a subset of regions
#          if the user specifies. We do this here to reduce runtime
#          later, as smaller datasets are quicker to process.
# Nesting structure: we would like the nesting to be as dynamic as
#                    possible. For now, this is set up so that
#                    you only need to modify this file by adding
#                    more levels and to modify `LandAllocator_setup`
#                    to call the read functions on those levels.
#                    We have created dynamic functions to add nodes & leaves
#                    in the `setup.R` file.

#' ReadData_LN0
#'
#' @details Read in total land allocation and logit exponents
#'          for the LandAllocator.
#' @param aRegionName Region to read data for
#' @param SUBREGION Subregion to read data for
#' @param subregionData Subregion data
#' @return Land allocator data
#' @importFrom readr read_csv
#' @importFrom dplyr rename select group_by summarize
#' @author KVC October 2017
ReadData_LN0 <- function(aRegionName, SUBREGION=NULL, subregionData=NULL) {
  # Silence package checks
  region <- land <- year.fillout <- logit.year.fillout <- Root.logit.exponent <- landAllocation <-
    allocation <- subregion <- LandAllocatorRoot <- logit.exponent <- NULL

  if(!is.null(SUBREGION)){
    # For SUBREGION only keep select columns pertaining to root
    subregionData %>%
      select(region, subregion, LandAllocatorRoot, year.fillout, allocation, Root.logit.exponent) %>%
      rename(logit.exponent = Root.logit.exponent,
             landAllocation = allocation) ->
      data

    # Sum allocation and find unique logit exponent across region, root, and year
    data %>%
      group_by(region, subregion, LandAllocatorRoot, year.fillout) %>%
      summarize(landAllocation = sum(landAllocation),
                logit.exponent = unique(logit.exponent)) ->
      data
  }
  else {
    # Read in calibration data
    land <- suppressMessages(read_csv(system.file("extdata", "./initialization-data/L211.LN0_Land.csv", package = "gcamland"), skip = 3))
    logit <- suppressMessages(read_csv(system.file("extdata", "./initialization-data/L211.LN0_Logit.csv", package = "gcamland"), skip = 3))

    # Join all data into a single frame
    logit %>%
      rename(year.fillout = logit.year.fillout) %>%
      left_join(land, by=c("region", "LandAllocatorRoot", "year.fillout")) ->
      data

    # Filter data for the specified region
    data <- subset(data, region == aRegionName)
  }

  return(data)
}


#' ReadData_LN1_Node
#'
#' @details Read in unmanaged land value, names of children, and logit exponents
#'          for the LandAllocator.
#' @param aRegionName Region to read data for
#' @param SUBREGION Subregion to read data for
#' @param subregionData Subregion data
#' @return Data on children of the land allocator
#' @importFrom readr read_csv
#' @importFrom dplyr rename select group_by summarize
#' @author KVC October 2017
ReadData_LN1_Node <- function(aRegionName, SUBREGION=NULL, subregionData=NULL) {
  # Silence package checks
  region <- subregion <- LandAllocatorRoot <- LandNode1 <- year.fillout <- logit.exponent <-
    LandNode1.logit.exponent <- unManagedLandValue <- logit.year.fillout <- NULL

  if(!is.null(SUBREGION)){
    # For SUBREGION only keep select columns pertaining to LandNode1 and logit exponent for LandNode1
    subregionData %>%
      select(region, subregion, LandAllocatorRoot, LandNode1, year.fillout, LandNode1.logit.exponent, unManagedLandValue) %>%
      rename(logit.exponent = LandNode1.logit.exponent,
             logit.year.fillout = year.fillout) ->
      data

    # Find unique logit exponent values across region, root, LandNode1, and year
    data %>%
      group_by(region, subregion, LandAllocatorRoot, LandNode1, logit.year.fillout) %>%
      summarize(logit.exponent = unique(logit.exponent),
                unManagedLandValue = unique(unManagedLandValue)) ->
      data
  }
  else {
    # Read in calibration data
    data <- suppressMessages(read_csv(system.file("extdata", "./initialization-data/L211.LN1_ValueLogit.csv", package = "gcamland"), skip = 3))

    # Filter data for the specified region
    data <- subset(data, region == aRegionName)

    # Filter data for specified AEZ
    if(!is.null(AEZ)){
      data <- subset(data, grepl(AEZ, data$LandNode1))
    }
  }

  return(data)
}

#' ReadData_LN1_LeafChildren
#'
#' @details Read in the leaf children of LandNode1. That is
#'          read in information on children that only have one
#'          node above them.
#' @param aRegionName Region to read data for
#' @param SUBREGION Subregion to read data for
#' @param subregionData Subregion data
#' @return Data on children of the land allocator
#' @importFrom readr read_csv
#' @importFrom dplyr rename select group_by summarize
#' @author KVC October 2017
ReadData_LN1_LeafChildren <- function(aRegionName, SUBREGION=NULL, subregionData=NULL) {
  # Silence package checks
  region <- LandNode1 <- year.fillout <- subregion <- LandAllocatorRoot <- UnmanagedLandLeaf <- allocation <- year <- NULL

  if(!is.null(SUBREGION)){
    # For SUBREGION only keep rows for which LandNode2 is NA
    # That is read in information on children that only have one node above them.
    data <- subregionData[is.na(subregionData$LandNode2),]

    # Only keep select columns pertaining to LandNode1
    data %>%
      select(region, subregion, LandAllocatorRoot, LandNode1, UnmanagedLandLeaf, year.fillout, allocation) %>%
      rename(year = year.fillout) ->
      data

    # Sum allocation across region, root, LandNode1, and year
    data %>%
      group_by(region, subregion, LandAllocatorRoot, LandNode1, UnmanagedLandLeaf, year) %>%
      summarize(allocation = sum(allocation)) ->
      data
  }
  else {
    # Read in calibration data
    data <- suppressMessages(read_csv(system.file("extdata", "./initialization-data/L211.LN1_UnmgdAllocation.csv", package = "gcamland"), skip = 3))

    # Filter data for the specified region
    data <- subset(data, region == aRegionName)

    # Filter data for specified AEZ
    if(!is.null(AEZ)){
      data <- subset(data, grepl(AEZ, data$LandNode1))
    }
  }

  return(data)
}

#' ReadData_LN2_Node
#'
#' @details Read in names of children and logit exponents for LN2
#' @param aRegionName Region to read data for
#' @param SUBREGION Subregion to read data for
#' @param subregionData Subregion data
#' @return Data on level 2 nodes of the land allocator
#' @importFrom readr read_csv
#' @importFrom dplyr rename select group_by summarize
#' @author KVC October 2017
ReadData_LN2_Node <- function(aRegionName, SUBREGION=NULL, subregionData=NULL) {
  # Silence package checks
  region <- LandNode1 <- year.fillout <- logit.year.fillout <- logit.exponent <-
    subregion <- LandAllocatorRoot <- LandNode2 <- LandNode2.logit.exponent <- NULL

  if(!is.null(SUBREGION)){
    # For SUBREGION only keep rows for which LandNode2 is not NA and LandNode3 is NA
    # That is read in information on children that have two node above them.
    data <- subregionData[!is.na(subregionData$LandNode2),]

    # Only keep select columns pertaining to LandNode2 and logit exponent for LandNode2
    data %>%
      select(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, year.fillout, LandNode2.logit.exponent) %>%
      rename(logit.exponent = LandNode2.logit.exponent) ->
      data

    # Find unique logit exponent values across region, root, LandNode1, LandNode2, and year
    data %>%
      group_by(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, year.fillout) %>%
      summarize(logit.exponent = unique(logit.exponent)) ->
      data
  }
  else {
    # Read in calibration data
    data <- suppressMessages(read_csv(system.file("extdata", "./initialization-data/L212.LN2_Logit.csv", package = "gcamland"), skip = 3))

    # Filter data for the specified region
    data <- rename(data, year.fillout = logit.year.fillout)
    data <- subset(data, region == aRegionName)

    # Filter data for specified AEZ
    if(!is.null(AEZ)){
      data <- subset(data, grepl(AEZ, LandNode1))
    }
  }

  return(data)
}

#' ReadData_LN2_LandLeaf
#'
#' @details Read in names of information on LandLeafs that are
#'          children of LandNode2 nodes
#' @param aRegionName Region to read data for
#' @param SUBREGION Subregion to read data for
#' @param subregionData Subregion data
#' @return Data on LandLeaf children of LandNode2
#' @importFrom readr read_csv
#' @importFrom dplyr rename select group_by summarize
#' @author KVC October 2017
ReadData_LN2_LandLeaf <- function(aRegionName, SUBREGION=NULL, subregionData=NULL) {
  # Silence package checks
  region <- subregion <- LandAllocatorRoot <- LandNode1 <- LandNode2 <-
    LandLeaf <- year.fillout <- allocation <- year <- NULL

  if(!is.null(SUBREGION)){
    # For SUBREGION only keep rows for which LandNode2 is not NA and LandNode3 is NA
    # That is read in information on children that have two node above them.
    data <- subregionData[!is.na(subregionData$LandNode2) & is.na(subregionData$LandNode3) & !is.na(subregionData$LandLeaf),]

    # Only keep select columns pertaining to LandNode2
    data %>%
      select(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, LandLeaf, year.fillout, allocation) %>%
      rename(year = year.fillout) ->
      data

    # Sum allocation across region, root, LandNode1, LandNode2, LandLeaf, and year
    data %>%
      group_by(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, LandLeaf, year) %>%
      summarize(allocation = sum(allocation)) ->
      data
  }
  else {
    # Read in calibration data
    data <- suppressMessages(read_csv(system.file("extdata", "./initialization-data/L212.LN2_MgdAllocation.csv", package = "gcamland"), skip = 3))

    # Filter data for the specified region
    data <- subset(data, region == aRegionName)

    # Filter data for specified AEZ
    if(!is.null(AEZ)){
      data <- subset(data, grepl(AEZ, LandNode1))
    }
  }

  return(data)
}

#' ReadData_LN2_UnmanagedLandLeaf
#'
#' @details Read in names of information on UnmanagedLandLeafs that are
#'          children of LandNode2 nodes
#' @param aRegionName Region to read data for
#' @param SUBREGION Subregion to read data for
#' @param subregionData Subregion data
#' @return Data on UnmanagedLandLeaf children of LandNode2
#' @importFrom readr read_csv
#' @importFrom dplyr rename select group_by summarize
#' @author KVC October 2017
ReadData_LN2_UnmanagedLandLeaf <- function(aRegionName, SUBREGION=NULL, subregionData=NULL) {
  # Silence package checks
  region <- subregion <- LandAllocatorRoot <- LandNode1 <- LandNode2 <-
    UnmanagedLandLeaf <- year.fillout <- allocation <- year <- NULL

  if(!is.null(SUBREGION)){
    # For SUBREGION only keep rows for which LandNode2 is not NA and LandNode3 is NA
    # That is read in information on children that have two node above them.
    data <- subregionData[!is.na(subregionData$LandNode2) & is.na(subregionData$LandNode3) & !is.na(subregionData$UnmanagedLandLeaf),]

    # Only keep select columns pertaining to LandNode2
    data %>%
      select(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, UnmanagedLandLeaf, year.fillout, allocation) %>%
      rename(year = year.fillout) ->
      data

    # Sum allocation across region, root, LandNode1, LandNode2, UnmanagedLandLeaf, and year
    data %>%
      group_by(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, UnmanagedLandLeaf, year) %>%
      summarize(allocation = sum(allocation)) ->
      data
  }
  else {
    # Read in calibration data
    data <- suppressMessages(read_csv(system.file("extdata", "./initialization-data/L212.LN2_UnmgdAllocation.csv", package = "gcamland"), skip = 3))

    # Filter data for the specified region
    data <- filter(data, region == aRegionName)

    # Filter data for specified AEZ
    if(!is.null(AEZ)){
      data <- filter(data, grepl(AEZ, LandNode1))
    }
  }

  return(data)
}

#' ReadData_LN3_Node
#'
#' @details Read in names of children and logit exponents for LN2
#' @param aRegionName Region to read data for
#' @param SUBREGION Subregion to read data for
#' @param subregionData Subregion data
#' @return Data on level 3 nodes of the land allocator
#' @importFrom readr read_csv
#' @importFrom dplyr rename select group_by summarize
#' @author KVC October 2017
ReadData_LN3_Node <- function(aRegionName, SUBREGION=NULL, subregionData=NULL) {
  # Silence package checks
  region <- subregion <- LandAllocatorRoot <- LandNode1 <- LandNode2 <-
    logit.exponent <- LandNode3 <- year.fillout <- LandNode3.logit.exponent <- logit.year.fillout <- NULL

  if(!is.null(SUBREGION)){
    # For SUBREGION only keep rows for which LandNode3 is not NA
    # That is read in information on children that have three node above them.
    data <- subregionData[!is.na(subregionData$LandNode3),]

    # Only keep select columns pertaining to LandNode3 and logit exponent for LandNode3
    data %>%
      select(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, year.fillout, LandNode3.logit.exponent) %>%
      rename(logit.exponent = LandNode3.logit.exponent) ->
      data

    # Find unique logit exponent values across region, root, LandNode1, LandNode2, LandNode3, and year
    data %>%
      group_by(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, year.fillout) %>%
      summarize(logit.exponent = unique(logit.exponent)) ->
      data
  }
  else {
    # Read in calibration data
    data <- suppressMessages(read_csv(system.file("extdata", "./initialization-data/L213.LN3_Logit.csv", package = "gcamland"), skip = 3))

    # Filter data for the specified region
    data <- rename(data, year.fillout = logit.year.fillout)
    data <- subset(data, region == aRegionName)

    # Filter data for specified AEZ
    if(!is.null(AEZ)){
      data <- subset(data, grepl(AEZ, LandNode1))
    }
  }

  return(data)
}

#' ReadData_LN3_GhostShare
#'
#' @details Read in ghost share for LN3 nodes
#' @param aRegionName Region to read data for
#' @return Ghost shares for level 3 nodes
#' @importFrom readr read_csv
#' @importFrom dplyr rename
#' @author KVC October 2017
ReadData_LN3_GhostShare <- function(aRegionName) {
  # Silence package checks
  region <- LandNode1 <- year.fillout <- logit.year.fillout <- NULL

  # Read in calibration data
  data <- suppressMessages(read_csv(system.file("extdata", "./initialization-data/L213.LN3_DefaultShare.csv", package = "gcamland"), skip = 3))

  # Filter data for the specified region -- using dplyr::filter for data frames with more than 15,000 entries
  data <- filter(data, region == aRegionName)

  # Filter data for specified AEZ
  if(!is.null(AEZ)){
    data <- filter(data, grepl(AEZ, LandNode1))
  }

  return(data)
}

#' ReadData_LN3_LandLeaf
#'
#' @details Read in names of information on LandLeafs that are
#'          children of LandNode3 nodes
#' @param aRegionName Region to read data for
#' @param SUBREGION Subregion to read data for
#' @param subregionData Subregion data
#' @return Data on LandLeaf children of LandNode2
#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows rename select group_by summarize
#' @author KVC October 2017
#' @export
ReadData_LN3_LandLeaf <- function(aRegionName, SUBREGION=NULL, subregionData=NULL) {
  # Silence package checks
  region <- subregion <- LandAllocatorRoot <- LandNode1 <- LandNode2 <- LandNode3 <-
    year <- LandLeaf <- year.fillout <- allocation <- NULL

  if(!is.null(SUBREGION)){
    # For SUBREGION only keep rows for which LandNode3 is not NA
    # That is read in information on children that have three node above them.
    data <- subregionData[!is.na(subregionData$LandNode3) & !is.na(subregionData$LandLeaf),]

    # Only keep select columns pertaining to LandNode3
    data %>%
      select(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, LandLeaf, year.fillout, allocation) %>%
      rename(year = year.fillout) ->
      data

    # Sum allocation across region, root, LandNode1, LandNode2, LandNode3, LandLeaf, and year
    data %>%
      group_by(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, LandLeaf, year) %>%
      summarize(allocation = sum(allocation)) ->
      data
  }
  else {
    # Read in calibration data
    suppressMessages(read_csv(system.file("extdata", "./initialization-data/L213.LN3_MgdAllocation_crop.csv", package = "gcamland"), skip = 3)) %>%
      bind_rows(suppressMessages(read_csv(system.file("extdata", "./initialization-data/L213.LN3_MgdAllocation_bio.csv", package = "gcamland"), skip = 3))) %>%
      bind_rows(suppressMessages(read_csv(system.file("extdata", "./initialization-data/L213.LN3_MgdAllocation_noncrop.csv", package = "gcamland"), skip = 3))) ->
      data

    # Filter data for the specified region -- using dplyr::filter for data frames with more than 15000 entries
    data <- filter(data, region == aRegionName)

    # Filter data for specified AEZ
    if(!is.null(AEZ)){
      data <- filter(data, grepl(AEZ, LandNode1))
    }
  }

  return(data)
}

#' ReadData_LN3_UnmanagedLandLeaf
#'
#' @details Read in names of information on UnmanagedLandLeafs that are
#'          children of LandNode3 nodes
#' @param aRegionName Region to read data for
#' @param SUBREGION Subregion to read data for
#' @param subregionData Subregion data
#' @return Data on UnmanagedLandLeaf children of LandNode3
#' @importFrom readr read_csv
#' @importFrom dplyr rename select group_by summarize
#' @author KVC October 2017
ReadData_LN3_UnmanagedLandLeaf <- function(aRegionName, SUBREGION=NULL, subregionData=NULL) {
  # Silence package checks
  region <- LandNode1 <- LandNode3 <- allocation <- LandAllocatorRoot <- subregion <- LandNode2 <-
    year <- UnmanagedLandLeaf <- year.fillout <- NULL

  if(!is.null(SUBREGION)){
    # For SUBREGION only keep rows for which LandNode3 is not NA
    # That is read in information on children that have three node above them.
    data <- subregionData[!is.na(subregionData$LandNode3) & !is.na(subregionData$UnmanagedLandLeaf),]

    # Only keep select columns pertaining to LandNode3
    data %>%
      select(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, UnmanagedLandLeaf, year.fillout, allocation) %>%
      rename(year = year.fillout) ->
      data

    # Sum allocation across region, root, LandNode1, LandNode2, LandNode3, UnmanagedLandLeaf, and year
    data %>%
      group_by(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, UnmanagedLandLeaf, year) %>%
      summarize(allocation = sum(allocation)) ->
      data
  }
  else {
    # Read in calibration data
    data <- suppressMessages(read_csv(system.file("extdata", "./initialization-data/L213.LN3_UnmgdAllocation.csv", package = "gcamland"), skip = 3))

    # Filter data for the specified region
    data <- subset(data, region == aRegionName)

    # Filter data for specified AEZ
    if(!is.null(AEZ)){
      data <- subset(data, grepl(AEZ, LandNode1))
    }
  }

  return(data)
}

#' ReadData_LN3_NewTech
#'
#' @details Read in new technologies that are children of LandNode3 nodes
#' @param aRegionName Region to read data for
#' @return Data on new technologies for children of LandNode3
#' @importFrom readr read_csv
#' @author KVC May 2018
ReadData_LN3_NewTech <- function(aRegionName) {
  # Silence package checks
  region <- LandNode1 <- NULL

  # Read in calibration data
  data <- suppressMessages(read_csv(system.file("extdata", "./initialization-data/L213.LN3_NewTech.csv", package = "gcamland"), skip = 3))

  # Filter data for the specified region
  data <- subset(data, region == aRegionName)

  # Filter data for specified AEZ
  if(!is.null(AEZ)){
    data <- subset(data, grepl(AEZ, LandNode1))
  }

  return(data)
}

#' ReadData_AgProd
#'
#' @details Read in ag production data
#' @param aRegionName Region to read data for
#' @param ascentype Scenario type: either "Reference" or "Hindcast"
#' @param SUBREGION Subregion to read data for
#' @param subregionData Subregion data
#' @return All AgProductionTechnology information
#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows rename select
#' @import tidyr
#' @author KVC October 2017
#' @export
ReadData_AgProd <- function(aRegionName, ascentype, SUBREGION=NULL, subregionData=NULL) {
  # Silence package checks
  region <- AgSupplySubsector <- year.fillout <- NULL

  if(!is.null(SUBREGION)){

    # Only keep rows for which AgSupplySector is not NA
    data <- subregionData[!is.na(subregionData$AgSupplySector),]

    # Only keep select columns pertaining to Agricultural production
    data %>%
      select("region","subregion","AgSupplySector",	"AgSupplySubsector", "AgProductionTechnology","year.fillout","calOutputValue","share.weight.year","subs.share.weight","tech.share.weight") %>%
      rename(year = year.fillout) ->
      calOutput

    agProdChange <- get_AgProdChange(ascentype, SUBREGION)

    cost <- suppressMessages(read_csv(system.file("extdata", "./initialization-data/AgCost_SRB.csv", package = "gcamland")))

    bioYield <- tibble(region=character(),
                       subregion=character(),
                       AgSupplySector=character(),
                       AgSupplySubsector=character(),
                       AgProductionTechnology=character(),
                       year=numeric(),
                       yield=numeric())
  }
  else {
    # Read in data
    suppressMessages(read_csv(system.file("extdata", "./initialization-data/L201.AgProduction_ag.csv", package = "gcamland"), skip = 3)) %>%
      bind_rows(suppressMessages(read_csv(system.file("extdata", "./initialization-data/L201.AgProduction_For.csv", package = "gcamland"), skip = 3))) %>%
      bind_rows(suppressMessages(read_csv(system.file("extdata", "./initialization-data/L201.AgProduction_Past.csv", package = "gcamland"), skip = 3))) ->
      calOutput
    agProdChange <- get_AgProdChange(ascentype)
    suppressMessages(read_csv(system.file("extdata", "./initialization-data/L205.AgCost_ag.csv", package = "gcamland"), skip = 3)) %>%
      bind_rows(suppressMessages(read_csv(system.file("extdata", "./initialization-data/L205.AgCost_bio.csv", package = "gcamland"), skip = 3))) %>%
      bind_rows(suppressMessages(read_csv(system.file("extdata", "./initialization-data/L205.AgCost_For.csv", package = "gcamland"), skip = 3))) ->
      cost
    suppressMessages(read_csv(system.file("extdata", "./initialization-data/L201.AgYield_bio_ref.csv", package = "gcamland"), skip = 3)) ->
      bioYield

    # Filter data for the specified region -- note using dplyr::filter for data frames with more than 15000 entries
    calOutput <- filter(calOutput, region == aRegionName)
    agProdChange <- filter(agProdChange, region == aRegionName)
    cost <- filter(cost, region == aRegionName)
    bioYield <- subset(bioYield, region == aRegionName)

    # Filter data for specified AEZ
    if(!is.null(AEZ)){
      calOutput <- filter(calOutput, grepl(AEZ, AgSupplySubsector))

      if(ascentype != "Hindcast") {
        # Hindcast data is only at the region level
        agProdChange <- filter(agProdChange, grepl(AEZ, AgSupplySubsector))
      }
      cost <- filter(cost, grepl(AEZ, AgSupplySubsector))
      bioYield <- subset(bioYield, grepl(AEZ, AgSupplySubsector))
    }
  }
  return(structure(list(calOutput, agProdChange, cost, bioYield),
                   rgn = aRegionName, scentype = ascentype))
}

#' get_AgProdChange
#'
#' @details Read in AgProdChange for all periods and return them
#' @param ascentype Scenario type: either "Reference" or "Hindcast"
#' @param SUBREGION Subregion to read data for
#' @return Tibble containing AgProdChange by commodity and year
#' @importFrom readr read_csv
#' @author KVC October 2017
get_AgProdChange <- function(ascentype, SUBREGION=NULL) {
  # Silence package checks
  year <- NULL

  if(!is.null(SUBREGION)){
    agProdChange <- suppressMessages(read_csv(system.file("extdata", "./scenario-data/AgProdChange_SRB.csv", package = "gcamland")))
  }
  else {
    # Read in data
    if(ascentype == "Hindcast") {
      agProdChange <- get_hindcast_AgProdChange()
    } else {
      agProdChange <- suppressMessages(read_csv(system.file("extdata", "./scenario-data/L205.AgProdChange_ref.csv", package = "gcamland"), skip = 3))
    }

    # Filter for years in model only
    agProdChange <- filter(agProdChange, year %in% YEARS[[ascentype]])
  }

  return(agProdChange)
}
