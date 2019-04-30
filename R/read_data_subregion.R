
library(readr)
library(dplyr)

setwd('~/Documents/Projects/GCAM/')

#' ReadData_LN0
#'
#' @details Read in total land allocation and logit exponents
#'          for the LandAllocator for subregion
#' @param aRegionName Region to read data for
#' @return Land allocator data
#' @importFrom readr read_csv
#' @importFrom dplyr select, rename, group_by, summarize
#' @author ES April 2019
ReadData_LN0_subregion <- function(aRegionName) {
  # Silence package checks
  region <- land <- year.fillout <- logit.year.fillout <- NULL

  # Read in calibration data
  land_logit <- read_csv("~/Documents/Projects/GCAM/Notes/LandUse_Nesting_SRB.csv")

  # Only keep select columns pertaining to root
  land_logit %>%
    select("region","subregion","LandAllocatorRoot","year.fillout","allocation","Root.logit.exponent") %>%
    rename(logit.exponent = Root.logit.exponent,
           landAllocation = allocation) ->
    data

  # Sum allocation and find unique logit exponent across region, root, and year
  data %>%
    group_by(region, subregion, LandAllocatorRoot, year.fillout) %>%
    summarize(landAllocation = sum(landAllocation),
              logit.exponent = unique(logit.exponent)) ->
    data

  # Filter data for the specified region
  data <- subset(data, region == aRegionName)

  return(data)
}


#' ReadData_LN1_Node
#'
#' @details Read in unmanaged land value, names of children, and logit exponents
#'          for the LandAllocator.
#' @param aRegionName Region to read data for
#' @return Data on children of the land allocator
#' @importFrom readr read_csv
#' @importFrom dplyr select, rename, group_by, summarize
#' @author ES April 2019
ReadData_LN1_Node_subregion <- function(aRegionName) {
  # Silence package checks
  region <- LandNode1 <- NULL

  # Read in calibration data
  land_logit <- read_csv("~/Documents/Projects/GCAM/Notes/LandUse_Nesting_SRB.csv")

  # Only keep select columns pertaining to LandNode1 and logit exponent for LandNode1
  land_logit %>%
    select("region","subregion","LandAllocatorRoot","LandNode1","year.fillout","LandNode1.logit.exponent","unManagedLandValue") %>%
    rename(logit.exponent = LandNode1.logit.exponent,
           logit.year.fillout = year.fillout) ->
    data

  # Find unique logit exponent values across region, root, LandNode1, and year
  data %>%
    group_by(region, subregion, LandAllocatorRoot, LandNode1, logit.year.fillout) %>%
    summarize(logit.exponent = unique(logit.exponent),
              unManagedLandValue = unique(unManagedLandValue)) ->
    data

  # Filter data for the specified region
  data <- subset(data, region == aRegionName)

  return(data)
}

#' ReadData_LN1_LeafChildren
#'
#' @details Read in the leaf children of LandNode1. That is
#'          read in information on children that only have one
#'          node above them.
#' @param aRegionName Region to read data for
#' @return Data on children of the land allocator
#' @importFrom readr read_csv
#' @importFrom dplyr select, rename, group_by, summarize
#' @author ES April 2019
ReadData_LN1_LeafChildren_subregion <- function(aRegionName) {
  # Silence package checks
  region <- LandNode1 <- NULL

  # Read in calibration data
  land_logit <- read_csv("~/Documents/Projects/GCAM/Notes/LandUse_Nesting_SRB.csv")

  # Only keep rows for which LandNode2 is NA
  # That is read in information on children that only have one node above them.
  data <- land_logit[is.na(land_logit$LandNode2),]

  # Only keep select columns pertaining to LandNode1
  data %>%
    select("region","subregion","LandAllocatorRoot","LandNode1","UnmanagedLandLeaf","year.fillout","allocation") %>%
    rename(year = year.fillout) ->
    data

  # Sum allocation across region, root, LandNode1, and year
  data %>%
    group_by(region, subregion, LandAllocatorRoot, LandNode1, UnmanagedLandLeaf, year) %>%
    summarize(allocation = sum(allocation)) ->
    data

  # Filter data for the specified region
  data <- subset(data, region == aRegionName)

  return(data)
}

#' ReadData_LN2_Node
#'
#' @details Read in names of children and logit exponents for LN2
#' @param aRegionName Region to read data for
#' @return Data on level 2 nodes of the land allocator
#' @importFrom readr read_csv
#' @importFrom dplyr select, rename, group_by, summarize
#' @author ES April 2019
ReadData_LN2_Node_subregion <- function(aRegionName) {
  # Silence package checks
  region <- LandNode1 <- year.fillout <- logit.year.fillout <- NULL

  # Read in calibration data
  land_logit <- read_csv("~/Documents/Projects/GCAM/Notes/LandUse_Nesting_SRB.csv")

  # Only keep rows for which LandNode2 is not NA and LandNode3 is NA
  # That is read in information on children that have two node above them.
  data <- land_logit[!is.na(land_logit$LandNode2),]

  # Only keep select columns pertaining to LandNode2 and logit exponent for LandNode2
  data %>%
    select("region","subregion","LandAllocatorRoot","LandNode1","LandNode2","year.fillout","LandNode2.logit.exponent") %>%
    rename(logit.exponent = LandNode2.logit.exponent) ->
    data

  # Find unique logit exponent values across region, root, LandNode1, LandNode2, and year
  data %>%
    group_by(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, year.fillout) %>%
    summarize(logit.exponent = unique(logit.exponent)) ->
    data

  # Filter data for the specified region
  data <- subset(data, region == aRegionName)

  return(data)
}

#' ReadData_LN2_LandLeaf
#'
#' @details Read in names of information on LandLeafs that are
#'          children of LandNode2 nodes
#' @param aRegionName Region to read data for
#' @return Data on LandLeaf children of LandNode2
#' @importFrom readr read_csv
#' @importFrom dplyr select, rename, group_by, summarize
#' @author ES April 2019
ReadData_LN2_LandLeaf_subregion <- function(aRegionName) {
  # Silence package checks
  region <- LandNode1 <- NULL

  # Read in calibration data
  land_logit <- read_csv("~/Documents/Projects/GCAM/Notes/LandUse_Nesting_SRB.csv")

  # Only keep rows for which LandNode2 is not NA and LandNode3 is NA
  # That is read in information on children that have two node above them.
  data <- land_logit[!is.na(land_logit$LandNode2) & is.na(land_logit$LandNode3) & !is.na(land_logit$LandLeaf),]

  # Only keep select columns pertaining to LandNode2
  data %>%
    select("region","subregion","LandAllocatorRoot","LandNode1","LandNode2","LandLeaf","year.fillout","allocation") %>%
    rename(year = year.fillout) ->
    data

  # Sum allocation across region, root, LandNode1, LandNode2, LandLeaf, and year
  data %>%
    group_by(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, LandLeaf, year) %>%
    summarize(allocation = sum(allocation)) ->
    data

  # Filter data for the specified region
  data <- subset(data, region == aRegionName)

  return(data)
}

#' ReadData_LN2_UnmanagedLandLeaf
#'
#' @details Read in names of information on UnmanagedLandLeafs that are
#'          children of LandNode2 nodes
#' @param aRegionName Region to read data for
#' @return Data on UnmanagedLandLeaf children of LandNode2
#' @importFrom readr read_csv
#' @importFrom dplyr select, rename, group_by, summarize
#' @author ES April 2019
ReadData_LN2_UnmanagedLandLeaf_subregion <- function(aRegionName) {
  # Silence package checks
  region <- LandNode1 <- NULL

  # Read in calibration data
  land_logit <- read_csv("~/Documents/Projects/GCAM/Notes/LandUse_Nesting_SRB.csv")

  # Only keep rows for which LandNode2 is not NA and LandNode3 is NA
  # That is read in information on children that have two node above them.
  data <- land_logit[!is.na(land_logit$LandNode2) & is.na(land_logit$LandNode3) & !is.na(land_logit$UnmanagedLandLeaf),]

  # Only keep select columns pertaining to LandNode2
  data %>%
    select("region","subregion","LandAllocatorRoot","LandNode1","LandNode2","UnmanagedLandLeaf","year.fillout","allocation") %>%
    rename(year = year.fillout) ->
    data

  # Sum allocation across region, root, LandNode1, LandNode2, UnmanagedLandLeaf, and year
  data %>%
    group_by(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, UnmanagedLandLeaf, year) %>%
    summarize(allocation = sum(allocation)) ->
    data

  # Filter data for the specified region
  data <- subset(data, region == aRegionName)

  return(data)
}

#' ReadData_LN3_Node
#'
#' @details Read in names of children and logit exponents for LN2
#' @param aRegionName Region to read data for
#' @return Data on level 3 nodes of the land allocator
#' @importFrom readr read_csv
#' @importFrom dplyr select, rename, group_by, summarize
#' @author ES April 2019
ReadData_LN3_Node_subregion <- function(aRegionName) {
  # Silence package checks
  region <- LandNode1 <- year.fillout <- logit.year.fillout <- NULL

  # Read in calibration data
  land_logit <- read_csv("~/Documents/Projects/GCAM/Notes/LandUse_Nesting_SRB.csv")

  # Only keep rows for which LandNode3 is not NA
  # That is read in information on children that have three node above them.
  data <- land_logit[!is.na(land_logit$LandNode3),]

  # Only keep select columns pertaining to LandNode3 and logit exponent for LandNode3
  data %>%
    select("region","subregion","LandAllocatorRoot","LandNode1","LandNode2","LandNode3","year.fillout","LandNode3.logit.exponent") %>%
    rename(logit.exponent = LandNode3.logit.exponent) ->
    data

  # Find unique logit exponent values across region, root, LandNode1, LandNode2, LandNode3, and year
  data %>%
    group_by(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, year.fillout) %>%
    summarize(logit.exponent = unique(logit.exponent)) ->
    data

  # Filter data for the specified region
  data <- subset(data, region == aRegionName)

  return(data)
}

#' ReadData_LN3_LandLeaf
#'
#' @details Read in names of information on LandLeafs that are
#'          children of LandNode3 nodes
#' @param aRegionName Region to read data for
#' @return Data on LandLeaf children of LandNode2
#' @importFrom readr read_csv
#' @importFrom dplyr select, rename, group_by, summarize
#' @author ES April 2019
ReadData_LN3_LandLeaf_subregion <- function(aRegionName) {
  # Silence package checks
  region <- LandNode1 <- NULL

  # Read in calibration data
  land_logit <- read_csv("~/Documents/Projects/GCAM/Notes/LandUse_Nesting_SRB.csv")

  # Only keep rows for which LandNode3 is not NA
  # That is read in information on children that have three node above them.
  data <- land_logit[!is.na(land_logit$LandNode3) & !is.na(land_logit$LandLeaf),]

  # Only keep select columns pertaining to LandNode3
  data %>%
    select("region","subregion","LandAllocatorRoot","LandNode1","LandNode2","LandNode3","LandLeaf","year.fillout","allocation") %>%
    rename(year = year.fillout) ->
    data

  # Sum allocation across region, root, LandNode1, LandNode2, LandNode3, LandLeaf, and year
  data %>%
    group_by(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, LandLeaf, year) %>%
    summarize(allocation = sum(allocation)) ->
    data

  # Filter data for the specified region
  data <- subset(data, region == aRegionName)

  return(data)
}

#' ReadData_LN3_UnmanagedLandLeaf
#'
#' @details Read in names of information on UnmanagedLandLeafs that are
#'          children of LandNode3 nodes
#' @param aRegionName Region to read data for
#' @return Data on UnmanagedLandLeaf children of LandNode3
#' @importFrom readr read_csv
#' @importFrom dplyr select, rename, group_by, summarize
#' @author ES April 2019
ReadData_LN3_UnmanagedLandLeaf_subregion <- function(aRegionName) {
  # Silence package checks
  region <- LandNode1 <- NULL

  # Read in calibration data
  land_logit <- read_csv("~/Documents/Projects/GCAM/Notes/LandUse_Nesting_SRB.csv")

  # Only keep rows for which LandNode3 is not NA
  # That is read in information on children that have three node above them.
  data <- land_logit[!is.na(land_logit$LandNode3) & !is.na(land_logit$UnmanagedLandLeaf),]

  # Only keep select columns pertaining to LandNode3
  data %>%
    select("region","subregion","LandAllocatorRoot","LandNode1","LandNode2","LandNode3","UnmanagedLandLeaf","year.fillout","allocation") %>%
    rename(year = year.fillout) ->
    data

  # Sum allocation across region, root, LandNode1, LandNode2, LandNode3, UnmanagedLandLeaf, and year
  data %>%
    group_by(region, subregion, LandAllocatorRoot, LandNode1, LandNode2, LandNode3, UnmanagedLandLeaf, year) %>%
    summarize(allocation = sum(allocation)) ->
    data

  # Filter data for the specified region
  data <- subset(data, region == aRegionName)

  return(data)
}
