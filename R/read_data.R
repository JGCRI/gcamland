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
#' @return Land allocator data
#' @importFrom readr read_csv
#' @author KVC October 2017
ReadData_LN0 <- function(aRegionName) {
  # Read in calibration data
  land <- suppressMessages(read_csv("./inst/extdata/gcam43-data/L211.LN0_Land.csv", skip = 3))
  logit <- suppressMessages(read_csv("./inst/extdata/gcam43-data/L211.LN0_Logit.csv", skip = 3))

  # Join all data into a single frame
  logit %>%
    rename(year.fillout = logit.year.fillout) %>%
    left_join(land, by=c("region", "LandAllocatorRoot", "year.fillout")) ->
    data

  # Filter data for the specified region
  data %>%
    filter(region == aRegionName) ->
    data

  return(data)
}


#' ReadData_LN1_Node
#'
#' @details Read in unmanaged land value, names of children, and logit exponents
#'          for the LandAllocator.
#' @param aRegionName Region to read data for
#' @return Data on children of the land allocator
#' @importFrom readr read_csv
#' @author KVC October 2017
ReadData_LN1_Node <- function(aRegionName) {
  # Read in calibration data
  data <- suppressMessages(read_csv("./inst/extdata/gcam43-data/L211.LN1_ValueLogit.csv", skip = 3))

  # Filter data for the specified region
  data %>%
    filter(region == aRegionName) ->
    data

  # TEMP: Filter data for specified AEZ
  data %>%
    filter(grepl(AEZ, LandNode1)) ->
    data

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
#' @author KVC October 2017
ReadData_LN1_LeafChildren <- function(aRegionName) {
  # Read in calibration data
  data <- suppressMessages(read_csv("./inst/extdata/gcam43-data/L211.LN1_UnmgdAllocation.csv", skip = 3))

  # Filter data for the specified region
  data %>%
    filter(region == aRegionName) ->
    data

  # TEMP: Filter data for specified AEZ
  data %>%
    filter(grepl(AEZ, LandNode1)) ->
    data

  return(data)
}

#' ReadData_LN2_Node
#'
#' @details Read in names of children and logit exponents for LN2
#' @param aRegionName Region to read data for
#' @return Data on level 2 nodes of the land allocator
#' @importFrom readr read_csv
#' @author KVC October 2017
ReadData_LN2_Node <- function(aRegionName) {
  # Read in calibration data
  data <- suppressMessages(read_csv("./inst/extdata/gcam43-data/L212.LN2_Logit.csv", skip = 3))

  # Filter data for the specified region
  data %>%
    rename(year.fillout = logit.year.fillout) %>%
    filter(region == aRegionName) ->
    data

  # TEMP: Filter data for specified AEZ
  data %>%
    filter(grepl(AEZ, LandNode1)) ->
    data

  return(data)
}

#' ReadData_LN2_LandLeaf
#'
#' @details Read in names of information on LandLeafs that are
#'          children of LandNode2 nodes
#' @param aRegionName Region to read data for
#' @return Data on LandLeaf children of LandNode2
#' @importFrom readr read_csv
#' @author KVC October 2017
ReadData_LN2_LandLeaf <- function(aRegionName) {
  # Read in calibration data
  data <- suppressMessages(read_csv("./inst/extdata/gcam43-data/L212.LN2_MgdAllocation.csv", skip = 3))

  head(data)

  # Filter data for the specified region
  data %>%
    filter(region == aRegionName) ->
    data

  # TEMP: Filter data for specified AEZ
  data %>%
    filter(grepl(AEZ, LandNode1)) ->
    data

  return(data)
}

#' ReadData_LN2_UnmanagedLandLeaf
#'
#' @details Read in names of information on UnmanagedLandLeafs that are
#'          children of LandNode2 nodes
#' @param aRegionName Region to read data for
#' @return Data on UnmanagedLandLeaf children of LandNode2
#' @importFrom readr read_csv
#' @author KVC October 2017
ReadData_LN2_UnmanagedLandLeaf <- function(aRegionName) {
  # Read in calibration data
  data <- suppressMessages(read_csv("./inst/extdata/gcam43-data/L212.LN2_UnmgdAllocation.csv", skip = 3))

  head(data)

  # Filter data for the specified region
  data %>%
    filter(region == aRegionName) ->
    data

  # TEMP: Filter data for specified AEZ
  data %>%
    filter(grepl(AEZ, LandNode1)) ->
    data

  return(data)
}

#' ReadData_LN3_Node
#'
#' @details Read in names of children and logit exponents for LN2
#' @param aRegionName Region to read data for
#' @return Data on level 3 nodes of the land allocator
#' @importFrom readr read_csv
#' @author KVC October 2017
ReadData_LN3_Node <- function(aRegionName) {
  # Read in calibration data
  data <- suppressMessages(read_csv("./inst/extdata/gcam43-data/L213.LN3_Logit.csv", skip = 3))

  # Filter data for the specified region
  data %>%
    rename(year.fillout = logit.year.fillout) %>%
    filter(region == aRegionName) ->
    data

  # TEMP: Filter data for specified AEZ
  data %>%
    filter(grepl(AEZ, LandNode1)) ->
    data

  return(data)
}

#' ReadData_LN3_LandLeaf
#'
#' @details Read in names of information on LandLeafs that are
#'          children of LandNode3 nodes
#' @param aRegionName Region to read data for
#' @return Data on LandLeaf children of LandNode2
#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows
#' @author KVC October 2017
ReadData_LN3_LandLeaf <- function(aRegionName) {
  # Read in calibration data
  suppressMessages(read_csv("./inst/extdata/gcam43-data/L213.LN3_MgdAllocation_crop.csv", skip = 3)) %>%
    bind_rows(suppressMessages(read_csv("./inst/extdata/gcam43-data/L213.LN3_MgdAllocation_bio.csv", skip = 3))) %>%
    bind_rows(suppressMessages(read_csv("./inst/extdata/gcam43-data/L213.LN3_MgdAllocation_noncrop.csv", skip = 3))) ->
    data

  head(data)

  # Filter data for the specified region
  data %>%
    filter(region == aRegionName) ->
    data

  # TEMP: Filter data for specified AEZ
  data %>%
    filter(grepl(AEZ, LandNode1)) ->
    data

  return(data)
}

#' ReadData_LN3_UnmanagedLandLeaf
#'
#' @details Read in names of information on UnmanagedLandLeafs that are
#'          children of LandNode3 nodes
#' @param aRegionName Region to read data for
#' @return Data on UnmanagedLandLeaf children of LandNode2
#' @importFrom readr read_csv
#' @author KVC October 2017
ReadData_LN3_UnmanagedLandLeaf <- function(aRegionName) {
  # Read in calibration data
  data <- suppressMessages(read_csv("./inst/extdata/gcam43-data/L213.LN3_UnmgdAllocation.csv", skip = 3))

  head(data)

  # Filter data for the specified region
  data %>%
    filter(region == aRegionName) ->
    data

  # TEMP: Filter data for specified AEZ
  data %>%
    filter(grepl(AEZ, LandNode1)) ->
    data

  return(data)
}

