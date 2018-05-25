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
#' @importFrom dplyr rename
#' @author KVC October 2017
ReadData_LN0 <- function(aRegionName) {
  # Silence package checks
  region <- land <- year.fillout <- logit.year.fillout <- NULL

  # Read in calibration data
  land <- suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L211.LN0_Land.csv", package = "gcamland"), skip = 3))
  logit <- suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L211.LN0_Logit.csv", package = "gcamland"), skip = 3))

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
  # Silence package checks
  region <- LandNode1 <- NULL

  # Read in calibration data
  data <- suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L211.LN1_ValueLogit.csv", package = "gcamland"), skip = 3))

  # Filter data for the specified region
  data %>%
    filter(region == aRegionName) ->
    data

  # Filter data for specified AEZ
  if(!is.null(AEZ)){
    data %>%
      filter(grepl(AEZ, LandNode1)) ->
      data
  }

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
  # Silence package checks
  region <- LandNode1 <- NULL

  # Read in calibration data
  data <- suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L211.LN1_UnmgdAllocation.csv", package = "gcamland"), skip = 3))

  # Filter data for the specified region
  data %>%
    filter(region == aRegionName) ->
    data

  # Filter data for specified AEZ
  if(!is.null(AEZ)){
    data %>%
      filter(grepl(AEZ, LandNode1)) ->
      data
  }

  return(data)
}

#' ReadData_LN2_Node
#'
#' @details Read in names of children and logit exponents for LN2
#' @param aRegionName Region to read data for
#' @return Data on level 2 nodes of the land allocator
#' @importFrom readr read_csv
#' @importFrom dplyr rename
#' @author KVC October 2017
ReadData_LN2_Node <- function(aRegionName) {
  # Silence package checks
  region <- LandNode1 <- year.fillout <- logit.year.fillout <- NULL

  # Read in calibration data
  data <- suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L212.LN2_Logit.csv", package = "gcamland"), skip = 3))

  # Filter data for the specified region
  data %>%
    rename(year.fillout = logit.year.fillout) %>%
    filter(region == aRegionName) ->
    data

  # Filter data for specified AEZ
  if(!is.null(AEZ)){
    data %>%
      filter(grepl(AEZ, LandNode1)) ->
      data
  }

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
  # Silence package checks
  region <- LandNode1 <- NULL

  # Read in calibration data
  data <- suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L212.LN2_MgdAllocation.csv", package = "gcamland"), skip = 3))

  # Filter data for the specified region
  data %>%
    filter(region == aRegionName) ->
    data

  # Filter data for specified AEZ
  if(!is.null(AEZ)){
    data %>%
      filter(grepl(AEZ, LandNode1)) ->
      data
  }

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
  # Silence package checks
  region <- LandNode1 <- NULL

  # Read in calibration data
  data <- suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L212.LN2_UnmgdAllocation.csv", package = "gcamland"), skip = 3))

  # Filter data for the specified region
  data %>%
    filter(region == aRegionName) ->
    data

  # Filter data for specified AEZ
  if(!is.null(AEZ)){
    data %>%
      filter(grepl(AEZ, LandNode1)) ->
      data
  }

  return(data)
}

#' ReadData_LN3_Node
#'
#' @details Read in names of children and logit exponents for LN2
#' @param aRegionName Region to read data for
#' @return Data on level 3 nodes of the land allocator
#' @importFrom readr read_csv
#' @importFrom dplyr rename
#' @author KVC October 2017
ReadData_LN3_Node <- function(aRegionName) {
  # Silence package checks
  region <- LandNode1 <- year.fillout <- logit.year.fillout <- NULL

  # Read in calibration data
  data <- suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L213.LN3_Logit.csv", package = "gcamland"), skip = 3))

  # Filter data for the specified region
  data %>%
    rename(year.fillout = logit.year.fillout) %>%
    filter(region == aRegionName) ->
    data

  # Filter data for specified AEZ
  if(!is.null(AEZ)){
    data %>%
      filter(grepl(AEZ, LandNode1)) ->
      data
  }

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
#' @export
ReadData_LN3_LandLeaf <- function(aRegionName) {
  # Silence package checks
  region <- LandNode1 <- NULL

  # Read in calibration data
  suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L213.LN3_MgdAllocation_crop.csv", package = "gcamland"), skip = 3)) %>%
    bind_rows(suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L213.LN3_MgdAllocation_bio.csv", package = "gcamland"), skip = 3))) %>%
    bind_rows(suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L213.LN3_MgdAllocation_noncrop.csv", package = "gcamland"), skip = 3))) ->
    data

  # Filter data for the specified region
  data %>%
    filter(region == aRegionName) ->
    data

  # Filter data for specified AEZ
  if(!is.null(AEZ)){
    data %>%
      filter(grepl(AEZ, LandNode1)) ->
      data
  }

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
  # Silence package checks
  region <- LandNode1 <- NULL

  # Read in calibration data
  data <- suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L213.LN3_UnmgdAllocation.csv", package = "gcamland"), skip = 3))

  # Filter data for the specified region
  data %>%
    filter(region == aRegionName) ->
    data

  # Filter data for specified AEZ
  if(!is.null(AEZ)){
    data %>%
      filter(grepl(AEZ, LandNode1)) ->
      data
  }

  return(data)
}

#' ReadData_AgProd
#'
#' @details Read in ag production data
#' @param aRegionName Region to read data for
#' @param ascentype Scenario type: either "Reference" or "Hindcast"
#' @return All AgProductionTechnology information
#' @importFrom readr read_csv
#' @importFrom dplyr bind_rows
#' @import tidyr
#' @author KVC October 2017
#' @export
ReadData_AgProd <- function(aRegionName, ascentype) {
  # Silence package checks
  region <- AgSupplySubsector <- NULL

  # Read in data
  suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L201.AgProduction_ag.csv", package = "gcamland"), skip = 3)) %>%
    bind_rows(suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L201.AgProduction_For.csv", package = "gcamland"), skip = 3))) %>%
    bind_rows(suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L201.AgProduction_Past.csv", package = "gcamland"), skip = 3))) ->
    calOutput
  agProdChange <- get_AgProdChange(ascentype)
  suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L205.AgCost_ag.csv", package = "gcamland"), skip = 3)) %>%
    bind_rows(suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L205.AgCost_bio.csv", package = "gcamland"), skip = 3))) %>%
    bind_rows(suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L205.AgCost_For.csv", package = "gcamland"), skip = 3))) ->
    cost
  suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L201.AgYield_bio_ref.csv", package = "gcamland"), skip = 3)) ->
    bioYield

  # Filter data for the specified region
  calOutput %>%
    filter(region == aRegionName) ->
    calOutput

  agProdChange %>%
    filter(region == aRegionName) ->
    agProdChange

  cost %>%
    filter(region == aRegionName) ->
    cost

  bioYield %>%
    filter(region == aRegionName) ->
    bioYield

  # Filter data for specified AEZ
  if(!is.null(AEZ)){
    calOutput %>%
      filter(grepl(AEZ, AgSupplySubsector)) ->
      calOutput

    if(ascentype != "Hindcast") {
      # Hindcast data is only at the region level
      agProdChange %>%
        filter(grepl(AEZ, AgSupplySubsector)) ->
        agProdChange
    }

    cost %>%
      filter(grepl(AEZ, AgSupplySubsector)) ->
      cost

    bioYield %>%
      filter(grepl(AEZ, AgSupplySubsector)) ->
      bioYield
  }

  return(list(calOutput, agProdChange, cost, bioYield))
}

#' get_AgProdChange
#'
#' @details Read in AgProdChange for all periods and return them
#' @param ascentype Scenario type: either "Reference" or "Hindcast"
#' @return Tibble containing AgProdChange by commodity and year
#' @importFrom readr read_csv
#' @author KVC October 2017
get_AgProdChange <- function(ascentype) {
  # Silence package checks
  year <- NULL

  # Read in data
  if(ascentype == "Hindcast") {
    agProdChange <- get_hindcast_AgProdChange()
  } else {
    agProdChange <- suppressMessages(read_csv(system.file("extdata", "./gcam43-data/L205.AgProdChange_ref.csv", package = "gcamland"), skip = 3))
  }

  # Filter for years in model only
  agProdChange %>%
    filter(year %in% YEARS[[ascentype]]) ->
    agProdChange

  return(agProdChange)
}

