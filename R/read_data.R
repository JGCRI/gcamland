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
#                    possible. For now, we are setting this up so that
#                    the only hard coding is in this file. To add another
#                    level, we only need to amend the files read in here.
#                    This is comparable to C++ where you need to change
#                    the xml inputs in the configuration file.

#' ReadData_LandAllocator
#'
#' @details Read in total land allocation and logit exponents
#'          for the LandAllocator.
#' @param aRegionName Region to read data for
#' @return Land allocator data
#' @importFrom readr read_csv
#' @author KVC October 2017
ReadData_LandAllocator <- function(aRegionName) {
  # Read in calibration data
  land <- suppressMessages(read_csv("./inst/extdata/gcam43-data/L211.LN0_Land.csv", skip = 3))
  logit <- suppressMessages(read_csv("./inst/extdata/gcam43-data/L211.LN0_Logit.csv", skip = 3))

  # Join all data into a single frame
  logit %>%
    rename(year.fillout = logit.year.fillout) %>%
    left_join(land, by=c("region", "LandAllocatorRoot", "year.fillout")) ->
    LandAllocatorData

  # Filter data for the specified region
  # TODO: Remove this when we are ready to do lots of regions
  LandAllocatorData %>%
    filter(region == aRegionName) ->
    LandAllocatorData

  return(LandAllocatorData)
}
