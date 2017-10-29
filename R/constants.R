# constants.R

# Time-related parameters
FINAL_CALIBRATION_PERIOD <- 4
HISTORY.YEARS <- c(1975, 1990, 2005, 2010)
FUTURE.YEARS <- seq(2015, 2050, 5)
YEARS <- c(HISTORY.YEARS, FUTURE.YEARS)
PERIODS <- 1:length(YEARS)

# Expectation-related parameters (currently only "Perfect" and "Linear" are supported)
EXPECTATION.TYPE <- "Perfect"
LINEAR.YEARS <- 5

# Threshold-related constants
# TODO: Make sure these are consistent with GCAM
LARGE_NUMBER <- 1e9
SMALL_NUMBER <- 1e-6
DBL_MIN <- 1e-15

# Types of biomass crops
BIOMASS_TYPES <- c("biomass", "willow")

# Things that will eventually be removed
UNMANAGED_LAND_VALUE <- 1e9
REGION <- "USA"
AEZ <- NULL
