# constants.R

# Time-related parameters
FINAL_CALIBRATION_PERIOD <- 1
PERIODS <- 1:4
YEARS <- c(2010, 2020, 2030, 2040)

# Expectation-related parameters (currently only "Perfect" and "Linear" are supported)
EXPECTATION.TYPE <- "Perfect"
LINEAR.YEARS <- 5

# Threshold-related constants
# TODO: Make sure these are consistent with GCAM
LARGE_NUMBER <- 1e9
SMALL_NUMBER <- 1e-6
DBL_MIN <- 1e-15

# Things that will eventually be read in
UNMANAGED_LAND_VALUE <- 1e9
REGION <- "USA"
