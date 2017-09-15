# constants.R

DEBUG <- FALSE
FINAL_CALIBRATION_PERIOD <- 2010
YEARS <- c(2010, 2020)

# TODO: Make sure these are consistent with GCAM
LARGE_NUMBER <- 1e9
SMALL_NUMBER <- 1e-6
DBL_MIN <- 1e-15

# Things that will eventually be read in
UNMANAGED_LAND_VALUE <- 0.5
LOGIT_EXPONENT <- 0.75
REGION <- "USA"
LAND_ALLOCATION <- 200

tibble::tibble(name = c("Crop1", "Crop2"),
               price = c(1, 2),
               cost = c(0, 1),
               yield = c(1, 1),
               area = c(100, 100)) -> LANDLEAF_CALDATA

tibble::tibble(name = c("Land"),
               area = c(200)) -> LANDNODE_CALDATA
