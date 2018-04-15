# constants.R

# **************************************************************************************************************

# **************************************************************************************************************
# * Run configuration settings
MAKE.PLOTS <- FALSE

# **************************************************************************************************************
# * Time-related parameters
TIME.PARAMS <- list(
    "Hindcast" = list(FINAL_CALIBRATION_PERIOD=1, HISTORY.YEARS=1975,
    FUTURE.YEARS=seq(1976, 2010, 1)),
    "Reference" = list(FINAL_CALIBRATION_PERIOD=4, HISTORY.YEARS=c(1975, 1990,
                                                   2005, 2010),
    FUTURE.YEARS=seq(2015, 2050, 5)))
SCEN.TYPES <- names(TIME.PARAMS)

# Concatenate years
YEARS <- sapply(SCEN.TYPES, function(type) {
                   c(TIME.PARAMS[[type]]$HISTORY.YEARS,
                     TIME.PARAMS[[type]]$FUTURE.YEARS)
               }, simplify=FALSE, USE.NAMES=TRUE)
PERIODS <- sapply(SCEN.TYPES, function(type) {
                      1:length(YEARS[[type]])
                  }, simplify=FALSE, USE.NAMES=TRUE)

# **************************************************************************************************************
# * Threshold-related constants
# TODO: Make sure these are consistent with GCAM
LARGE_NUMBER <- 1e9
SMALL_NUMBER <- 1e-6
DBL_MIN <- 1e-15

# **************************************************************************************************************
# * Types of biomass crops
BIOMASS_TYPES <- c("biomass", "willow")

# **************************************************************************************************************
# * Things that will eventually be removed
DEFAULT.REGION <- "USA"
AEZ <- NULL
