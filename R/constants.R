# constants.R

# **************************************************************************************************************

# **************************************************************************************************************
# * Time-related parameters
TIME.PARAMS <- list(
    "Hindcast" = list(FINAL_CALIBRATION_PERIOD=1, HISTORY.YEARS=1975,
    FUTURE.YEARS=seq(1976, 2015, 1)),
    "Hindcast1990" = list(FINAL_CALIBRATION_PERIOD=1, HISTORY.YEARS=1990,
                      FUTURE.YEARS=seq(1991, 2015, 1)),
    "Hindcast2005" = list(FINAL_CALIBRATION_PERIOD=1, HISTORY.YEARS=2005,
                     FUTURE.YEARS=seq(2006, 2015, 1)),
    "Hindcast5yr" = list(FINAL_CALIBRATION_PERIOD=1, HISTORY.YEARS=1990,
                      FUTURE.YEARS=seq(1995, 2015, 5)),
    "Reference" = list(FINAL_CALIBRATION_PERIOD=4, HISTORY.YEARS=c(1975, 1990,
                                                                   2005, 2010),
                       FUTURE.YEARS=seq(2015, 2100, 5)),
    "SRB" = list(FINAL_CALIBRATION_PERIOD=4, HISTORY.YEARS=c(1975, 1990,
                                                                   2005, 2010),
                       FUTURE.YEARS=seq(2015, 2100, 5)),
    "PCHES" = list(FINAL_CALIBRATION_PERIOD=1, HISTORY.YEARS=c(2010),
    FUTURE.YEARS=c(2015, 2020, 2050)))
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
# * Crop groupings for expectations. gcamland has the option of differentiating expectation
# * parameters by crop. We do this in three groups, with the crops belonging to each group specified here.
CROP_GROUP1 <- c("Corn", "OilCrop")         # Crops used for feed/fuel.
CROP_GROUP2 <- c("Wheat", "OtherGrain")     # Other coarse grains.
CROP_GROUP3 <- c( "Rice")
CROP_GROUP4 <- c("SugarCrop")
CROP_GROUP5 <- c("biomass", "FiberCrop", "FodderGrass", "FodderHerb",
                 "Forest", "MiscCrop", "PalmFruit", "Pasture",
                 "Root_Tuber", "willow")  # All other commodities


# **************************************************************************************************************
# * Threshold-related constants
# TODO: Make sure these are consistent with GCAM
LARGE_NUMBER <- 1e9
SMALL_NUMBER <- 1e-6
DBL_MIN <- 1e-15

# **************************************************************************************************************
# * Types of biomass crops
BIOMASS_TYPES <- c("biomass", "willow", "eucalyptus", "Jatropha", "miscanthus")

# **************************************************************************************************************
# * Region-specificiation. Only one region is currently permitted. A single AEZ can be specified, or
# * if AEZ = NULL, then all AEZs will be run.
# * TODO: enable multiple regions as an option
DEFAULT.REGION <- "USA"
AEZ <- NULL
