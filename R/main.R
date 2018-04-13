# main.R

#' run_ensembles
#'
#' @details Loop over a large parameter set and run the offline land model
#' @param aOutputDir Output directory
#' @import foreach doParallel
#' @author KVC November 2017
#' @export
run_ensembles <- function(aOutputDir = "./outputs") {
  # Silence package checks
  obj <- NULL

  # Set options for ensembles
  levels.AGROFOREST <- c(0.1, 0.25, 0.5, 1.25, 2.5, 5, 10)
  levels.AGROFOREST_NONPASTURE <- c(0.1, 0.25, 0.5, 1.25, 2.5, 5, 10)
  levels.CROPLAND <- c(0.1, 0.25, 0.5, 1.25, 2.5, 5, 10)
  levels.LAGSHARE <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  levels.LINYEARS <- c(1, 5, 10, 15)

  # Set a counter to use for file names
  i <- 1

  # Set up a list to store scenario information objects
  scenObjects <- list()

  # Loop over all LOGIT.AGROFOREST options
  for(agFor in levels.AGROFOREST) {
    # Loop over all levels.AGROFOREST_NONPASTURE options
    for(agForNonPast in levels.AGROFOREST_NONPASTURE) {
      # Loop over all levels.CROPLAND
      for(crop in levels.CROPLAND) {
        # Run Perfect expectations
        scenName <- getScenName(SCENARIO, "Perfect", NULL, agFor, agForNonPast, crop)

        currScenInfo <- ScenarioInfo(aScenario = SCENARIO,
                                      aExpectationType = "Perfect",
                                      aLinearYears = NULL,
                                      aLaggedShareOld = NULL,
                                      aLogitUseDefault = FALSE,
                                      aLogitAgroForest = agFor,
                                      aLogitAgroForest_NonPasture = agForNonPast,
                                      aLogitCropland = crop,
                                      aScenarioName = scenName,
                                      aFileName = i,
                                      aOutputDir = aOutputDir)

        scenObjects[[i]] <- currScenInfo
        i <- i + 1

        # Loop over all TAU options and run Lagged
        for(share in levels.LAGSHARE) {
          scenName <- getScenName(SCENARIO, "Lagged", share, agFor, agForNonPast, crop)

          currScenInfo <- ScenarioInfo(aScenario = SCENARIO,
                                       aExpectationType = "Lagged",
                                       aLinearYears = NULL,
                                       aLaggedShareOld = share,
                                       aLogitUseDefault = FALSE,
                                       aLogitAgroForest = agFor,
                                       aLogitAgroForest_NonPasture = agForNonPast,
                                       aLogitCropland = crop,
                                       aScenarioName = scenName,
                                       aFileName = i,
                                       aOutputDir = aOutputDir)

          scenObjects[[i]] <- currScenInfo
          i <- i + 1
        }

        # Loop over all LINYEARS options and run Linear
        for(linyears in levels.LINYEARS) {
          scenName <- getScenName(SCENARIO, "Linear", linyears, agFor, agForNonPast, crop)
          currScenInfo <- ScenarioInfo(aScenario = SCENARIO,
                                       aExpectationType = "Linear",
                                       aLinearYears = linyears,
                                       aLaggedShareOld = NULL,
                                       aLogitUseDefault = FALSE,
                                       aLogitAgroForest = agFor,
                                       aLogitAgroForest_NonPasture = agForNonPast,
                                       aLogitCropland = crop,
                                       aScenarioName = scenName,
                                       aFileName = i,
                                       aOutputDir = aOutputDir)

          scenObjects[[i]] <- currScenInfo
          i <- i + 1
        }
      }
    }

  }

  # Loop over all scenario configurations and run the model
  foreach(obj = scenObjects) %dopar% {
    message("Starting simulation: ", obj$mFileName)
    run_model(obj)
  }
}

#' Generate the ensemble members for a single set of parameters
#'
#' This generates one each of the Perfect, Lagged, and Linear scenario types
#' using the input parameters.  The return value is a list of the three
#' ScenarioInfo objects for the scenarios generated.
#'
#' @param agFor The logit exponent the ag/forest(?) nest
#' @param agForNonPast The logit exponent for the non-pasture(?) nest
#' @param crop The logit exponent for the crop nest
#' @param share The share parameter for the lagged model
#' @param linyears The number of years parameter for the linear model
#' @param serialnum A serial number for generating unique file names
#' @param outdir Name of the output directory
#' @return List of three ScenarioInfo objects
#' @keywords internal
gen_ensemble_member <- function(agFor, agForNonPast, crop, share, linyears, serialnum, aOutputDir)
{
  ## Perfect expectations scenario
  scenName <- getScenName(SCENARIO, "Perfect", NULL, agFor, agForNonPast, crop)

  perfscen <- ScenarioInfo(aScenario = SCENARIO,
                           aExpectationType = "Perfect",
                           aLinearYears = NULL,
                           aLaggedShareOld = NULL,
                           aLogitUseDefault = FALSE,
                           aLogitAgroForest = agFor,
                           aLogitAgroForest_NonPasture = agForNonPast,
                           aLogitCropland = crop,
                           aScenarioName = scenName,
                           aFileName = sprintf("Perf_%04d", serialnum),
                           aOutputDir = aOutputDir)


  ## Lagged scenario
  scenName <- getScenName(SCENARIO, "Lagged", share, agFor, agForNonPast, crop)

  lagscen <- ScenarioInfo(aScenario = SCENARIO,
                          aExpectationType = "Lagged",
                          aLinearYears = NULL,
                          aLaggedShareOld = share,
                          aLogitUseDefault = FALSE,
                          aLogitAgroForest = agFor,
                          aLogitAgroForest_NonPasture = agForNonPast,
                          aLogitCropland = crop,
                          aScenarioName = scenName,
                          aFileName = sprintf("Lag_%04d", serialnum),
                          aOutputDir = aOutputDir)


  # Loop over all LINYEARS options and run Linear
  scenName <- getScenName(SCENARIO, "Linear", linyears, agFor, agForNonPast, crop)
  linscen <- ScenarioInfo(aScenario = SCENARIO,
                          aExpectationType = "Linear",
                          aLinearYears = linyears,
                          aLaggedShareOld = NULL,
                          aLogitUseDefault = FALSE,
                          aLogitAgroForest = agFor,
                          aLogitAgroForest_NonPasture = agForNonPast,
                          aLogitCropland = crop,
                          aScenarioName = scenName,
                          aFileName = sprintf("Lin_%04d", serialnum),
                          aOutputDir = aOutputDir)

  list(perfscen, lagscen, linscen)
}


#' run_model
#'
#' @details Loops through all years and runs the land model.
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @return Name of the output directory
#' @author KVC
#' @export
run_model <- function(aScenarioInfo, aPeriods=PERIODS) {
  ## Ensure that output directories exist
  odnorm <- outdir_setup(aScenarioInfo$mOutputDir)

  if(length(aPeriods) < 1) {
      ## This is mostly here to facilitate testing.
      return(invisible(odnorm))
  }

  # Initialize LandAllocator and read in calibration data
  mLandAllocator <- LandAllocator(REGION)
  LandAllocator_setup(mLandAllocator, aScenarioInfo)

  # Loop through each period and run the model
  # TODO: put model running in a function, add loop on regions
  for(per in aPeriods){
    message("Starting period: ", per)

    # First, call initCalc for AgProductionTechnology (via Sector) and LandAllocator
    # Note: AgProductionTechnology must be called first so profits
    #       can be set before LandAllocator can be calibrated
    Sector_initCalc(mLandAllocator, per, aScenarioInfo)
    LandAllocator_initCalc(mLandAllocator, per)

    # Next, call calcFinalLandAllocation for LandAllocator
    LandAllocator_calcFinalLandAllocation(mLandAllocator, per)
  }

  # Print Outputs
  message("All model periods complete. Starting output.")
  printOutput(mLandAllocator, aScenarioInfo)

  # Make figures
  if(MAKE.PLOTS) {
    message("Plotting diagnostic figures.")
    plotNest(mLandAllocator, aScenarioInfo)
    plotLandAllocation(mLandAllocator, aScenarioInfo)
    plotRegionalLandAllocation(mLandAllocator, aScenarioInfo)
  }
  return(invisible(odnorm))
}



