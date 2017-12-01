# main.R

#' run_ensembles
#'
#' @details Loop over a large parameter set and run the offline land model
#' @import foreach
#' @author KVC November 2017
#' @export
run_ensembles <- function() {
  # Set options for ensembles
  levels.AGROFOREST <- c(1)
  levels.AGROFOREST_NONPASTURE <- c(1)
  levels.CROPLAND <- c(1)
  levels.LAGSHARE <- c(0.1, 0.9)
  levels.LINYEARS <- c(1, 10)

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
                                      aFileName = i)

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
                                       aFileName = i)

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
                                       aFileName = i)

          scenObjects[[i]] <- currScenInfo
          i <- i + 1
        }
      }
    }

  }

  # Loop over all scenario configurations and run the model
  foreach(obj = scenObjects) %do% run_model(obj)
}


#' run_model
#'
#' @details Loops through all years and runs the land model.
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations
#' @author KVC
#' @export
run_model <- function(aScenarioInfo) {
  print(paste0("Starting simulation: ", aScenarioInfo$mFileName))

  # Initialize LandAllocator and read in calibration data
  mLandAllocator <- LandAllocator(REGION)
  LandAllocator_setup(mLandAllocator, aScenarioInfo)

  # Loop through each period and run the model
  # TODO: put model running in a function, add loop on regions
  for(per in PERIODS){
    print(paste("Starting period:", per))

    # First, call initCalc for AgProductionTechnology (via Sector) and LandAllocator
    # Note: AgProductionTechnology must be called first so profits
    #       can be set before LandAllocator can be calibrated
    Sector_initCalc(mLandAllocator, per, aScenarioInfo)
    LandAllocator_initCalc(mLandAllocator, per)

    # Next, call calcFinalLandAllocation for LandAllocator
    LandAllocator_calcFinalLandAllocation(mLandAllocator, per)
  }

  # Print Outputs
  print("All model periods complete. Starting output.")
  printOutput(mLandAllocator, aScenarioInfo)

  # Make figures
  if(MAKE.PLOTS) {
    print("Plotting diagnostic figures.")
    plotNest(mLandAllocator)
    plotLandAllocation(mLandAllocator, aScenarioInfo)
    plotRegionalLandAllocation(mLandAllocator, aScenarioInfo)
  }
}



