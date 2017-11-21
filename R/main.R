# main.R

#' run_ensembles
#'
#' @details Loop over a large parameter set and run the offline land model
#' @author KVC November 2017
#' @export
run_ensembles <- function() {
  # Save all information
  currFileName <- FILE.NAME
  currScenName <- SCENARIO.NAME
  currLogitOption <- LOGIT.USE.DEFAULT
  currAgFor <- LOGIT.AGROFOREST
  currAgForNonPast <- LOGIT.AGROFOREST_NONPASTURE
  currCrop <- LOGIT.CROPLAND
  currExpectationType <- EXPECTATION.TYPE
  currTau <- LAGGED.TAU
  currLinYears <- LINEAR.YEARS

  # Set options for ensembles
  levels.AGROFOREST <- c(1)
  levels.AGROFOREST_NONPASTURE <- c(1)
  levels.CROPLAND <- c(1)
  levels.TAU <- c(1, 10)
  levels.LINYEARS <- c(1, 10)

  # Set a counter to use for file names
  i <- 0

  # Set logit to use read in values (default currently not supported in these loops)
  LOGIT.USE.DEFAULT <- FALSE

  # Loop over all LOGIT.AGROFOREST options
  for(agFor in levels.AGROFOREST) {
    LOGIT.AGROFOREST <- agFor

    # Loop over all levels.AGROFOREST_NONPASTURE options
    for(agForNonPast in levels.AGROFOREST_NONPASTURE) {
      LOGIT.AGROFOREST_NONPASTURE <- agForNonPast

      # Loop over all levels.CROPLAND
      for(crop in levels.CROPLAND) {
        LOGIT.CROPLAND <- crop

        # Run Perfect expectations
        EXPECTATION.TYPE <- "Perfect"
        SCENARIO.NAME <- getScenName(SCENARIO, EXPECTATION.TYPE, NULL, LOGIT.AGROFOREST,
                                     LOGIT.AGROFOREST_NONPASTURE, LOGIT.CROPLAND)
        FILE.NAME <- i
        print(paste("Starting run number ", FILE.NAME, ":", SCENARIO.NAME))
        run_model()
        i <- i + 1

        # Loop over all TAU options and run Lagged
        for(tau in levels.TAU) {
          EXPECTATION.TYPE <- "Lagged"
          LAGGED.TAU <- tau
          SCENARIO.NAME <- getScenName(SCENARIO, EXPECTATION.TYPE, tau, LOGIT.AGROFOREST,
                                       LOGIT.AGROFOREST_NONPASTURE, LOGIT.CROPLAND)
          FILE.NAME <- i
          print(paste("Starting run number ", FILE.NAME, ":", SCENARIO.NAME))
          run_model()
          i <- i + 1
        }

        # Loop over all LINYEARS options and run Linear
        for(linyears in levels.LINYEARS) {
          EXPECTATION.TYPE <- "Lagged"
          LINEAR.YEARS <- linyears
          SCENARIO.NAME <- getScenName(SCENARIO, EXPECTATION.TYPE, linyears, LOGIT.AGROFOREST,
                                       LOGIT.AGROFOREST_NONPASTURE, LOGIT.CROPLAND)
          FILE.NAME <- i
          print(paste("Starting run number ", FILE.NAME, ":", SCENARIO.NAME))
          run_model()
          i <- i + 1
        }
      }
    }

  }


  # Reset information
  FILE.NAME <- currFileName
  SCENARIO.NAME <- currScenName
  LOGIT.USE.DEFAULT <- currLogitOption
  LOGIT.AGROFOREST <- currAgFor
  LOGIT.AGROFOREST_NONPASTURE <- currAgForNonPast
  LOGIT.CROPLAND <- currCrop
  EXPECTATION.TYPE <- currExpectationType
  LAGGED.TAU <- currTau
  LINEAR.YEARS <- currLinYears
}


#' run_model
#'
#' @details Loops through all years and runs the land model.
#' @author KVC
#' @export
run_model <- function() {
  # Initialize LandAllocator and read in calibration data
  mLandAllocator <- LandAllocator(REGION)
  LandAllocator_setup(mLandAllocator)

  # Loop through each period and run the model
  # TODO: put model running in a function, add loop on regions
  for(per in PERIODS){
    print(paste("Starting period:", per))

    # First, call initCalc for AgProductionTechnology (via Sector) and LandAllocator
    # Note: AgProductionTechnology must be called first so profits
    #       can be set before LandAllocator can be calibrated
    Sector_initCalc(mLandAllocator, per)
    LandAllocator_initCalc(mLandAllocator, per)

    # Next, call calcFinalLandAllocation for LandAllocator
    LandAllocator_calcFinalLandAllocation(mLandAllocator, per)
  }

  # Print Outputs
  print("All model periods complete. Starting output.")
  printOutput(mLandAllocator)

  # Make figures
  if(MAKE.PLOTS) {
    print("Plotting diagnostic figures.")
    plotNest(mLandAllocator)
    plotLandAllocation(mLandAllocator)
    plotRegionalLandAllocation(mLandAllocator)
  }
}



