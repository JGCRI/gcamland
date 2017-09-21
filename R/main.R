# main.R

#' run_model
#'
#' @details Loops through all years and runs the land model.
#' @author KVC
#' @export
run_model <- function() {
  # Initialize LandAllocator and read in calibration data
  mLandAllocator <- LandAllocator(REGION, 0, -1)
  LandAllocator_readData(mLandAllocator)

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
    # TODO: Figure out when/what to call from AgProductionTechnology to make sure
    #       profits are calculated/sent before land is allocated
    LandAllocator_calcFinalLandAllocation(mLandAllocator, per)
  }

  for ( leaf in mLandAllocator$mChild$mChildren ) {
    print(paste("DEBUG: yield is ", leaf$mYield))
    print(paste("DEBUG: profit is ", leaf$mProfitRate))
  }

  plot_LandAllocation(mLandAllocator)
}



