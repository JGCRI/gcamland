# main.R

#' run_model
#'
#' @details Loops through all years and runs the land model.
#' @author KVC
#' @export
run_model <- function() {
  # Initialize LandAllocator and read in calibration data
  mLandAllocator <- LandAllocator(REGION)
  LandAllocator_readData(mLandAllocator)

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
    # TODO: Figure out when/what to call from AgProductionTechnology to make sure
    #       profits are calculated/sent before land is allocated
    LandAllocator_calcFinalLandAllocation(mLandAllocator, per)
  }

  # node <- mLandAllocator$mChildren[[1]]
  # for ( leaf in node$mChildren ) {
  #   print(paste("DEBUG: profit is ", leaf$mProfitRate))
  #   print(paste("DEBUG: share is ", leaf$mShare))
  #   print(paste("DEBUG: share weight is ", leaf$mShareWeight))
  # }

  plot_LandAllocation(mLandAllocator)
  plot_Nest(mLandAllocator)
}



