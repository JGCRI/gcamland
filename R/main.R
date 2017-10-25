# main.R

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
    # TODO: Figure out when/what to call from AgProductionTechnology to make sure
    #       profits are calculated/sent before land is allocated
    LandAllocator_calcFinalLandAllocation(mLandAllocator, per)
  }

  node <- mLandAllocator$mChildren[[1]]
  node2 <- node$mChildren[[1]]
  node3 <- node2$mChildren[[2]]
  for ( leaf in node3$mChildren ) {
    print(paste("DEBUG: name is ", leaf$mName))
    print(paste("DEBUG: profit is ", leaf$mProfitRate))
     print(paste("DEBUG: share is ", leaf$mShare))
     print(paste("DEBUG: share weight is ", leaf$mShareWeight))
  }

  plot_Nest(mLandAllocator)
  plot_LandAllocation(mLandAllocator)
}



