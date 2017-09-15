# main.R

#' run_model
#'
#' @details Loops through all years and runs the land model.
#' @author KVC
#' @export
run_model <- function() {
  # Initialize LandAllocator and read in calibration data
  mLandAllocator <- LandAllocator(REGION, 0, LAND_ALLOCATION)

  # Loop through each period and run the model
  # TODO: put model running in a function, add loop on regions
  for(per in PERIODS){
    print(paste("Starting period:", per))

    # First, call initCalc for AgProductionTechnology and LandAllocator
    # Note: AgProductionTechnology must be called first so profits
    #       can be set before LandAllocator can be calibrated
    AgProductionTechnology_initCalc(mLandAllocator, per)
    LandAllocator_initCalc(mLandAllocator, per)

    # Next, call calcFinalLandAllocation for LandAllocator
    # TODO: Figure out when/what to call from AgProductionTechnology to make sure
    #       profits are calculated/sent before land is allocated
    LandAllocator_calcFinalLandAllocation(mLandAllocator, per)
  }

  plot_LandAllocation(mLandAllocator)
}



