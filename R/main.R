# main.R

#' Run an ensemble of offline land models
#'
#' Parameter combinations are selected by generating a quasi-random
#' sequence and mapping it to a specified range for each parameter.
#' Then, each parameter set is run through the offline land model in
#' each of the Perfect, Lagged, and Linear variants.  (I.e., if N
#' parameter sets are selected, then 3N scenarios are run.)
#'
#' @param N Number of parameter sets to select
#' @param aOutputDir Output directory
#' @param atype Scenario type: either "Reference" or "Hindcast"
#' @return List of ScenarioInfo objects for the ensemble members
#' @import foreach doParallel
#' @author KVC November 2017
#' @export
run_ensemble <- function(N = 500, aOutputDir = "./outputs", atype="Hindcast") {
  # Silence package checks
  obj <- NULL

  NPARAM <- 4   # There are actually 5 parameters, but only one of lagshare
                # or linyears is used in a single model design.

  ## Set options for ensembles
  ## min and max values for each parameter
  limits.AGROFOREST <- c(0.1, 10)
  limits.AGROFOREST_NONPASTURE <- c(0.1, 10)
  limits.CROPLAND <- c(0.1, 10)
  limits.LAGSHARE <- c(0.1, 0.9)
  limits.LINYEARS <- round(c(1, 15))

  rn <- randtoolbox::sobol(N, NPARAM)
  scl <- function(fac, limits) {limits[1] + fac*(limits[2]-limits[1])}
  levels.AGROFOREST <- scl(rn[,1], limits.AGROFOREST)
  levels.AGROFOREST_NONPASTURE <- scl(rn[,2], limits.AGROFOREST_NONPASTURE)
  levels.CROPLAND <- scl(rn[,3], limits.CROPLAND)
  levels.LAGSHARE <- scl(rn[,4], limits.LAGSHARE)
  levels.LINYEARS <- round(scl(rn[,4], limits.LINYEARS))  # reuse rn[,4] because
                                        # lagshare and linyears are mutually
                                        # exclusive

  # Set up a list to store scenario information objects
  scenObjects <- Map(gen_ensemble_member,
                     levels.AGROFOREST, levels.AGROFOREST_NONPASTURE, levels.CROPLAND,
                     levels.LAGSHARE, levels.LINYEARS,
                     seq_along(levels.AGROFOREST), atype, aOutputDir) %>%
    unlist(recursive=FALSE)

  # Loop over all scenario configurations and run the model
  foreach(obj = scenObjects) %dopar% {
    message("Starting simulation: ", obj$mFileName)
    run_model(obj)
  }
  invisible(scenObjects)
}

#' Generate the ensemble members for a single set of parameters
#'
#' This generates one each of the Perfect, Lagged, and Linear scenario types
#' using the input parameters.  The return value is a list of the three
#' \code{ScenarioInfo} objects for the scenarios generated.
#'
#' @param agFor The logit exponent the ag/forest nest, which controls
#' competition between pasture and all other arable land.
#' @param agForNonPast The logit exponent for the non-pasture nest, which
#' controls competition between crops, grass/shrub, and forest.
#' @param crop The logit exponent for the crop nest
#' @param share The share parameter for the lagged model
#' @param linyears The number of years parameter for the linear model
#' @param serialnum A serial number for generating unique file names
#' @param scentype Scenario type, either "Hindcast" or "Reference"
#' @param outdir Name of the output directory
#' @return List of three ScenarioInfo objects
#' @keywords internal
gen_ensemble_member <- function(agFor, agForNonPast, crop, share, linyears,
                                serialnum, scentype, aOutputDir)
{
  ## Perfect expectations scenario
  scenName <- getScenName(scentype, "Perfect", NULL, agFor, agForNonPast, crop)

  perfscen <- ScenarioInfo(aScenario = scentype,
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
  scenName <- getScenName(scentype, "Lagged", share, agFor, agForNonPast, crop)

  lagscen <- ScenarioInfo(aScenario = scentype,
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


  ## Linear scenario
  scenName <- getScenName(scentype, "Linear", linyears, agFor, agForNonPast, crop)
  linscen <- ScenarioInfo(aScenario = scentype,
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


#' Run the GCAM land model
#'
#' Loop through all years and run the land model.
#'
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations.
#' @param aPeriods Integer vector of periods to run.  Default is all periods
#' defined for the scenario type.
#' @return Name of the output directory for the run.
#' @author KVC
#' @export
run_model <- function(aScenarioInfo, aPeriods=NULL) {
  ## Ensure that output directories exist
  odnorm <- outdir_setup(aScenarioInfo$mOutputDir)

  if(is.null(aPeriods))
      aPeriods <- PERIODS[[aScenarioInfo$mScenarioType]]

  if(length(aPeriods) < 1) {
      ## This is mostly here to facilitate testing.
      return(invisible(odnorm))
  }

  # Initialize LandAllocator and read in calibration data
  mLandAllocator <-
      LandAllocator(aScenarioInfo$mRegion,
                    TIME.PARAMS[[aScenarioInfo$mScenarioType]]$FINAL_CALIBRATION_PERIOD)
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



