# main.R

#' Run an ensemble of offline land models
#'
#' Parameter combinations are selected by generating a quasi-random
#' sequence and mapping it to a specified range for each parameter.
#' Then, each parameter set is run through the offline land model in
#' each of the Perfect, Lagged, and Linear variants.  (I.e., if N
#' parameter sets are selected, then 3N scenarios are run.)
#'
#' @section Output:
#' The scenario results are written to a series of files in the specified output
#' directory.  There is a subdirectory for each output type, and output from
#' each scenario is written in its own file in the relevant subdirectory.  The
#' list of \code{ScenarioInfo} objects is written to a file called
#' \code{scenario-info.rds} in the output directory.  This file can be loaded
#' with a command such as \code{scenaro_list <-
#' readRDS('output/scenario-info.rds')}
#'
#' @param N Number of parameter sets to select
#' @param aOutputDir Output directory
#' @param skip Number of iterations to skip (i.e., if building on another run.)
#' @param atype Scenario type: either "Reference" or "Hindcast"
#' @param logparallel Name of directory to use for parallel workers' log files.
#' If \code{NULL}, then don't write log files.
#' @return List of ScenarioInfo objects for the ensemble members
#' @import foreach doParallel
#' @author KVC November 2017
#' @importFrom utils capture.output sessionInfo
#' @export
run_ensemble <- function(N = 500, aOutputDir = "./outputs", skip = 0, atype="Hindcast",
                         logparallel=NULL) {
  # Silence package checks
  obj <- NULL

  NPARAM <- 4   # There are actually 5 parameters, but only one of lagshare
                # or linyears is used in a single model design.

  ## Set options for ensembles
  ## min and max values for each parameter
  limits.AGROFOREST <- c(0.1, 6)
  limits.AGROFOREST_NONPASTURE <- c(0.1, 6)
  limits.CROPLAND <- c(0.1, 6)
  limits.LAGSHARE <- c(0.1, 0.9)
  limits.LINYEARS <- round(c(2, 20))

  serialnumber <- skip + (1:N)
  rn <- randtoolbox::sobol(N+skip, NPARAM)
  rn <- rn[serialnumber,]
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
                     levels.LAGSHARE, levels.LINYEARS, serialnumber,
                     atype, aOutputDir) %>%
    unlist(recursive=FALSE)

  serialized_scenObjs <- lapply(scenObjects, as.list) # Convert to a list to survive serialization

  # Loop over all scenario configurations and run the model
  rslt <-
      foreach(obj = serialized_scenObjs, .combine=rbind) %dopar% {
          if(!is.null(logparallel)) {
              nn <- Sys.info()['nodename']
              sn <- obj$mScenarioName
              fn <- file.path(logparallel, paste0('info-', sn, '.txt'))
              print(fn)
              logfil <- file(fn, 'w')
              if(!file.exists(fn)) {
                  stop("Couldn't create logfile: ", fn)
              }
              writeLines(c('nodename= ', nn), con=logfil)
              writeLines(capture.output(sessionInfo()),con=logfil)
              flush(logfil)
          }

          if(N <= 50)  {
              message("Starting simulation: ", obj$mScenarioName)
          }

          si <- as.ScenarioInfo(obj)
          if(N > 50) {
              rslt <- suppressMessages(run_model(si))
          }
          else {
              rslt <- run_model(si)
          }

          message("Finished: ", obj$mSerialNumber)

          if(!is.null(logparallel)) {
              writeLines(capture.output(warnings()), con=logfil)
              close(logfil)
          }
          rslt
      }

  message("Result is ", nrow(rslt), "rows, ", ncol(rslt), "columns, total size: ",
          format(object.size(rslt), units="auto"))

  ## Save the scenario info from the scenarios that we ran
  suffix <- paste0("-",skip)
  filebase <- paste0("scenario-info", suffix, ".rds")
  scenfile <- file.path(aOutputDir, filebase)
  saveRDS(scenObjects, scenfile)

  ## Save the full set of ensemble results
  filebase <- paste0("output_ensemble", suffix, ".rds")
  outfile <- file.path(aOutputDir, filebase)
  saveRDS(rslt, outfile)

  message("Output directory is", aOutputDir)
  message("scenario file: ", scenfile)
  message("output file: ", outfile)

  warnings()

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
#' @param serialnum Serial number for the run
#' @param scentype Scenario type, either "Hindcast" or "Reference"
#' @param outdir Name of the output directory
#' @return List of three ScenarioInfo objects
#' @keywords internal
gen_ensemble_member <- function(agFor, agForNonPast, crop, share, linyears,
                                serialnum, scentype, aOutputDir)
{
  ## Perfect expectations scenario
  scenName <- getScenName(scentype, "Perfect", NULL, agFor, agForNonPast, crop)

  perfscen <- ScenarioInfo(aScenarioType = scentype,
                           aExpectationType = "Perfect",
                           aLinearYears = NA,
                           aLaggedShareOld = NA,
                           aLogitUseDefault = FALSE,
                           aLogitAgroForest = agFor,
                           aLogitAgroForest_NonPasture = agForNonPast,
                           aLogitCropland = crop,
                           aScenarioName = scenName,
                           aFileName = "ensemble",
                           aSerialNum = serialnum+0.1,
                           aOutputDir = aOutputDir)


  ## Lagged scenario
  scenName <- getScenName(scentype, "Lagged", share, agFor, agForNonPast, crop)

  lagscen <- ScenarioInfo(aScenarioType = scentype,
                          aExpectationType = "Lagged",
                          aLinearYears = NA,
                          aLaggedShareOld = share,
                          aLogitUseDefault = FALSE,
                          aLogitAgroForest = agFor,
                          aLogitAgroForest_NonPasture = agForNonPast,
                          aLogitCropland = crop,
                          aScenarioName = scenName,
                          aFileName = "ensemble",
                          aSerialNum = serialnum+0.2,
                          aOutputDir = aOutputDir)


  ## Linear scenario
  scenName <- getScenName(scentype, "Linear", linyears, agFor, agForNonPast, crop)
  linscen <- ScenarioInfo(aScenarioType = scentype,
                          aExpectationType = "Linear",
                          aLinearYears = linyears,
                          aLaggedShareOld = NA,
                          aLogitUseDefault = FALSE,
                          aLogitAgroForest = agFor,
                          aLogitAgroForest_NonPasture = agForNonPast,
                          aLogitCropland = crop,
                          aScenarioName = scenName,
                          aFileName = "ensemble",
                          aSerialNum = serialnum+0.3,
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
#' @param aVerbose If \code{TRUE}, output additional debugging information.
#' @return Table of model results.
#' @author KVC
#' @export
run_model <- function(aScenarioInfo, aPeriods=NULL, aVerbose=FALSE) {
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
  ## Write the output to a file only if verbose mode is set
  rslt <- printOutput(mLandAllocator, aScenarioInfo, aFileOutput=aVerbose)
  if(aVerbose) {
      printDebug(mLandAllocator, aScenarioInfo)
  }

  # Make figures
  if(MAKE.PLOTS) {
    message("Plotting diagnostic figures.")
    plotNest(mLandAllocator, aScenarioInfo)
    plotLandAllocation(mLandAllocator, aScenarioInfo)
    plotRegionalLandAllocation(mLandAllocator, aScenarioInfo)
  }
  return(invisible(rslt))
}



