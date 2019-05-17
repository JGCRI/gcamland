# main.R

#' Run an ensemble of offline land models
#'
#' Parameter combinations are selected by generating a quasi-random
#' sequence and mapping it to a specified range for each parameter.
#' Then, each parameter set is run through the offline land model in
#' each of the Perfect, Lagged, and Linear variants.  (I.e., if N
#' parameter sets are selected, then 3N scenarios are run.)
#'
#' If the scenario type is "Hindcast", then after each model has been run, the
#' Bayesian analysis will be run so that its results can be stored with the rest
#' of the ScenarioInfo structure.
#'
#' @section Output:
#' The model results are written to a series of files in the specified output
#' directory.
#' The
#' list of \code{ScenarioInfo} objects is written to a file called
#' \code{scenario-info.rds} in the output directory.  This file can be loaded
#' with a command such as \code{scenaro_list <-
#' readRDS('output/scenario-info.rds')}.  These objects contain links to the
#' model output files, as well as the posterior probability density tables, if
#' the Bayesian analysis was run.
#'
#' @param N Number of parameter sets to select
#' @param aOutputDir Output directory
#' @param skip Number of iterations to skip (i.e., if building on another run.)
#' @param lpdf Log-likelihood function.  Used only if Bayesian posteriors are
#' being run.
#' @param lprior Log-prior probability density function.  Used only if Bayesian
#' posteriors are being run.
#' @param atype Scenario type: either "Reference" or "Hindcast"
#' @param logparallel Name of directory to use for parallel workers' log files.
#' If \code{NULL}, then don't write log files.
#' @return List of ScenarioInfo objects for the ensemble members
#' @import foreach doParallel
#' @author KVC November 2017
#' @importFrom utils capture.output sessionInfo
#' @export
run_ensemble <- function(N = 500, aOutputDir = "./outputs", skip = 0,
                         lpdf=get_lpdf(1), lprior=uniform_prior,
                         atype="Hindcast", logparallel=NULL) {
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

  ## Filename suffix.  This will be used to create unique filenames for the
  ## outputs across all worker processes, continuation runs, etc.
  suffix <- sprintf("-%06d",skip)

  # Set up a list to store scenario information objects
  scenObjects <- Map(gen_ensemble_member,
                     levels.AGROFOREST, levels.AGROFOREST_NONPASTURE, levels.CROPLAND,
                     levels.LAGSHARE, levels.LINYEARS, serialnumber,
                     atype, suffix, aOutputDir) %>%
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
          format(utils::object.size(rslt), units="auto"))

  ## Save the full set of ensemble results
  filebase <- paste0("output_ensemble", suffix, ".rds")
  outfile <- file.path(aOutputDir, filebase)
  saveRDS(rslt, outfile)

  if(atype == "Hindcast") {
      ## For hindcast runs, calculate the Bayesian posteriors
      scenObjects <- run_bayes(scenObjects, lpdf=lpdf, lprior=lprior)
  }

  ## Save the scenario info from the scenarios that we ran
  filebase <- paste0("scenario-info", suffix, ".rds")
  scenfile <- file.path(aOutputDir, filebase)
  saveRDS(scenObjects, scenfile)

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
#' @param suffix Suffix for output filenames.
#' @param aOutputDir Name of the output directory.
#' @return List of three ScenarioInfo objects
#' @keywords internal
gen_ensemble_member <- function(agFor, agForNonPast, crop, share, linyears,
                                serialnum, scentype, suffix, aOutputDir)
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
                           aFileName = paste0("ensemble", suffix),
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
                          aFileName = paste0("ensemble", suffix),
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
                          aFileName = paste0("ensemble", suffix),
                          aSerialNum = serialnum+0.3,
                          aOutputDir = aOutputDir)

  list(perfscen, lagscen, linscen)
}


#' Run the GCAM land model
#'
#' Loop through all years and run the land model.
#'
#' @param aScenarioInfo A structure containing scenario-related information,
#' created by \code{\link{ScenarioInfo}}.  There is a pre-built structure for the
#' default scenario called \code{\link{SCENARIO.INFO}}.
#' @param aPeriods Integer vector of periods to run.  Default is all periods
#' defined for the scenario type.
#' @param aVerbose If \code{TRUE}, output additional debugging information.
#' @param agData Ag data read by \code{\link{ReadData_AgProd}}.  The data must
#' be for the same region and scenario type as the \code{aScenarioInfo} object.
#' @return Table of model results.
#' @author KVC
#' @importFrom assertthat assert_that has_attr
#' @export
#' @examples
#' \dontrun{
#' run_model(SCENARIO.INFO, aVerbose=TRUE)
#' run_model(SCENARIO.INFO, aPeriods = 1:5)
#' }
run_model <- function(aScenarioInfo, aPeriods=NULL, aVerbose=FALSE, agData=NULL) {

  #### Step 1: Setup
  # Ensure that output directories exist
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
  LandAllocator_setup(mLandAllocator, aScenarioInfo, agData, SUBREGION=aScenarioInfo$mSubRegion)

  # Loop through each period and run the model
  # TODO: put model running in a function, add loop on regions
  for(per in aPeriods){
    message("Starting period: ", per, " (", get_per_to_yr(per, aScenarioInfo$mScenarioType), ")")

    #### Step 2: Initial calculation
    # First, call initCalc for AgProductionTechnology (via Sector) and LandAllocator
    # Note: AgProductionTechnology must be called first so profits
    #       can be set before LandAllocator can be calibrated
    Sector_initCalc(mLandAllocator, per, aScenarioInfo)
    LandAllocator_initCalc(mLandAllocator, per, aScenarioInfo)

    #### Step 3: Final calculation
    # Next, call calcFinalLandAllocation for LandAllocator
    LandAllocator_calcFinalLandAllocation(mLandAllocator, per, aScenarioInfo)
  }

  #### Step 4: Reporting
  # Print Outputs
  message("All model periods complete. Starting output.")
  ## Write the output to a file only if verbose mode is set
  rslt <- printOutput(mLandAllocator, aScenarioInfo, aFileOutput=aVerbose)
  if(aVerbose) {
    message("Printing diagnostic information.")
    printDebug(mLandAllocator, aScenarioInfo)
    plotNest(mLandAllocator, aScenarioInfo)
  }

  return(invisible(rslt))
}


#' Export scenario results as a csv file
#'
#' Filter the scenario information object for a particular scenario and export its results
#'
#' @param aScenarioInfo Scenario-related information, including names, logits, expectations.
#' @return Table of model results.
#' @importFrom readr write_csv
#' @author KVC
#' @export
export_results <- function(aScenarioInfo) {
  scenario <- NULL             # silence package checker.

  # Get file name where results are curently stored
  inFile <- paste0(aScenarioInfo$mOutputDir, "/output_", aScenarioInfo$mFileName, ".rds")

  # Read land allocation
  allLand <- suppressMessages(readRDS(normalizePath(inFile)))

  # Filter for requested scenario
  allLand %>%
    dplyr::filter(scenario == aScenarioInfo$mScenarioName) ->
    scenResults

  # Get file name to store outputs
  file <- paste0(aScenarioInfo$mOutputDir, "/output_", aScenarioInfo$mScenarioName, ".csv")
  write_csv(scenResults, file)

  return(invisible(scenResults))
}




