# utility-functions-analysis.R
#
# utility functions for analysis
# pulled out of `bayesian.R` because they're useful
# in both a bayesian approach and an objective function approach.

#' Get historical land use data
#'
#' Fetch the historical land use data and filter to the desired regions, years,
#' and GCAM commodities.
#'
#' All of the runs in a single ensemble should have the same regions, years, and
#' commodities, so generally it will only be necessary to run this function once
#' at the beginning of the analysis.  The resulting data can be matched to the
#' model results for each of the models run in order to calculate Bayesian
#' likelihoods and objective function evaluations.
#'
#' @param regions Regions to keep in filtered data. (Default is use all regions.)
#' @param years Years to keep in filtered data. (Default is use all years.)
#' @param commodities GCAM commodities to keep in filtered data (Default is to
#' use all commodities.)
#' @return Data frame containing the filtered data.
#' @export
get_historical_land_data <- function(regions = NULL, years = NULL,
                                     commodities = NULL)
{
  ## silence notes
  GCAM_commodity <- variable <- year <- area <- obsvar <- trend <- detrended <-
    region <- land.type <- obs <- NULL

  Land_history <- gcamland::Land_history

  filter <- rep(TRUE, nrow(Land_history))
  if(!is.null(regions))
    filter <- filter & Land_history$region %in% regions
  if(!is.null(years))
    filter <- filter & Land_history$year %in% years
  if(!is.null(commodities))
    filter <- filter & Land_history$GCAM_commodity %in% commodities

  Land_history[filter,] %>%
    dplyr::mutate(variable="Land Area") %>%
    dplyr::select(region, land.type=GCAM_commodity, variable, year, obs=area) %>%
    group_by(land.type, variable, region) %>%
    mutate(trend = lm(obs ~ year)$fitted.values) %>%
    mutate(detrended = obs - trend) %>%
    mutate(obsvar = stats::var(detrended)) %>%
    ungroup
}



#' Load land use results for a list of already-run scenarios
#'
#' Fetch land use results and aggregate to region, commodity, year, and
#' scenario.
#'
#' The name of the input file is constructed from the ScenarioInfo structure's
#' \code{mOutputDir} and \code{mFileName} attributes.  It is permissible to have
#' multiple output directories and/or file names in the list of structures
#' passed as an argument.  Each file will be read only once.
#'
#' The return value is a list of data frames, indexed by the scenario name.
#' The data frames
#' returned this way have columns for region, land-type, year, and area.
#'
#' @section TODO:
#'
#' Currently the land use results don't have a region column, so we can't
#' aggregate by region (we have to assume that the results are all for a single
#' region).  Once we add a region column to the model output, we will want to
#' add region to the grouping variables for this function.
#'
#' @param aScenarioList List of ScenarioInfo structures for the runs.
#' @return List of data frames containing land data.
#' @export
get_scenario_land_data <- function(aScenarioList)
{
  land.type <- year <- harvested.land <- scenario <-
    land.allocation <- name <- land <- NULL # silence package notes

  if(inherits(aScenarioList, "ScenarioInfo")) {
    ## user passed a single scenario.  Convert it to a list and press on
    aScenarioList <- list(aScenarioList)
  }

  outputdirs <- sapply(aScenarioList, function(s) {s$mOutputDir})
  filestems <- sapply(aScenarioList, function(s) {s$mFileName})
  ## Construct filenames, then filter out dupes
  filenames <- unique(construct_landdata_filename(outputdirs, filestems))

  ## For now we have to assume a single region.  See TODO note above.
  region <- unique(sapply(aScenarioList, function(s) {s$mRegion}))
  if(length(region) > 1) {
    stop('get_scenario_land_data: scenarios must all be from a single region')
  }


  ## Read and process the data from a single land data file. Returns a data
  ## frame with the requested data.  We will apply this to each file from the
  ## list we obtained above.
  read_landdata_file <- function(fn) {
    readRDS(fn) %>%
      dplyr::rename(land.type = name) %>%
      dplyr::mutate(land = if_else(is.na(harvested.land), land.allocation, harvested.land),
                    land.type = sub("Unmanaged", "", land.type)) %>%
      dplyr::group_by(land.type, year, scenario) %>%
      dplyr::summarise(model = sum(land)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(variable = "Land Area") %>%
      dplyr::mutate(region = region)
  }

  landdata <-
    lapply(filenames, read_landdata_file) %>%
    dplyr::bind_rows()

  split(landdata, landdata$scenario)
}


## Combine a directory name and filename stem to get:
##    /path/to/dir/output_stem.rds
## Either dir or stem, or both, can be a vector.
construct_landdata_filename <- function(dir, stem)
{
  fn <- paste0("output_", stem, ".rds")
  file.path(dir,fn)
}


#' Add model parameter values to a table of model results
#'
#' @param modeldata Data frame with model results
#' @param aScenarioList List of ScenarioInfo structures for the scenarios
#' @export
add_parameter_data <- function(modeldata, aScenarioList)
{
  if(inherits(aScenarioList, "ScenarioInfo")) {
    ## user passed a single scenario.  Convert it to a list and press on
    aScenarioList <- list(aScenarioList)
  }

  ## Create a table of parameters by scenario name
  stbl <-
    lapply(aScenarioList, function(s) {
      tibble::tibble(expectation.type = s$mExpectationType,
                     share.old1 = s$mLaggedShareOld1,
                     share.old2 = s$mLaggedShareOld2,
                     share.old3 = s$mLaggedShareOld3,
                     linear.years1 = s$mLinearYears1,
                     linear.years2 = s$mLinearYears2,
                     linear.years3 = s$mLinearYears3,
                     logit.agforest = s$mLogitAgroForest,
                     logit.afnonpast = s$mLogitAgroForest_NonPasture,
                     logit.crop = s$mLogitCropland,
                     region = s$mRegion,
                     scenario = s$mScenarioName)
    }) %>%
    dplyr::bind_rows()

  ## join parameters to results.
  dplyr::left_join(modeldata, stbl, by="scenario")
}

