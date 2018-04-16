#### Functions for doing Bayesian inference on model runs

#' Get historical land use data
#'
#' Fetch the historical land use data and filter to the desired regions, years,
#' and GCAM commodities.
#'
#' All of the runs in a single ensemble should have the same regions, years, and
#' commodities, so generally it will only be necessary to run this function once
#' at the beginning of the analysis.  The resulting data can be matched to the
#' model results for each of the models run in order to calculate Bayesian
#' likelihoods.
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
    filter <- rep(TRUE, nrow(FAO_land_history))
    if(!is.null(regions))
        filter <- filter & FAO_land_history$region %in% regions
    if(!is.null(years))
        filter <- filter & FAO_land_history$year %in% years
    if(!is.null(commodities))
        filter <- filter & FAO_land_history$GCAM_commodity %in% commodities

    FAO_land_history[filter,]
}


#' Load land use results for an already-run scenario
#'
#' Fetch land use results and aggregate to region, commodity, and year.
#'
#' @param aScenarioInfo ScenarioInfo structure for the run.
#' @return Table with region, commodity, year, and area.
#' @export
get_scenario_land_data <- function(aScenarioInfo)
{
    land.type <- year <- land.allocation <- NULL # silence package notes

    outputdir <- aScenarioInfo$mOutputDir
    filename <- paste0('landAllocation_',aScenarioInfo$mFileName,'.csv')
    fn <- file.path(outputdir, 'land', filename)

    readr::read_csv(fn) %>%
      ## split name / AEZ
      tidyr::extract('name', c('land.type', 'AEZ'),
                     '(.+)(AEZ[0-9]+)') %>%
      dplyr::group_by(land.type, year) %>%
      dplyr::summarise(land.allocation = sum(land.allocation))
}
