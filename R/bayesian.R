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
#' This function also computes the grouped variance for use in subsequent
#' calculations.
#'
#' @section TODO:
#'
#' Calculate obsvar as a detrended variance instead of total variance.
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
    GCAM_commodity <- variable <- year <- area <- obsvar <- NULL

    filter <- rep(TRUE, nrow(FAO_land_history))
    if(!is.null(regions))
        filter <- filter & FAO_land_history$region %in% regions
    if(!is.null(years))
        filter <- filter & FAO_land_history$year %in% years
    if(!is.null(commodities))
        filter <- filter & FAO_land_history$GCAM_commodity %in% commodities

    FAO_land_history[filter,] %>%
      dplyr::mutate(variable="Land Area") %>%
      dplyr::select(region, land.type=GCAM_commodity, variable, year, obs=area) %>%
      group_by(land.type, variable, region) %>%
      mutate(obsvar = var(obs)) %>%
      ungroup
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
      dplyr::summarise(model = sum(land.allocation)) %>%
      ungroup %>%
      dplyr::mutate(variable = "Land Area") %>%
      dplyr::mutate(region = aScenarioInfo$mRegion)
}


#' Add model parameter values to a table of model results
#'
#' @param df Data frame with model results
#' @param aScenarioInfo ScenarioInfo structure for the scenario
#' @keywords internal
add_parameter_data <- function(df, aScenarioInfo)
{
    df$expectation.type <- aScenarioInfo$mExpectationType
    df$share.old <- aScenarioInfo$mLaggedShareOld
    df$linear.years <- aScenarioInfo$mLinearYears
    df$logit.agforest <- aScenarioInfo$mLogitAgroForest
    df$logit.afnonpast <- aScenarioInfo$mLogitAgroForest_NonPasture
    df$logit.crop <- aScenarioInfo$mLogitCropland
    df$region <- aScenarioInfo$mRegion

    df
}

#' Select a log-probability density function
#'
#' Choose a log-probability density function from the family of Gossett's
#' t-distributions (including the normal distribution as a special case).
#'
#' The t-distributions are parameterized by a parameter \eqn{\nu} called the
#' "degrees of freedom".  Despite the name, this parameter need not be an
#' integer; however, it must be positive.  The smaller \eqn{\nu} is, the heavier
#' the tails of the distribution.  In the limit that \eqn{\nu \rightarrow \infty},
#' the distribution becomes equivalent to the normal distribution.  Therefore,
#' as a special case, passing \code{df = Inf} will return a normal distribution.
#'
#' The function returned from this generator should be called with a vector of
#' differences between the model data and observed data, and a vector of scale
#' factors \eqn{sigma}.  Together, these will be used to compute
#' t-scores; that is, scaled differences between the model data and observed
#' data: \eqn{t = (M-O)/\sigma}.
#'
#' The scaling factor \eqn{\sigma} is a parameter of the probability model.
#' Since the scales of the observed and model values depend on the commodity
#' and/or region, this will be a vector of the same length as the difference
#' vector.
#'
#' @param df Degrees of freedom for the distribution.
#' @return A function \code{lp(model-observation, sigma)}
#' @export
get_lpdf <- function(df)
{
    if(length(df) != 1) {
        stop("get_lpdf must be called with a single df value.")
    }
    if(df <= 0) {
        stop("get_lpdf: df must be > 0.")
    }
    ## df=Inf is explicitly allowed in stats::dt
    function(x, sig) {
        stats::dt(x/sig, df=df, log=TRUE) - log(sig)
    }
}


#' Compute the log-likelihoods for each data point
#'
#' Match historical data to observations and compute the log-likelihood for each
#' data point.  This will be done for a variety of variance levels, so the
#' result will be a table with an extra parameter \code{xi} and an output column
#' \code{lppd_} that gives the log pointwise probability density.
#'
#' The variance levels \eqn{\xi} are used to calculate the scale parameter
#' required by the lpdf.  For each grouping of region, land type (i.e., GCAM
#' commodity), and variable (e.g., land area), we calculate a variance
#' \eqn{\varsigma^2_g} for the grouping.  Then, the scale factor for all of the
#' data points in the grouping is calculated as \eqn{\sigma_g = \sqrt{\xi
#' \varsigma^2_g}}.  If multiple \eqn{\xi} values are passed, this process is
#' repeated for each one, and the results are combined into a single table.
#'
#' @param model Table of model outputs
#' (q.v. \code{\link{get_historical_land_data}}) for a \emph{single} model.
#' @param obs Table of observational data (q.v. \code{\link{add_parameter_data}})
#' @param lpdf Log probability density function (q.v. \code{\link{get_lpdf}})
#' @param varlvls Variance levels to run (see details).
#' @return Data frame containing xi and lppd_
#' @export
calc_lppd <- function(model, obs, lpdf = get_lpdf(1), varlvls = seq(0.1, 1, 0.1))
{
    ## silence package checks
    land.type <- variable <- region <- NULL

    jointbl <- dplyr::inner_join(obs, model,
                                 by=(c('region','land.type','variable','year')))

    lapply(varlvls,
           function(xi) {
               sig <- sqrt(xi*jointbl$obsvar)
               lp_ <- lpdf(jointbl$model-jointbl$obs, sig)
               list(xi=xi, lppd_=sum(lp_))
           }) %>%
      dplyr::bind_rows()
}

