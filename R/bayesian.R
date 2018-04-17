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
    GCAM_commodity <- variable <- year <- area <- obsvar <-
        region <- land.type <- obs <- NULL

    FAO_land_history <- gcamland::FAO_land_history

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
      mutate(obsvar = stats::var(obs)) %>%
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
#' \code{ll_} that gives the log pointwise probability density.
#'
#' The variance levels \eqn{\xi} are used to calculate the scale parameter
#' required by the lpdf.  For each grouping of region, land type (i.e., GCAM
#' commodity), and variable (e.g., land area), we calculate a variance
#' \eqn{\varsigma^2_g} for the grouping.  Then, the scale factor for all of the
#' data points in the grouping is calculated as \eqn{\sigma_g = \sqrt{\xi
#' \varsigma^2_g}}.  If multiple \eqn{\xi} values are passed, this process is
#' repeated for each one, and the results are combined into a single table.
#'
#' @param aScenarioInfo ScenarioInfo object for the scenario
#' (q.v. \code{\link{get_historical_land_data}}) for a \emph{single} model.
#' @param obs Table of observational data (q.v. \code{\link{add_parameter_data}})
#' @param lpdf Log probability density function (q.v. \code{\link{get_lpdf}})
#' @param varlvls Variance levels to run (see details).
#' @return Data frame containing xi and ll_
#' @export
calc_loglikelihood <- function(aScenarioInfo, obs, lpdf, varlvls)
{
    ## silence package checks
    land.type <- variable <- region <- NULL

    model <- get_scenario_land_data(aScenarioInfo)

    jointbl <- dplyr::inner_join(obs, model,
                                 by=(c('region','land.type','variable','year')))

    lapply(varlvls,
           function(xi) {
               sig <- sqrt(xi*jointbl$obsvar)
               ll_ <- lpdf(jointbl$model-jointbl$obs, sig)
               dplyr::bind_cols(jointbl, data.frame(xi=xi, ll_=ll_))
           }) %>%
      dplyr::bind_rows()
}


#' Compute the log-prior for a paremeter set
#'
#' The land model parameters are extracted from the ScenarioInfo object
#' supplied.  The \code{xi} parameter isn't stored in the ScenarioInfo
#' structure, so it must be supplied separately.  Multiple \code{xi} values in a
#' vector are allowed.  The prior must also be supplied as a function.  It
#' should take a named list of parameter vectors and compute the log prior for
#' those parameters
#'
#' The design of the prior function is trying to walk a tightrope between
#' convenience, flexibility, and extensibility.  Right now the parameters are
#' expectation.type (character), share.old and linear.years (double, only
#' defined for certain types), logit.agforest, logit.afnonpast, logit.crop, and
#' xi (all double, and all defined for all models.  The purpose of passing them
#' in a list is to ensure that we can easily add new parameters if we want to.
#'
#' Also note that because a couple of the parameters are defined only for
#' certain expectation types, a prior function will have to handle those cases
#' correctly.
#'
#' Finally, note that the list \emph{actually} passed to the prior function may
#' contain extra junk variables (to avoid having to waste time dropping columns
#' that aren't needed.)  The prior function should ignore these.
#'
#' @param aScenarioInfo ScenarioInfo structure for the scenario.
#' @param xi Vector of xi parameter values
#' @param prior Function that calculates the log-prior (see details).  Default
#' applies a uniform prior.
#' @return vector of prior values
#' @export
calc_prior <- function(aScenarioInfo, xi, prior)
{
    d <- data.frame(xi=xi) %>%
      add_parameter_data(aScenarioInfo)

    prior(d)
}


#' Compute the log-likelihood and log-posteriors for a set of parameters
#'
#' The log-posterior is just the sum over the pointwise likelihoods, plus the
#' prior for the given parameter set.
#'
#' @param aScenarioInfo ScenarioInfo structure for the scenario
#' @param obs Table of observational data (q.v. \code{\link{add_parameter_data}})
#' @param lpdf Log probability density function (q.v. \code{\link{get_lpdf}})
#' @param prior Log prior function (q.v. \code{\link{calc_prior}})
#' @param varlvls Variance levels to run (see description in
#' \code{\link{calc_loglikelihood}}).
#' @return Modified ScenarioInfo with pointwise likelihood and model posterior
#' tables.
#' @export
calc_post <- function(aScenarioInfo, obs, lpdf = get_lpdf(1), prior = function(x){0},
                      varlvls = seq(0.1, 1, 0.1))
{
    ## silence package checks
    xi <- ll_ <- NULL

    ll_point <- calc_loglikelihood(aScenarioInfo, obs, lpdf, varlvls)
    aScenarioInfo$mPointwiseLikelihood <- ll_point

    lpost <- dplyr::group_by(ll_point, xi) %>%
      dplyr::summarise(lp_ = sum(ll_)) %>%
      ungroup

    ## Add the prior
    lpost$lp_ <- lpost$lp_ + calc_prior(aScenarioInfo, lpost$xi, prior)
    aScenarioInfo$mLogPost <- lpost

    ## return the modified scenario object
    aScenarioInfo
}


#' Run all the Bayesian analysis on a list of scenarios
#'
#' The assumption is that the scenarios have already been run.  The best way to
#' arrange this is to use the return value of \code{\link{run_ensemble}} as the
#' argument to this function
#'
#' @section TODO:
#'
#' \itemize{
#' \item{Offer some more control over likelihood and prior functions, xi values,
#' etc.}
#' \item{Offer control over years, land types.}
#' \item{Compute WAIC values in this function.}
#' }
#'
#' @param aScenarioList List of ScenarioInfo structures
#' @return Modified list of ScenarioInfo structures with the Bayesian
#' calculation tables populated.
#' @export
run_bayes <- function(aScenarioList)
{
    rgns <- unique(sapply(aScenarioList, function(s) {s$mRegion}))
    obsdata <- get_historical_land_data(rgns)

    invisible(
        lapply(aScenarioList,
               function(s) {
                   calc_post(s, obsdata)
               }))
}


#' Organize a list of ScenarioInfo objects into a grand table of parameters
#'
#' The table produced includes the model parameters and log-posterior
#' probability for all of the models in the input list.
#'
#' Before this function can run, the scenario list must have been run through
#' \code{\link{run_bayes}} to compute the posterior probability densities.
#'
#' Note that the table produced here has \emph{nearly} everything needed to do
#' inference with the models, but some statistics, such as the WAIC, still
#' require the pointwise data stored in the ScenarioInfo structures.
#'
#' @param aScenarioList List of ScenarioInfo structures
#' @return Data frame containing model parameters and log-posterior
#' probabilities.
#' @export
grand_table <- function(aScenarioList)
{
    lapply(aScenarioList,
           function(s) {
               add_parameter_data(s$mLogPost, s)
           }) %>%
      dplyr::bind_rows()
}
