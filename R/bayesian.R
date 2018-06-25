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
    land.type <- year <- land.allocation <- scenario <- NULL # silence package notes

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
          ## split name / AEZ
          tidyr::extract('name', c('land.type', 'AEZ'),
                         '(.+)(AEZ[0-9]+)') %>%
          dplyr::group_by(land.type, year, scenario) %>%
          dplyr::summarise(model = sum(land.allocation)) %>%
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
#' @keywords internal
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
                                  share.old = s$mLaggedShareOld,
                                  linear.years = s$mLinearYears,
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
#' @param obs Table of observational data (q.v. \code{\link{add_parameter_data}})
#' @param model Table of model outputs
#' (q.v. \code{\link{get_scenario_land_data}})
#' @param lpdf Log probability density function (q.v. \code{\link{get_lpdf}})
#' @param varlvls Variance levels to run (see details).
#' @return Data frame containing xi and ll_
#' @keywords internal
calc_loglikelihood <- function(obs, model, lpdf, varlvls)
{
    ## silence package checks
    land.type <- variable <- region <- NULL

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
#' xi (all double, and all defined for all models).  The purpose of passing them
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
#' @param lprior Function that calculates the log-prior (see details).  Default
#' applies a uniform prior.
#' @return Vector of log-prior values
#' @keywords internal
calc_prior <- function(aScenarioInfo, xi, lprior)
{
    d <- tibble::tibble(xi=xi, scenario=aScenarioInfo$mScenarioName) %>%
      add_parameter_data(aScenarioInfo)

    lprior(d)
}


#' Compute the log-likelihood and log-posteriors for a set of parameters
#'
#' The log-posterior is just the sum over the pointwise likelihoods, plus the
#' prior for the given parameter set.
#'
#' @param aScenarioInfo ScenarioInfo structure for the scenario
#' @param obs Table of observational data (q.v. \code{\link{add_parameter_data}})
#' @param model Table of model results for the scenario.
#' @param lpdf Log probability density function (q.v. \code{\link{get_lpdf}})
#' @param lprior Log prior function (q.v. \code{\link{calc_prior}})
#' @param varlvls Variance levels to run (see description in
#' \code{\link{calc_loglikelihood}}).
#' @return Modified ScenarioInfo with pointwise likelihood and model posterior
#' tables.
#' @keywords internal
calc_post <- function(aScenarioInfo, obs, model, lpdf, lprior,
                      varlvls = seq(0.1, 1, 0.1))
{
    ## silence package checks
    xi <- ll_ <- NULL

    ll_point <- calc_loglikelihood(obs, model, lpdf, varlvls)
    aScenarioInfo$mPointwiseLikelihood <- ll_point

    lpost <- dplyr::group_by(ll_point, xi) %>%
      dplyr::summarise(lp_ = sum(ll_)) %>%
      ungroup

    ## Add the prior
    lpost$lp_ <- lpost$lp_ + calc_prior(aScenarioInfo, lpost$xi, lprior)
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
#' The \code{years} and \code{landtypes} arguments can be use to restrict the
#' observations that will be used in the analysis.  The regions are always
#' filtered to excatly the regions that are included in the ScenarioInfo
#' structures.
#'
#' Typically log-probaility density functions are parameterized with a parameter
#' (conventionally called \eqn{\sigma}) that describes how quickly the
#' probability density falls off as the discrepancy between the model outputs
#' and observed data grows.  We use a slightly modified version of this
#' convention.  Because we look at a variety of different land use types, each
#' of which can have completely different scale.  Therefore, we let each land
#' use type have its own \eqn{\sigma} value.  We tie these values together using
#' a single parameter \eqn{\xi}, which represents the fraction of the total
#' variance of the observed values for a land use type that is allocated to that
#' land use's \eqn{\sigma}.  In other words, for a land type \eqn{i}, with
#' observations \eqn{O_i},
#' \deqn{\sigma^2_i = \xi \var(O_i).}
#' A single call to \code{run_bayes} will produce posteriors for a range of
#' \eqn{\xi} values, by default \eqn{\xi = 0.1 \ldots 1.0} in steps of 0.1.
#'
#' @section TODO:
#'
#' \itemize{
#' \item{Offer some more control over xi values,
#' etc.}
#' }
#'
#' @param aScenarioList List of ScenarioInfo structures
#' @param years Vector of years to filter observations to (default is to use all
#' available years)
#' @param landtypes Vector of land types to filter observations to (default is
#' to use all available land types)
#' @param lpdf Log probability density function (q.v. \code{\link{get_lpdf}})
#' @param lprior Log prior function (q.v. \code{\link{calc_prior}}).
#' @return Modified list of ScenarioInfo structures with the Bayesian
#' calculation tables populated.
#' @export
run_bayes <- function(aScenarioList, years=NULL, landtypes=NULL,
                      lpdf = get_lpdf(1), lprior = uniform_prior)
{
    rgns <- unique(sapply(aScenarioList, function(s) {s$mRegion}))
    obsdata <- get_historical_land_data(rgns, years, landtypes)
    modeldata <- get_scenario_land_data(aScenarioList)

    invisible(
        lapply(aScenarioList,
               function(s) {
                   calc_post(s, obsdata, modeldata[[s$mScenarioName]],
                             lpdf=lpdf, lprior=lprior)
               }))
}

### A uniform prior to serve as the default in run_bayes
uniform_prior <- function(params) {0}

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
    ## silence package checks
    scenario <- NULL

    tbllen <- sapply(aScenarioList, function(s) {length(s$mLogPost)})
    if(any(tbllen) == 0) {
        warning('grand_table: One or more scenarios do not have posterior probability tables. Running run_bayes with default arguments.')
        aScenarioList <- run_bayes(aScenarioList)
    }

    modata <-
        lapply(aScenarioList,
               function(s) {
                   lpost <- s$mLogPost
                   lpost$scenario <- s$mScenarioName
                   lpost
           }) %>%
          dplyr::bind_rows()

    add_parameter_data(modata, aScenarioList) %>%
      dplyr::select(-scenario)
}


#' Calculate MAP (maximum a posteriori) estimates for a collection of model runs
#'
#' The MAP estimate is the estimate in each model group with the highest
#' posterior probability density.  The results are reported in a data frame that
#' contains the MAP values of all of the parameters, for
#' all model groups, along with the in-sample deviance.
#'
#' @param samples Monte Carlo samples, given either as a grand table or a list
#' of \code{ScenarioInfo} objects
#' @param modelgroup Vector of names of columns that define the model groupings.
#' The default is the single column \code{expectation.type}.
#' @param reportvars Vector of names of variables for which to report
#' expectations.  The default is all parameter values.
#' @param lp Name of the column containing the log posterior
#' probability.  Ignored if \code{weighted==FALSE}.
#' @export
MAP <- function(samples, modelgroup='expectation.type', reportvars=NULL,
                lp='lp_')
{
    if(!inherits(samples, 'data.frame')) {
        ## Check that this is a scenario list
        if( !inherits(samples, 'list') || !all(sapply(samples, is.ScenarioInfo)))
            stop('EV: samples must be a data frame or list of ScenarioInfo objects.')
        samples <- grand_table(samples)
    }

    if(is.null(reportvars)) {
        ## Use default values of reportvars
        reportvars <- c('logit.agforest', 'logit.afnonpast', 'logit.crop',
                        'share.old', 'linear.years', 'xi')
    }

    samples_by_model <- split(samples, samples[,modelgroup])
    maprows <-
        lapply(
            samples_by_model,
            function(d) {
                k <- which.max(d[[lp]])
                mapval <- d[k,c(modelgroup, reportvars)]
                mapval[['dev_']] <- -2.0*d[[lp]][k]
                mapval
            })
    bind_rows(maprows)
}


#' Calculate the parameter expectation values for a collection of model runs.
#'
#' Use the parameter sample values to compute expectation values for the
#' parameters.  The samples can be either MCMC samples or uniform samples.  In
#' the latter case, the values will be weighted by their posterior
#' probabilities.
#'
#' The input to this function can be given either as a grand table
#' (q.v. \code{\link{grand_table}}) or as a list of \code{ScenarioInfo}
#' objects.  Generally this collection will have several model families
#' represented, so the table is split according to the model type.  The result
#' will be a table of expectation values by model
#'
#' @param samples Monte Carlo samples, given either as a grand table or a list
#' of \code{ScenarioInfo} objects
#' @param modelgroup Vector of names of columns that define the model groupings.
#' The default is the single column \code{expectation.type}.
#' @param reportvars Vector of names of variables for which to report
#' expectations.  The default is all parameter values.
#' @param weighted If \code{TRUE}, weight the samples by their posterior.
#' @param lp Name of the column containing the log posterior
#' probability.  Ignored if \code{weighted==FALSE}.
#' @export
EV <- function(samples, modelgroup='expectation.type', reportvars=NULL,
               weighted=TRUE, lp='lp_')
{
    if(!inherits(samples, 'data.frame')) {
        ## Check that this is a scenario list
        if( !inherits(samples, 'list') || !all(sapply(samples, is.ScenarioInfo)))
            stop('EV: samples must be a data frame or list of ScenarioInfo objects.')
        samples <- grand_table(samples)
    }

    if(is.null(reportvars)) {
        ## Use default values of reportvars
        reportvars <- c('logit.agforest', 'logit.afnonpast', 'logit.crop',
                        'share.old', 'linear.years', 'xi')
    }

    samples_by_model <- split(samples, samples[,modelgroup])
    evtbls <- lapply(X=samples_by_model, FUN=ev_single, modelgroup=modelgroup,
                     reportvars=reportvars, weighted=weighted, lp=lp)
    bind_rows(evtbls)
}

#' Helper function for EV
#'
#' @param samples Data frame of Monte Carlo samples for a single model.
#' @param modelgroup Vector of names of columns that define the model
#' groupings.  These will be included in the results along with the
#' \code{reportvars}.
#' @param reportvars Vector of names of variables for which to report
#' expectations.
#' @param weighted If \code{TRUE}, weight samples by their posterior
#' probabilities.
#' @param lp Name of column containing the log posterior.
#' @keywords internal
ev_single <- function(samples, modelgroup, reportvars, weighted, lp)
{
    outtbl <- unique(samples[,modelgroup])
    if(weighted) {
        logwgt <- samples[[lp]]
        wgt <- exp(logwgt - max(logwgt))
        fac <- 1.0/sumx(wgt)
        for(col in reportvars) {
            outtbl[[col]] <- sum(wgt*samples[[col]]) * fac
        }
    }
    else {
        for(col in reportvars) {
            outtbl[[col]] <- mean(samples[[col]])
        }
    }
    outtbl
}


#' Calculate highest posterior density interval (HPDI) for a set of samples
#'
#' The HPDI is the interval that contains a specified fraction of the sample
#' points, ordered by posterior probability density.
#'
#' @param samples Monte Carlo samples, given either as a grand table or a list
#' of \code{ScenarioInfo} objects
#' @param interval The fraction of samples to be contained in the interval
#' @param modelgroup Vector of names of columns that define the model groupings.
#' The default is the single column \code{expectation.type}.
#' @param reportvars Vector of names of variables for which to report
#' expectations.  The default is all parameter values.
#' @param weighted If \code{TRUE}, weight the samples by their posterior.
#' @param lp Name of the column containing the log posterior
#' probability.
#' @return List of matrices, one element for each model. Each matrix has
#' parameters in rows and the upper/lower bounds of the interval in its two
#' columns.
#' @export
HPDI <- function(samples, interval = 0.95, modelgroup = 'expectation.type', reportvars=NULL,
                 weighted=TRUE, lp='lp_')
{
    if(!inherits(samples, 'data.frame')) {
        ## Check that this is a scenario list
        if( !inherits(samples, 'list') || !all(sapply(samples, is.ScenarioInfo)))
            stop('EV: samples must be a data frame or list of ScenarioInfo objects.')
        samples <- grand_table(samples)
    }

    if(is.null(reportvars)) {
        ## Use default values of reportvars
        reportvars <- c('logit.agforest', 'logit.afnonpast', 'logit.crop',
                        'share.old', 'linear.years', 'xi')
    }

    samples_by_model <- split(samples, samples[,modelgroup])
    lapply(X=samples_by_model, FUN=hpdi_single, interval=interval,
           reportvars=reportvars, weighted=weighted, lp=lp)
}

hpdi_single <- function(samples, interval, reportvars, weighted, lp)
{
    samples <- samples[order(samples[[lp]], decreasing=TRUE),]
    if(weighted) {
        logwt <- samples[[lp]]
        wgt <- exp(logwt-max(logwt))
        fac <- 1.0/sumx(wgt)
        wgt <- wgt * fac

        kmax <- which.max(cumsum(wgt) >= interval) # bet you didn't know you
                                        # could do this.
    }
    else {
        kmax <- as.integer(ceiling(interval*nrow(samples)))
    }
    samples <- samples[1:kmax,]
    mat <- t(sapply(reportvars, function(var) {c(min(samples[[var]]),
                                                 max(samples[[var]]))}))
    rownames(mat) <- reportvars
    colnames(mat) <- c(paste('|',interval),
                       paste(interval,'|'))
    mat
}


