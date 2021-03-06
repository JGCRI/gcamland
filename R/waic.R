#' Compute the WAIC for a list of scenarios
#'
#' The WAIC is Watanabe's "Widely Applicable Information Criterion".  It
#' uses a model's performance on the training set to estimate its out of sample
#' performance.
#'
#' To calculate the WAIC, you need a collection of samples from the parameter
#' space.  Typically these samples would be collected through a Markov chain
#' Monte Carlo process, so that parameter combinations appear in the samples
#' proportional to their posterior probability density.  However, it is also
#' possible to use uniform density samples, weighted by their posterior
#' probability density.  This function supports both modes.
#'
#' The WAIC applies to the model as a whole.  That is, it provides an estimate
#' of the expectation \emph{deviance} (what we loosely called "performance"
#' above) when we use the moel to generate predictions for some data set
#' \emph{not} used to fit the model.  These predictions are assumed to be
#' generated by sampling from the posterior PDF of the parameters and averaging
#' together the predictions from the sampled parameter set.  If you just take
#' the modal parameter set from the PDF, your predictions will likely be biased
#' relative to future observations.  (Note, however, that there are ways to
#' approximate what you would have gotten by sampling without actually doing the
#' sampling.)
#'
#' The WAIC can be used to compare models with different parameterizations and
#' even different numbers of parameters.  The difference between the WAIC for
#' different models provides an indication of the expected difference in
#' out-of-sample deviance between the models.  You can also use the WAIC values
#' to calculate \emph{Akaike weights}, which can be used for model averaging
#' (q.v. \code{akaike_wgt} for more details)
#'
#' The return value is a data frame containing WAIC results, one row for each model
#' family represented in the input.
#' The WAIC result table has these columns:
#' \describe{
#'   \item{waic}{The WAIC value for the model.}
#'   \item{lppd}{The log predictive probability density.  The in-sample deviance
#' is -2 times this value.}
#'   \item{pwaic}{The effective number of parameters in the model.  This can and
#' often will be less than the actual number of parameters (and it need not be
#' an integer).}
#'   \item{se}{An estimate of the standard error for the WAIC value.}
#'   \item{dwaic}{Difference between a model's WAIC and the WAIC of the model
#' with the lowest WAIC in the input set.}
#'   \item{se.dwaic}{Standard error for \code{dwaic}.}
#'   \item{awgt}{Akaike weight for each model.}
#' }
#' The first four of these are characteristics of the individual model they are
#' calculated for.  The last three, \code{dwaic}, \code{se.dwaic}, and \code{awgt} are joint
#' properties of the collection of models in the input.
#'
#' @param aScenarioList List of \code{ScenarioInfo} structures.
#' @param weighted If \code{TRUE}, treat the scenarios as uniform samples and
#' weight them by their posterior probability densities.  Otherwise, treat them
#' as Markov chain Monte Carlo samples and do not weight them.
#' @return Data frame containing a table of statistics based on WAIC
#' calculations.
#' @export
waic <- function(aScenarioList, weighted=TRUE)
{
    ## Do some basic type checking
    if(!is.list(aScenarioList)) {
        stop('waic requires a list of scenarios to operate on')
    }
    if(!all(sapply(aScenarioList, is.ScenarioInfo))) {
        stop('Objects passed to waic must be ScenarioInfo objects.')
    }

    ## Check that all of the scenario objects have had their likelihoods and
    ## posterior probabilities calculated.
    if(!all(sapply(aScenarioList, function(x) {nrow(x$mPointwiseLikelihood) > 0}))) {
        warning("waic: Some scenarios don't have posterior probabilities calculated. ",
                "Posteriors will be calculated with default priors.")
        aScenarioList <- run_bayes(aScenarioList)
    }

    scenarios_by_type <-
        split(aScenarioList,
              as.factor(sapply(aScenarioList, function(x){x$mExpectationType})))


    ## for each model, compute a matrix of lp values and (if weighted==TRUE) a
    ## vector of log-weights.
    logprob_by_type <- lapply(X=scenarios_by_type, FUN=pointwise_loglike, weighted=weighted)

    model <- names(scenarios_by_type)
    lppd_lst <- lapply(logprob_by_type, lppd)    # Each item in the list is a model
    pwaic_lst <- lapply(logprob_by_type, pwaic)  # same
    waic <- mapply(function(lppd, pwaic) {-2.0 * (sum(lppd) - sum(pwaic))},
                   lppd_lst, pwaic_lst)          # produces one value for each model,
                                                 # organized into a vector
    ## Get the total lppd and pwaic for each model so we can include them in the
    ## output
    lppd_mod <- sapply(lppd_lst, sum)
    pwaic_mod <- sapply(pwaic_lst, sum)
    ## Calculate the Akaike weight.
    awgt <- softmax(-0.5*waic)


    ## standard error of the WAIC is sqrt(n_obs * var(waic_obs)), where waic_obs
    ## is -2*(lppd-pwaic) for each observation.  McElreath uses the population
    ## variance (n instead of n-1 in the denominator), but I don't see a good
    ## reason for that.  It doesn't really matter that much anyhow.  If your
    ## sample size is so small that n and n-1 are dramatically different, you're
    ## probably doing something wrong.
    se <- mapply(
        function(lppd, pwaic) {
            nobs <- length(lppd)
            waic <- -2.0 * (lppd - pwaic)
            sqrt(nobs * stats::var(waic))
        },
        lppd_lst, pwaic_lst)


    ## Find the model with the smallest ("best") WAIC.  Compute the WAIC
    ## difference between each model and that "best" model.
    dwaic <- waic - min(waic)

    ## Compute an estimate of the standard error on these differences.  The
    ## technique is similar to how we do the se for the WAIC, except we use
    ## pointwise WAIC differences instead of pointwise WAIC
    imin <- which.min(waic)
    waic0 <- -2.0*(lppd_lst[[imin]] - pwaic_lst[[imin]])  # pointwise WAIC
                                        # for lowest-WAIC model
    se.dwaic <- mapply(
        function(lppd, pwaic) {
            wm <- -2.0*(lppd - pwaic) # pointwise waic estimate for model
            dw <- wm - waic0
            nobs <- length(wm)
            ## needless to say, this will fail if the number of observations
            ## is different between the two models.  More subtly, it will be
            ## meaningless if the observations are not the same between the
            ## two models.
            sqrt(nobs * stats::var(dw))
        },
        lppd_lst, pwaic_lst)


    ## Return as a data frame with an additional class attribute so we can
    ## potentially define some methods (e.g. plots) that work on that class
    perm <- order(waic)
    structure(
        data.frame(model=model,
                   waic=waic, se=se,
                   lppd=lppd_mod, pwaic=pwaic_mod,
                   dwaic=dwaic, se.dwaic=se.dwaic,
                   awgt=awgt)[perm,],
        class=c('modelcompare','data.frame'))
}

#' Return pointwise log likelihood density for all samples in a model
#'
#' Produce a matrix of log likelihood density for each observational data point in
#' each parameter sample for the model.  The result is a matrix with data points
#' in rows and model samples in columns.  If the samples are uniform (i.e.,
#' instead of MCMC), then the samples will need to be weighted by their
#' posterior probabilities.  In this case, a vector of log weight factors is
#' also produced.
#'
#' @param aScenarioList A list of ScenarioInfo structures.  They should all be
#' from the same model (i.e., the same expectation type).
#' @param weighted Flag indicating whether posterior probability weight factors
#' should also be produce
#' @return A list with the matrix of log likelihood density values and, if
#' \code{weighted==TRUE}, a vector of log posterior probabilities for weighting.
#' @keywords internal
pointwise_loglike <- function(aScenarioList, weighted)
{
    loglike <- do.call(cbind,
                       lapply(aScenarioList, extract_pointwise_loglike))
    if(weighted) {
        weight <- unlist(lapply(aScenarioList, extract_post_vector))
    }
    else {
        weight <- NULL
    }

    list(loglike=loglike, weight=weight)
}

#' Extract the pointwise log likelihood values from a \code{ScenarioInfo} structure
#'
#' One "scenario" will generally produce several samples because the likelihood
#' calculations for all of the \code{xi} values will be stored in a single
#' scenario object.
#'
#' @param aScenarioInfo ScenarioInfo structure to extract from
#' @return Matrix of log likelihood values, points in rows, parameter samples in
#' columns
#' @keywords internal
extract_pointwise_loglike <- function(aScenarioInfo)
{
    ## ensure that the log-likelihood table is in order by xi values
    lltbl <- aScenarioInfo$mPointwiseLikelihood
    permvec <- order(lltbl$xi)
    lltbl <- lltbl[permvec,]

    ## turn the column of ll_ values into a matrix with each xi value in a
    ## column.  Since each xi value has the same number of points, we can do
    ## this the easy way
    nxi <- length(unique(lltbl$xi))
    matrix(lltbl$ll_, ncol=nxi)
}

#' Extract the posterior vector for the samples in a \code{ScenarioInfo} object
#'
#' @param aScenarioInfo ScenarioInfo object
#' @return vector of log posterior values.
#' @keywords internal
extract_post_vector <- function(aScenarioInfo)
{
    ## Order the table by xi values so that we can ensure that these values
    ## correspond to the columns from extract_pointwise_loglike
    ptbl <- aScenarioInfo$mLogPost
    permvec <- order(ptbl$xi)
    ptbl[permvec, 'lp_'][[1]]
}

#' Compute the log of the sum of values given as logs
#'
#' Compute \code{log(sum(exp(x)))}, but do so in a way that avoids underflow.
#'
#' Use the following identity to stabilize the calculation:
#' \deqn{\sum \exp(x-x_0) = 1/x0 \sum \exp(x)}
#'
#' @param lx Vector of logs
#' @return log of the sum of the antilogs of the input values.
#' @export
log_sum <- function(lx)
{
    lx0 <- max(lx)
    log(sumx(exp(lx-lx0))) + lx0
}


#' Compute the softmax function of a vector
#'
#' The softmax of a vector of variables \eqn{x_i} is
#' \deqn{s_i = \frac{\exp(x_i)}{\sum_{j=1}^{N} \exp(x_j)}}
#'
#' @param x Vector of values for which to compute softmax
#' @return Vector of softmax values
#' @export
softmax <- function(x)
{
    ## Note that softmax is invariant to a constant shift in its arguments, so
    ## shift everything such that the largest x value is 0.
    x0 <- max(x)
    xx <- x-x0
    expx <- exp(xx)
    denom <- sumx(expx)
    expx / denom
}

#' Compute the log pointwise predictive density for a single model.
#'
#' @param pointwise_ll List returned from \code{\link{pointwise_loglike}}.
#' @return A vector of lppd values averaged over Monte Carlo samples.  Each
#' observation will have one corresponding pont in the vector.
#' @keywords internal
lppd <- function(pointwise_ll)
{
    llmat <- pointwise_ll$loglike
    logwgt <- pointwise_ll$weight

    ## If weights are in effect, apply them.  The weights and likelihoods are
    ## both logs, so we add them to get log(wgt*L)
    if(!is.null(logwgt)) {
        llmat <- sapply(seq(1,ncol(llmat)),
                        function(i) {logwgt[i] + llmat[,i]})
        logfac <- log_sum(logwgt)
    }
    else {
        logfac <- log(ncol(llmat))
    }

    ## For each point average over all samples.  Samples are in rows, so we
    ## apply the sum over that margin.
    apply(llmat, 1, log_sum) - logfac
}


#' Compute the effective number of parameters \code{pwaic} for a model.
#'
#' For each observation, compute the variance of the log likelihood.  Note that
#' we are computing the variance of the \emph{log} likelihood.  The total
#' \code{pwaic} for the model is the sum of these, but we return the individual
#' values for use in computing standard errors.
#'
#' @param pointwise_ll List returned from \code{\link{pointwise_loglike}}.
#' @return A vector of pwaic values averaged over Monte Carlo samples.  Each
#' observation will have one corresponding point in the vector.
#' @keywords internal
pwaic <- function(pointwise_ll)
{
    llmat <- pointwise_ll$loglike
    logwgt <- pointwise_ll$weight
    nc <- ncol(llmat)

    if(is.null(logwgt)) {
        llvar <- apply(llmat, 1, stats::var)
    }
    else {
        ## scale weights to avoid excessively large or small numbers
        wgt <- exp(logwgt - max(logwgt))
        fac <- 1.0/sumx(wgt)
        wgtllmat <- sapply(seq(1,nc),
                           function(i) {wgt[i] * llmat[,i]})
        llmean <- apply(wgtllmat, 1, sum) * fac

        ## Now calculate the squared differences.  Note that if
        ## nrow(m)==length(v), then m-v subtracts v from each column of m.
        sqdiff <- (llmat-llmean)^2
        ## apply weights to the squared difference
        wgtsqdiff <- sapply(seq(1,nc),
                            function(i) {wgt[i] * sqdiff[,i]})

        llvar <- apply(wgtsqdiff, 1, sum) * fac
    }

    llvar
}

