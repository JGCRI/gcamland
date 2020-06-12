# objectivefun.R
# Objective functions for use in model evaluation across parameter sets.
# Can be used for  brute force approach.
# Also includes functions for analysis of objective function results,
# namely analogues to `grand_table`and `MAP` from bayesian.R



#' Run user-specified objective function evaluations on a list of scenarios
#'
#' The assumption is that the scenarios have already been run.  The best way to
#' arrange this is to use the return value of \code{\link{run_ensemble}} as the
#' argument to this function
#'
#' The \code{years} and \code{landtypes} arguments can be use to restrict the
#' observations that will be used in the analysis.  The regions are always
#' filtered to excatly the regions that are included in the ScenarioInfo
#' structures. The \code{objectivefuns} is vector of strings with the names of
#' objective functions to be evaluated. Currently, we support the following
#' objective functions:
#' \code{c('bias', 'rms', 'centeredrms', 'nrms', 'centerednrms', 'kge')}
#'
#' \code{c('bias', 'rms', 'centeredrms', 'nrms', 'centerednrms'} are defined and
#' examined in Snyder et al 2017 (https://doi.org/10.5194/gmd-10-4307-2017),
#' among other places.
#'
#' The definition for \code{'kge'} presented in Knoben et al 2019
#' (https://doi.org/10.5194/hess-23-4323-2019) is the one implemented here.
#'
#' @param aScenarioList List of ScenarioInfo structures
#' @param years Vector of years to filter observations to (default is to use all
#' available years)
#' @param landtypes Vector of land types to filter observations to (default is
#' to use all available land types). Currently, this is crop-type GCAM commodities
#' with observations from FAO via the \code{get_historical_land_data} function.
#' TODO: update to include choice of data sources and other land types, eg Forest.
#' @param objectivefuns User specified string of objective functions to calculate. Default
#' is to all supported objective functions:
#' c('bias', 'rms', 'centeredrms', 'nrms', 'centerednrms', 'kge')).
#' @return Modified list of ScenarioInfo structures with the objective
#' function calculation tables populated as a new list entry in ScenarioInfo:
#' ScenarioInfo$mObjFunEval.
#' @importFrom stats cor sd
#' @author ACS May 2020
#' @export
run_objective <- function(aScenarioList, years=NULL, landtypes=NULL,
                          objectivefuns = c('bias', 'rms', 'centeredrms', 'nrms', 'centerednrms', 'kge'))
{
  # Silence package checks
  region <- land.type <- variable <- obs <- model <- modelMean <- obsMean <-
    obsSD <- modelSD <- corr <- objfunval <- scenario <- year <- trend <-
    detrended <- obsvar <- . <- NULL

  # Pull the comparison data and model data
  rgns <- unique(sapply(aScenarioList, function(s) {s$mRegion}))
  obsdata <- get_historical_land_data(rgns, years, landtypes)
  modeldata <- get_scenario_land_data(aScenarioList)


  # for each scenario, do analysis for all objective functions specified in input
  # arguments:
  aScenarioListAnalyzed <- lapply(aScenarioList,  function(s) {

    # bring together observed and scenario specific model data for analysis.
    # Do this as an extra list entry to each scenario holding the objective function
    # evaluations: mObjFunAnalysis.
    # This is along the lines of the mPointwiseLikelihood, mLogPost list entries.
    modeldata[[s$mScenarioName]] %>%
      dplyr::inner_join(obsdata, .,
                        by=(c('region','land.type','variable','year'))) %>%
      # calculate many of the pieces that are commonly used across the different
      # objective functions:
      group_by(region, land.type, variable) %>%
      mutate(obsMean = mean(obs, na.rm = T),
             modelMean = mean(model, na.rm = T),
             obsSD = stats::sd(obs, na.rm = T) ) %>%
      ungroup ->
      s$mObjFunEval


    # Loop over all user specified objective functions to append that calculation
    # as its own column in this new list entry, s$mObjFunEval:
    for(objfun in objectivefuns) {

      if (objfun == 'bias') {

        s$mObjFunEval %>%
          group_by(region, land.type, variable) %>%
          mutate(bias = modelMean - obsMean) %>%
          ungroup  ->
          s$mObjFunEval

        } # end bias calculation


      if (objfun == 'rms') {

         s$mObjFunEval %>%
          group_by(region, land.type, variable) %>%
          mutate(rms = sqrt(mean( ((model-obs)^2) ) ) ) %>%
          ungroup ->
          s$mObjFunEval

      } # end rms  calculation


      if (objfun == 'centeredrms') {

        s$mObjFunEval %>%
          group_by(region, land.type, variable) %>%
          mutate(centeredrms = sqrt(mean( (( (model - modelMean) - (obs - obsMean)  )^2) ) ) ) %>%
          ungroup ->
          s$mObjFunEval

      } # end centeredrms  calculation


      if (objfun == 'nrms') {

        s$mObjFunEval %>%
          group_by(region, land.type, variable) %>%
          mutate(nrms = sqrt(mean( ((model-obs)^2) ) ) / obsSD ) %>%
          ungroup  ->
          s$mObjFunEval

      } # end nrms  calculation


      if (objfun == 'centerednrms') {

        s$mObjFunEval %>%
          group_by(region, land.type, variable) %>%
          mutate(centerednrms = sqrt(mean( (( (model - modelMean) - (obs - obsMean)  )^2) ) ) / obsSD) %>%
          ungroup  ->
          s$mObjFunEval

      } # end centeredrms  calculation


      if (objfun == 'kge') {

        s$mObjFunEval %>%
          group_by(region, land.type, variable) %>%
          mutate(modelSD = stats::sd(model, na.rm = T),
                 corr = stats::cor(model, obs),
                 kge = 1 - sqrt(  (corr - 1)^2  + ( (modelSD / obsSD) - 1 )^2 + ( (modelMean / obsMean) - 1 )^2 )) %>%
          ungroup %>%
          select(-modelSD, -corr) ->
          s$mObjFunEval

      } # end centeredrms  calculation

    }  # end for loop over objective function options


    # reshape the analyzed data for the scenario being analyzed;
    # take from having each obj fun evaluation in its own column
    # named from the measure name, holding the measured value to
    # two columns: measure name and value:
    s$mObjFunEval %>%
      select(-modelMean, -obsMean, -obsSD) %>%
      gather(objfun, objfunval, -scenario, -region, -land.type, -variable, -year, -obs, -trend,
             -detrended, -obsvar, -model) ->
      s$mObjFunEval


    # Finally, return the scenario environment with the added entry containing
    # objective function evaluations to its label:
    return(s)

  }) # end the lapply 'loop' over scenarios


  # Final returned quantity: list of scenario info,
  # each scenario now has list entry with the analysis
  # done across specified objective functions
  return(aScenarioListAnalyzed)

} # end run_objective



#' Organize a list of ScenarioInfo objects that have been analyzed with
#' objective functions into a grand table of parameters.
#'
#' The table produced includes the model parameters and all user-specified objective
#' function evaluations, for all of the models in the input list.
#'
#' Before this function can run, the scenario list must have been run through
#' \code{\link{run_objective}} to calculate all objective function values.
#'
#'
#' @param aScenarioList List of ScenarioInfo structures
#' @return Data frame containing model parameters and objective function values
#' @author ACS May 2020
#' @export
grand_table_objective <- function(aScenarioList)
{
  ## silence package checks
  scenario <- year <- obs <- trend <- detrended <- obsvar <- model <- NULL

  tbllen <- sapply(aScenarioList, function(s) {length(s$mObjFunEval)})
  if(any(tbllen) == 0) {
    warning('grand_table_objective: One or more scenarios do not have objective function evaluations. Running run_objective with default arguments.')
    aScenarioList <- run_objective(aScenarioList)
  }

  modata <-
    lapply(aScenarioList,
           function(s) {
             objfuneval <- s$mObjFunEval
             # removed line from `grand_table` that specified the scenario for this list entry because it's already in the
             # data frame held in this list. May be unnecessary in `grand_table` as well. Not touching for now.
             return(objfuneval)
           }) %>%
    dplyr::bind_rows()

  add_parameter_data(modata, aScenarioList) %>%
    # remove individual year entries for observations and model data to clean up:
    dplyr::select(-scenario, -year, -obs, -trend, -detrended, -obsvar, -model) %>%
    distinct

}




#' Calculate parameters than minimize objective function of choice for a collection of model runs
#'
#' Scans the runs in a given sample of model runs and selects the parameter set from those runs
#' that minimizes a specified objective function across specified landtypes.
#' The final minimized value is currently coded as the mean across specified landtypes of the
#' specified objective function of interest.
#'
#' For \code{'bias'}, the quantity being minimized is actually the average over specified land
#' types of \code{abs('bias')}, as a bias of 0 is
#' perfect performance, and a bias of 0.25 vs -0.25 are equally 'good'.
#'
#' For \code{'kge'}, the quantity being minimized is actually the average over specified land
#' types of \code{1 -'kge'}. This is chosen because of the following:
#' \itemize{
#' \item{Values of \code{'kge'} can range anywhere from -infinity to +1, with a value of +1 being
#' perfect model performance, therefore \code{'kge'} is not directly useful as a quantity to be
#' minimized.}
#' \item{Values of \code{-'kge'} range from -1 to +infinity, with -1 corresponding to perfect
#' model performance, as well as being the minimum value achieved by this measure. However,
#' because this quantity can be either negative or positive, it is possible that cancellation
#' of errors may occur when calculating the mean across specified land types, leading to
#' a false minimum.}
#' \item{Values of \code{1-'kge'} range from 0 to +infinity, with 0 corresponding to perfect
#' model performance, as well as being the minimum value achieved by this measure.
#' Additionally, because all values of this quantity are positive, no cancelation of
#' errors may occur when calculating the mean across specified land types.}
#' }
#'
#'
#' TODO: update the name so that it's not MAP anymore; currently naming this way for workflow
#' consistency with bayesian analysis, but we aren't doing Maximum A Posteriori analysis anymore,
#' so MAP is kind of misleading. I don't really have a better name right now though.
#' maybe minimizer_objective??
#'
#'
#' @param samples Monte Carlo samples, given either as a grand table or a list
#' of \code{ScenarioInfo} objects
#' @param modelgroup Vector of names of columns that define the model groupings.
#' The default is the single column \code{expectation.type}.
#' @param reportvars Vector of names of variables for which to report
#' expectations.  The default is all parameter values.
#' @param objfun_to_min a single objective function to minimize, from the supported
#' c('bias', 'rms', 'centeredrms', 'nrms', 'centerednrms', 'kge')). Defaults to rms.
#' Must be of length 1.
#' @param landtypes Vector of land types to include in minimization. (default is
#' to use  \code{c( "Corn", "FiberCrop", "MiscCrop", "OilCrop",
#'  "OtherGrain", "PalmFruit",  "Rice", "Root_Tuber","SugarCrop",  "Wheat"))}.
#'
#' @author ACS May 2020
#'
#' @export
MAP_objective <- function(samples, modelgroup='expectation.type', reportvars=NULL,
                objfun_to_min = 'rms', landtypes = NULL)
{
  # Silence package checks
  land.type <- region <- variable <- objfun <- objfunval <- expectation.type <-
    share.old1 <- share.old2 <- share.old3 <- linear.years1 <- linear.years2 <-
    linear.years3 <- logit.agforest <- logit.afnonpast <- logit.crop <-
      minimizervalue <- NULL

  if(!inherits(samples, 'data.frame')) {
    ## Check that this is a scenario list
    if( !inherits(samples, 'list') || !all(sapply(samples, is.ScenarioInfo)))
      stop('EV: samples must be a data frame or list of ScenarioInfo objects.')
  }

  if(is.null(reportvars)) {
    ## Use default values of reportvars
    reportvars <- c('logit.agforest', 'logit.afnonpast', 'logit.crop',
                    'share.old1', 'share.old2',  'share.old3',
                    'linear.years1', 'linear.years2', 'linear.years3')
  }

  if(is.null(landtypes)){
    landtypes <- c( "Corn", "FiberCrop", "MiscCrop", "OilCrop",
                    "OtherGrain", "PalmFruit",  "Rice", "Root_Tuber",
                    "SugarCrop",  "Wheat")
  }



  # Filter to objective function of interest, and land types of interest.
  # Adjust the quantity minimized for bias and kge:
  samples %>%
    filter(land.type %in% landtypes,
           objfun == objfun_to_min) %>%
    mutate(minimizervalue = if_else(objfun == 'bias', abs(objfunval),
                                    if_else(objfun == 'kge', 1-objfunval,
                                            objfunval))  ) ->
    adjustedsamples


  samples_by_model <- split(adjustedsamples, adjustedsamples[,modelgroup])

  maprows <-
    lapply(
      samples_by_model,
      function(d) {

        d %>%
          # Calculate the average across those land.types of that
          # objective function for each parameter set:
          group_by(region, variable, objfun, expectation.type,
                   share.old1, share.old2, share.old3,
                   linear.years1, linear.years2, linear.years3,
                   logit.agforest, logit.afnonpast, logit.crop) %>%
          # TODO ^ group by region may change when look beyond US.
          summarise(landTypeMeanObjFunVal = mean(minimizervalue, na.rm = T)) %>%
          ungroup ->
          d_LandTypeMeanObjFunVal

        k <- which.min(d_LandTypeMeanObjFunVal$landTypeMeanObjFunVal)

        mapval <-  d_LandTypeMeanObjFunVal[k,c(modelgroup, reportvars, 'objfun', 'landTypeMeanObjFunVal')]

        return(mapval)
      })
  bind_rows(maprows)
}
