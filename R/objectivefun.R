# objectivefun.R
# Objective functions for use in model evaluation across parameter sets.
# Can be used for  brute force approach.
# Also includes functions for analysis of objective function results,
# namely analogues to `grand_table`and `MAP` from bayesian.R



#' Run user-specified objective function evaluations on a list of scenarios
#'
#' The assumption is that the scenarios have already been run.  The best way to
#' arrange this is to use the return value of \code{\link{run_ensemble_no_analysis}} as the
#' argument to this function
#'
#' The \code{years} and \code{landtypes} arguments can be use to restrict the
#' observations that will be used in the analysis.  The regions are always
#' filtered to excatly the regions that are included in the ScenarioInfo
#' structures. The \code{objectivefuns} is vector of strings with the names of
#' objective functions to be evaluated. Currently, we support the following
#' objective functions: \code{c('bias', 'rms', 'centeredrms', 'nrms', 'centerednrms', 'kge'))}
#' TODO: Define each and give equations being calculated
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
#' @author ACS May 2020
#' @export
run_objective <- function(aScenarioList, years=NULL, landtypes=NULL,
                          objectivefuns = c('bias', 'rms', 'centeredrms', 'nrms', 'centerednrms', 'kge'))
{
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
                        by=(c('region','land.type','variable','year'))) ->
      s$mObjFunEval


    # Loop over all user specified objective functions to append that calculation
    # as its own column in this new list entry, s$mObjFunEval:
    for(objfun in objectivefuns) {

      if (objfun == 'bias') {

        s$mObjFunEval %>%
          group_by(region, land.type, variable) %>%
          mutate(obsMean = mean(obs, na.rm = T),
                 modelMean = mean(model, na.rm = T),
                 bias = modelMean - obsMean) %>%
          ungroup %>%
          select(-modelMean, -obsMean) ->
          s$mObjFunEval

        } # end bias calculation



      if (objfun == 'rms') {

         s$mObjFunEval %>%
          group_by(region, land.type, variable) %>%
          mutate(rms = sqrt(mean( ((obs-model)^2) ) ) ) %>%
          ungroup ->
          s$mObjFunEval

      } # end rms  calculation


    }  # end for loop over objective function options


    # reshape the analyzed data for the scenario being analyzed;
    # take from having each obj fun evaluation in its own column
    # named from the measure name, holding the measured value to
    # two columns: measure name and value:
    s$mObjFunEval %>%
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
  scenario <- NULL

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
#' that minimizes a specified objective function across specified land.types.
#' The final minimized value is currently coded as the mean across specified land.types of the
#' specified objective function of interest.
#'
#' TODO: update the name so that it's not MAP anymore; currently naming this way for workflow
#' consistency with bayesian analysis, but we aren't doing Maximum A Posteriori analysis anymore,
#' so MAP is kind of misleading. I don't really have a better name right now though.
#'
#' TODO: more detail on calculation being done.
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
  if(!inherits(samples, 'data.frame')) {
    ## Check that this is a scenario list
    if( !inherits(samples, 'list') || !all(sapply(samples, is.ScenarioInfo)))
      stop('EV: samples must be a data frame or list of ScenarioInfo objects.')
  }

  if(is.null(reportvars)) {
    ## Use default values of reportvars
    reportvars <- c('logit.agforest', 'logit.afnonpast', 'logit.crop',
                    'share.old', 'linear.years')
  }

  if(is.null(landtypes)){
    landtypes <- c( "Corn", "FiberCrop", "MiscCrop", "OilCrop",
                    "OtherGrain", "PalmFruit",  "Rice", "Root_Tuber",
                    "SugarCrop",  "Wheat")
  }

  samples_by_model <- split(samples, samples[,modelgroup])
  maprows <-
    lapply(
      samples_by_model,
      function(d) {

        d %>%
          filter(land.type %in% landtypes,
                 objfun == objfun_to_min) %>%
          # Calculate the average across those land.types of that
          # objective function for each parameter set:
          group_by(region, variable, objfun, expectation.type,
                   share.old, linear.years, logit.agforest,
                   logit.afnonpast, logit.crop) %>%
          # TODO ^ group by region may change when look beyond US.
          summarise(landTypeMeanObjFunVal = mean(abs(objfunval), na.rm = T)) %>%
          ungroup ->
          d_LandTypeMeanObjFunVal

        k <- which.min(d_LandTypeMeanObjFunVal$landTypeMeanObjFunVal)

        mapval <-  d_LandTypeMeanObjFunVal[k,c(modelgroup, reportvars, 'objfun', 'landTypeMeanObjFunVal')]

        return(mapval)
      })
  bind_rows(maprows)
}
