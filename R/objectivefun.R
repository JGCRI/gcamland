# objectivefun.R
# Objective functions for use in model evaluation across parameter sets.
# Can be used for either a brute force approach, or as objective functions in
# iterative methods approaches.



#' Run user-specified objective function evaluations on a list of scenarios
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
#'
#' @param aScenarioList List of ScenarioInfo structures
#' @param years Vector of years to filter observations to (default is to use all
#' available years)
#' @param landtypes Vector of land types to filter observations to (default is
#' to use all available land types)
#' @param objectivefuns
#' @return Modified list of ScenarioInfo structures with the objective
#' function calculation tables populated.
#' @export
run_objective <- function(aScenarioList, years=NULL, landtypes=NULL,
                       objectivefuns = c())
{
  rgns <- unique(sapply(aScenarioList, function(s) {s$mRegion}))
  obsdata <- get_historical_land_data(rgns, years, landtypes)
  modeldata <- get_scenario_land_data(aScenarioList)


  return(rgs, obsdata, modeldata)
  # invisible(
  #   lapply(aScenarioList,
  #          function(s) {
  #            calc_obj(s, obsdata, modeldata[[s$mScenarioName]],
  #                      lpdf=lpdf, lprior=lprior)
  #          }))
}
