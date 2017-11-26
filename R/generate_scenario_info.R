
#' ScenarioInfo
#'
#' @details Contains all information needed to describe a scenario
#' @param aScenario Scenario name
#' @param aExpectationType Expectation type
#' @param aLaggedYears Years for lagged expectations
#' @param aLinearYears Years for linear expectations
#' @param aLogitUseDefault Boolean indicating whether to use default logits
#' @param aLogitAgroForest AgroForest logit exponent (assuming mLogitUseDefault == FALSE)
#' @param aLogitAgroForest_NonPasture AgroForest_NonPasture logit exponent (assuming mLogitUseDefault == FALSE)
#' @param aLogitCropland Cropland logit exponent (assuming mLogitUseDefault == FALSE)
#' @param aScenarioName Complete scenario name, with expectations & logit info
#' @param aFileName File name
#' @return New ScenarioInfo object
#' @export
#' @author KVC November 2017
ScenarioInfo <- function(aScenario = NULL,
                         # Currently only "Perfect", "Linear", and "Lagged" ExpectationType are supported
                         aExpectationType = NULL,
                         aLaggedYears = NULL,
                         aLinearYears = NULL,
                         aLogitUseDefault = NULL,
                         aLogitAgroForest = NULL,
                         aLogitAgroForest_NonPasture = NULL,
                         aLogitCropland = NULL,
                         aScenarioName = NULL,
                         aFileName = NULL) {
  mScenario <- aScenario
  mExpectationType <- aExpectationType
  mLaggedYears <- aLaggedYears
  mLinearYears <- aLinearYears
  mLogitUseDefault <- aLogitUseDefault
  mLogitAgroForest <- aLogitAgroForest
  mLogitAgroForest_NonPasture <- aLogitAgroForest_NonPasture
  mLogitCropland <- aLogitCropland
  mScenarioName <- aScenarioName
  mFileName <- aFileName

  self <- environment()
  class(self) <- "ScenarioInfo"
  self
}

#' SCENARIO.INFO
#'
#' A tibble with scenario info for the default
#' @export
#' @author Kate Calvin
SCENARIO.INFO <- ScenarioInfo(aScenario = SCENARIO,
                              aExpectationType = "Lagged",
                              aLinearYears = NULL,
                              aLaggedYears = 5,
                              aLogitUseDefault = TRUE,
                              aLogitAgroForest = NULL,
                              aLogitAgroForest_NonPasture = NULL,
                              aLogitCropland = NULL,
                              aScenarioName = paste(SCENARIO, "_", "Lagged", 5, sep=""),
                              aFileName = paste(SCENARIO, "_", "Lagged", 5, sep=""))
