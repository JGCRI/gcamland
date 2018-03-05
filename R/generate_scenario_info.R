
#' ScenarioInfo
#'
#' @details Contains all information needed to describe a scenario
#' @param aScenario Scenario name
#' @param aExpectationType Expectation type
#' @param aLaggedShareOld Share of old expectations included in current expectation
#' @param aLinearYears Years for linear expectations
#' @param aLogitUseDefault Boolean indicating whether to use default logits
#' @param aLogitAgroForest AgroForest logit exponent (assuming mLogitUseDefault == FALSE)
#' @param aLogitAgroForest_NonPasture AgroForest_NonPasture logit exponent (assuming mLogitUseDefault == FALSE)
#' @param aLogitCropland Cropland logit exponent (assuming mLogitUseDefault == FALSE)
#' @param aScenarioName Complete scenario name, with expectations & logit info
#' @param aFileName File name
#' @param aOutputDir Output directory
#' @return New ScenarioInfo object
#' @export
#' @author KVC November 2017
ScenarioInfo <- function(aScenario = NULL,
                         # Currently only "Perfect", "Linear", and "Lagged" ExpectationType are supported
                         aExpectationType = NULL,
                         aLaggedShareOld = NULL,
                         aLinearYears = NULL,
                         aLogitUseDefault = NULL,
                         aLogitAgroForest = NULL,
                         aLogitAgroForest_NonPasture = NULL,
                         aLogitCropland = NULL,
                         aScenarioName = NULL,
                         aFileName = NULL,
                         aOutputDir = "./outputs") {
  mScenario <- aScenario
  mExpectationType <- aExpectationType
  mLaggedShareOld <- aLaggedShareOld
  mLinearYears <- aLinearYears
  mLogitUseDefault <- aLogitUseDefault
  mLogitAgroForest <- aLogitAgroForest
  mLogitAgroForest_NonPasture <- aLogitAgroForest_NonPasture
  mLogitCropland <- aLogitCropland
  mScenarioName <- aScenarioName
  mFileName <- aFileName
  mOutputDir <- aOutputDir

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
                              aExpectationType = "Perfect",
                              aLinearYears = NULL,
                              aLaggedShareOld = NULL,
                              aLogitUseDefault = TRUE,
                              aLogitAgroForest = NULL,
                              aLogitAgroForest_NonPasture = NULL,
                              aLogitCropland = NULL,
                              aScenarioName = paste(SCENARIO, "_", "Perfect", sep=""),
                              aFileName = paste(SCENARIO, "_", "Perfect", sep=""))
