
#' ScenarioInfo constructor
#'
#' Create a structure that ontains all information needed to describe a
#' scenario.
#'
#' Most of the parameters are self-explanatory.  The \code{Obsvar} parameter
#' is a little unusual, in that it plays no role in the model calculations; it
#' only enters into the likelihood function.  Fitting this parameter allows us
#' to estimate how much of the variation in the observed data isn't captured by
#' our model.  This variation could be because of irreducible uncertainty (e.g.,
#' measurement error in the observed data), or it could be an indicator that
#' there is some behavior that our model is failing to capture.
#'
#' @param aScenario Scenario name
#' @param aExpectationType Expectation type
#' @param aLaggedShareOld Share of old expectations included in current expectation
#' @param aLinearYears Years for linear expectations
#' @param aLogitUseDefault Boolean indicating whether to use default logits
#' @param aLogitAgroForest AgroForest logit exponent (assuming mLogitUseDefault == FALSE)
#' @param aLogitAgroForest_NonPasture AgroForest_NonPasture logit exponent (assuming mLogitUseDefault == FALSE)
#' @param aLogitCropland Cropland logit exponent (assuming mLogitUseDefault ==
#' FALSE)
#' @param aObsvar Observed data variance.
#' @param aScenarioName Complete scenario name, with expectations & logit info
#' @param aFileName File name
#' @param aOutputDir Output directory
#' @param aRegion Region to use in the calculation.  Right now we only run a
#' single region at a time.
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
                         aObsvar = 1.0,
                         aScenarioName = NULL,
                         aFileName = NULL,
                         aOutputDir = "./outputs",
                         aRegion = DEFAULT.REGION) {

  self <- new.env()
  class(self) <- "ScenarioInfo"

  self$mScenario <- aScenario
  self$mExpectationType <- aExpectationType
  self$mLaggedShareOld <- aLaggedShareOld
  self$mLinearYears <- aLinearYears
  self$mLogitUseDefault <- aLogitUseDefault
  self$mLogitAgroForest <- aLogitAgroForest
  self$mLogitAgroForest_NonPasture <- aLogitAgroForest_NonPasture
  self$mLogitCropland <- aLogitCropland
  self$mObsvar <- aObsvar
  self$mScenarioName <- aScenarioName
  self$mFileName <- aFileName
  self$mOutputDir <- aOutputDir
  self$mRegion <- aRegion

  self
}

#' SCENARIO.INFO
#'
#' A \code{ScenarioInfo} object with parameters for the default scenario.
#'
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
