
DEFAULT.SCENARIO.TYPE <- "Reference"

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
#' @param aExpectationType Expectation type
#' @param aLaggedShareOld1 Share of old expectations included in current expectation for crop group 1 (see constants.R)
#' @param aLaggedShareOld2 Share of old expectations included in current expectation for crop group 2 (see constants.R)
#' @param aLaggedShareOld3 Share of old expectations included in current expectation for crop group 3 (see constants.R)
#' @param aLaggedShareOld4 Share of old expectations included in current expectation for crop group 4 (see constants.R)
#' @param aLaggedShareOld5 Share of old expectations included in current expectation for crop group 5 (see constants.R)
#' @param aLinearYears1 Years for linear expectations for crop group 1 (see constants.R)
#' @param aLinearYears2 Years for linear expectations for crop group 2 (see constants.R)
#' @param aLinearYears3 Years for linear expectations for crop group 3 (see constants.R)
#' @param aLinearYears4 Years for linear expectations for crop group 4 (see constants.R)
#' @param aLinearYears5 Years for linear expectations for crop group 5 (see constants.R)
#' @param aLogitUseDefault Boolean indicating whether to use default logits
#' @param aLogitAgroForest AgroForest logit exponent (assuming mLogitUseDefault == FALSE)
#' @param aLogitAgroForest_NonPasture AgroForest_NonPasture logit exponent (assuming mLogitUseDefault == FALSE)
#' @param aLogitCropland Cropland logit exponent (assuming mLogitUseDefault ==
#' FALSE)
#' @param aUseZeroCost Boolean indicating whether to set costs to zero (assuming mUseZeroCost == FALSE)
#' @param aCalibrateShareWt Boolean indicating that the model should calculate share weights during calibration
#' @param aIncludeSubsidies Boolean indicating whether subsidy information should modify profit
#' @param aShareWeights Named vector of share weights to use instead of calibrating.
#' The names should correspond to the names of the land leaf nodes.
#' @param aScenarioType Type of scenario to run: either "Reference" or "Hindcast".
#' @param aScenarioName Complete scenario name, with expectations & logit info
#' @param aFileName File name
#' @param aOutputDir Output directory
#' @param aSerialNum Serial number for a run that is part of a series.
#' @param aRegion Region to use in the calculation.  Right now we only run a
#' single region at a time.
#' @param aSubRegion Subregion name. Note we can only run a full region or a subregion not both
#' @return New ScenarioInfo object
#' @export
#' @author KVC November 2017
ScenarioInfo <- function(# Currently only "Perfect", "Linear", "Adaptive", "HybridLinearAdaptive", and "HybridPerfectAdaptive" ExpectationType are supported
                         aExpectationType = NULL,
                         aLaggedShareOld1 = NA,
                         aLaggedShareOld2 = NA,
                         aLaggedShareOld3 = NA,
                         aLaggedShareOld4 = NA,
                         aLaggedShareOld5 = NA,
                         aLinearYears1 = NA,
                         aLinearYears2 = NA,
                         aLinearYears3 = NA,
                         aLinearYears4 = NA,
                         aLinearYears5 = NA,
                         aLogitUseDefault = TRUE,
                         aLogitAgroForest = NA,
                         aLogitAgroForest_NonPasture = NA,
                         aLogitCropland = NA,
                         aUseZeroCost = FALSE,
                         aCalibrateShareWt = TRUE,
                         aIncludeSubsidies = FALSE,
                         aShareWeights = NULL,
                         aScenarioType = DEFAULT.SCENARIO.TYPE,
                         aScenarioName = NULL,
                         aFileName = NULL,
                         aOutputDir = "./outputs",
                         aSerialNum = NA,
                         aRegion = DEFAULT.REGION,
                         aSubRegion = NULL) {

  self <- new.env(parent=emptyenv())
  class(self) <- c("ScenarioInfo", class(self))

  self$mExpectationType <- aExpectationType
  self$mLaggedShareOld1 <- aLaggedShareOld1
  self$mLaggedShareOld2 <- aLaggedShareOld2
  self$mLaggedShareOld3 <- aLaggedShareOld3
  self$mLaggedShareOld4 <- aLaggedShareOld4
  self$mLaggedShareOld5 <- aLaggedShareOld5
  self$mLinearYears1 <- aLinearYears1
  self$mLinearYears2 <- aLinearYears2
  self$mLinearYears3 <- aLinearYears3
  self$mLinearYears4 <- aLinearYears4
  self$mLinearYears5 <- aLinearYears5
  self$mLogitUseDefault <- aLogitUseDefault
  self$mLogitAgroForest <- aLogitAgroForest
  self$mLogitAgroForest_NonPasture <- aLogitAgroForest_NonPasture
  self$mLogitCropland <- aLogitCropland
  self$mUseZeroCost <- aUseZeroCost
  self$mCalibrateShareWt <- aCalibrateShareWt
  self$mIncludeSubsidies <- aIncludeSubsidies
  self$mShareWeights <- aShareWeights
  assertthat::assert_that(!is.null(aShareWeights) || aCalibrateShareWt, msg='If aShareWeights is not supplied, the aCalibrateShareWt must be TRUE.')
  self$mScenarioType <- aScenarioType
  self$mScenarioName <- aScenarioName
  self$mFileName <- aFileName
  self$mOutputDir <- aOutputDir
  self$mRegion <- aRegion
  self$mSubRegion <- aSubRegion
  self$mSerialNumber <- aSerialNum          # Used in run_ensemble
  self$mPointwiseLikelihood <- data.frame() # actually log-likelihood, tabulated
                                        # by data point.
  self$mLogPost <- data.frame()

  self
}


#' Test whether an object is a \code{ScenarioInfo} object
#'
#' @param object Object to be tested.
#' @export
is.ScenarioInfo <- function(object)
{
    inherits(object, 'ScenarioInfo')
}

#' Convert an object to a \code{ScenarioInfo} object
#'
#' @param object Object to be converted.
#' @export
as.ScenarioInfo <- function(object)
{
    UseMethod("as.ScenarioInfo", object)
}

#' @describeIn as.ScenarioInfo Convert an environment to a \code{ScenarioInfo}
#'
#' @export
as.ScenarioInfo.environment <- function(object)
{
    if(!is.ScenarioInfo(object)) {
        class(object) <- c('ScenarioInfo', class(object))
    }
    object
}

#' @describeIn as.ScenarioInfo Convert a list to a \code{ScenarioInfo}
#'
#' @export
as.ScenarioInfo.list <- function(object)
{
    eobj <- list2env(object, parent=emptyenv())
    as.ScenarioInfo(eobj)
}

#' SCENARIO.INFO
#'
#' A \code{ScenarioInfo} object with parameters for the default scenario.
#'
#' @export
#' @author Kate Calvin
SCENARIO.INFO <- ScenarioInfo(aScenarioType = DEFAULT.SCENARIO.TYPE,
                              aExpectationType = "Perfect",
                              aLinearYears1 = NA,
                              aLinearYears2 = NA,
                              aLinearYears3 = NA,
                              aLinearYears4 = NA,
                              aLinearYears5 = NA,
                              aLaggedShareOld1 = NA,
                              aLaggedShareOld2 = NA,
                              aLaggedShareOld3 = NA,
                              aLaggedShareOld4 = NA,
                              aLaggedShareOld5 = NA,
                              aLogitUseDefault = TRUE,
                              aLogitAgroForest = NA,
                              aLogitAgroForest_NonPasture = NA,
                              aLogitCropland = NA,
                              aUseZeroCost = FALSE,
                              aCalibrateShareWt = TRUE,
                              aIncludeSubsidies = FALSE,
                              aScenarioName = paste0(DEFAULT.SCENARIO.TYPE, "_", "Perfect"),
                              aFileName = paste0(DEFAULT.SCENARIO.TYPE, "_", "Perfect"))

#' PCHES.SCENARIO.INFO
#'
#' A \code{ScenarioInfo} object with parameters for the PCHES scenario.
#'
#' @export
#' @author Kate Calvin
PCHES.SCENARIO.INFO <- ScenarioInfo(aScenarioType = "PCHES",
                              aExpectationType = "Perfect",
                              aSubRegion = "PCHES",
                              aLinearYears1 = NA,
                              aLinearYears2 = NA,
                              aLinearYears3 = NA,
                              aLinearYears4 = NA,
                              aLinearYears5 = NA,
                              aLaggedShareOld1 = NA,
                              aLaggedShareOld2 = NA,
                              aLaggedShareOld3 = NA,
                              aLaggedShareOld4 = NA,
                              aLaggedShareOld5 = NA,
                              aLogitUseDefault = TRUE,
                              aLogitAgroForest = NA,
                              aLogitAgroForest_NonPasture = NA,
                              aLogitCropland = NA,
                              aUseZeroCost = FALSE,
                              aCalibrateShareWt = TRUE,
                              aScenarioName = paste0(DEFAULT.SCENARIO.TYPE, "_", "Perfect", "_", "PCHES"),
                              aFileName = paste0(DEFAULT.SCENARIO.TYPE, "_", "Perfect", "_", "PCHES"))

#' SRB.SCENARIO.INFO
#'
#' A \code{ScenarioInfo} object with parameters for the PCHES scenario.
#'
#' @export
#' @author Kate Calvin
SRB.SCENARIO.INFO <- ScenarioInfo(aScenarioType = "SRB",
                                    aExpectationType = "Perfect",
                                    aSubRegion = "SnakeRiverBasin",
                                    aLinearYears1 = NA,
                                    aLinearYears2 = NA,
                                    aLinearYears3 = NA,
                                    aLinearYears4 = NA,
                                    aLinearYears5 = NA,
                                    aLaggedShareOld1 = NA,
                                    aLaggedShareOld2 = NA,
                                    aLaggedShareOld3 = NA,
                                    aLaggedShareOld4 = NA,
                                    aLaggedShareOld5 = NA,
                                    aLogitUseDefault = TRUE,
                                    aLogitAgroForest = NA,
                                    aLogitAgroForest_NonPasture = NA,
                                    aLogitCropland = NA,
                                    aUseZeroCost = FALSE,
                                    aCalibrateShareWt = TRUE,
                                    aScenarioName = paste0(DEFAULT.SCENARIO.TYPE, "_", "Perfect", "_", "SRB"),
                                    aFileName = paste0(DEFAULT.SCENARIO.TYPE, "_", "Perfect", "_", "SRB"))


#' update_scen_info
#'
#' This function takes the default \code{\link{SCENARIO.INFO}} object and updates it based on
#' user specified arguments. Some updates (e.g., changing from "Reference" to "Hindcast" scenario
#' type) will automatically update the scenario name and file name.
#'
#' @param aName New scenario name (default will generate this from other info)
#' @param aScenarioType New scenario type (default = \code{DEFAULT.SCENARIO.TYPE})
#' @param aExpectationType New expectation type (default = "Perfect")
#' @param aLinearYears New linear years (default = NULL)
#' @param aLinearYears1 New linear years for crop group 1 (see constants.R) (default = NULL)
#' @param aLinearYears2 New linear years for crop group 2 (see constants.R) (default = NULL)
#' @param aLinearYears3 New linear years for crop group 3 (see constants.R) (default = NULL)
#' @param aLinearYears4 New linear years for crop group 4 (see constants.R) (default = NULL)
#' @param aLinearYears5 New linear years for crop group 5 (see constants.R) (default = NULL)
#' @param aLaggedShareOld New lagged share old (default = NULL)
#' @param aLaggedShareOld1 New lagged share old (group 1) (see constants.R) (default = NULL)
#' @param aLaggedShareOld2 New lagged share old (group 2) (see constants.R) (default = NULL)
#' @param aLaggedShareOld3 New lagged share old (group 3) (see constants.R) (default = NULL)
#' @param aLaggedShareOld4 New lagged share old (group 4) (see constants.R) (default = NULL)
#' @param aLaggedShareOld5 New lagged share old (group 5) (see constants.R) (default = NULL)
#' @param aUseZeroCost New cost assumption (default = FALSE)
#' @param aCalibrateShareWt Flag indicating share weights should be calibrated
#' @param aIncludeSubsidies Flag indicating subsidies should be added to profit
#' @param aShareWts Vector of share weights
#'
#' @return Updated scenario info object
#' @export
#' @author KVC November 2018
#' @examples
#' update_scen_info(aCalibrateShareWt = FALSE, aShareWts=get_saved_share_weights())
#' update_scen_info(aScenarioType = "Hindcast")
update_scen_info <- function(aName = NULL, aScenarioType = DEFAULT.SCENARIO.TYPE , aExpectationType = "Perfect",
                             aLinearYears = NULL, aLinearYears1 = NULL, aLinearYears2 = NULL, aLinearYears3 = NULL,
                             aLinearYears4 = NULL, aLinearYears5 = NULL,
                             aLaggedShareOld = NULL, aLaggedShareOld1 = NULL, aLaggedShareOld2 = NULL, aLaggedShareOld3 = NULL,
                             aLaggedShareOld4 = NULL,  aLaggedShareOld5 = NULL,
                             aUseZeroCost = FALSE, aCalibrateShareWt = TRUE, aIncludeSubsidies = FALSE, aShareWts = NULL) {

  # Set the names of the scenario & file based on read in information
  if(is.null(aName)) {
    new_name <- paste0(aScenarioType, "_", aExpectationType)
    new_name <- paste0(aScenarioType, "_", aExpectationType)
  } else {
    new_name <- aName
  }

  # Copy scenario info from default & update all scenario info
  new_scen_info <- SCENARIO.INFO
  new_scen_info$mScenarioType <- aScenarioType
  new_scen_info$mExpectationType <- aExpectationType
  new_scen_info$mUseZeroCost <- aUseZeroCost
  new_scen_info$mScenarioName <- new_name
  new_scen_info$mFileName <- new_name
  new_scen_info$mCalibrateShareWt <- aCalibrateShareWt
  new_scen_info$mIncludeSubsidies <- aIncludeSubsidies
  new_scen_info$mShareWeights <- aShareWts

  # Set number of linear years, if specified
  if(is.numeric(aLinearYears)) {
    # Set all groups to this value if it specified. These can be individually overwritten later.
    new_scen_info$mLinearYears1 <- aLinearYears
    new_scen_info$mLinearYears2 <- aLinearYears
    new_scen_info$mLinearYears3 <- aLinearYears
    new_scen_info$mLinearYears4 <- aLinearYears
    new_scen_info$mLinearYears5 <- aLinearYears
  }
  if(is.numeric(aLinearYears1)) {
    new_scen_info$mLinearYears1 <- aLinearYears1
  }
  if(is.numeric(aLinearYears2)) {
    new_scen_info$mLinearYears2 <- aLinearYears2
  }
  if(is.numeric(aLinearYears3)) {
    new_scen_info$mLinearYears3 <- aLinearYears3
  }
  if(is.numeric(aLinearYears4)) {
    new_scen_info$mLinearYears4 <- aLinearYears4
  }
  if(is.numeric(aLinearYears5)) {
    new_scen_info$mLinearYears5 <- aLinearYears5
  }

  # Set share of old expectations in adaptive expectation if specified
  if(is.numeric(aLaggedShareOld)) {
    # Set all groups to this value if it specified. These can be individually overwritten later.
    new_scen_info$mLaggedShareOld1 <- aLaggedShareOld
    new_scen_info$mLaggedShareOld2 <- aLaggedShareOld
    new_scen_info$mLaggedShareOld3 <- aLaggedShareOld
    new_scen_info$mLaggedShareOld4 <- aLaggedShareOld
    new_scen_info$mLaggedShareOld5 <- aLaggedShareOld
  }
  if(is.numeric(aLaggedShareOld1)) {
    new_scen_info$mLaggedShareOld1 <- aLaggedShareOld1
  }
  if(is.numeric(aLaggedShareOld2)) {
    new_scen_info$mLaggedShareOld2 <- aLaggedShareOld2
  }
  if(is.numeric(aLaggedShareOld3)) {
    new_scen_info$mLaggedShareOld3 <- aLaggedShareOld3
  }
  if(is.numeric(aLaggedShareOld4)) {
    new_scen_info$mLaggedShareOld4 <- aLaggedShareOld4
  }
  if(is.numeric(aLaggedShareOld5)) {
    new_scen_info$mLaggedShareOld5 <- aLaggedShareOld5
  }

  if(aCalibrateShareWt == FALSE & is.null(aShareWts)) {
    # If share weights aren't calculated or provided, get them from a file
    new_scen_info$mShareWeights <- get_saved_share_weights()
  }

  return(new_scen_info)
}

#' get_saved_share_weights
#'
#' Read in share weights from a file
#'
#' @details Returns a named vector of share weights, where names are the names of LandLeafs.
#' The resulting vector can be passed as an argument to the \code{\link{ScenarioInfo}} constructor,
#' to the \code{\link{update_scen_info}} function, or set directly to \code{mShareWeights} in a
#' \code{\link{ScenarioInfo}} object.
#'
#' @return Share weights as a named vector
#' @export
#'
#' @author KVC December 2018
get_saved_share_weights <- function() {
  temp <- suppressMessages(read.csv(system.file("extdata", "./initialization-data/CalibratedShareWeights_2010.csv", package = "gcamland"), skip = 3, stringsAsFactors = FALSE))

  shwt_vector <- as.numeric(temp$shareWeight)
  names(shwt_vector) <- temp$name

  return(shwt_vector)
}
