% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/objectivefun.R
\name{minimize_objective}
\alias{minimize_objective}
\title{Calculate parameters than minimize objective function of choice for a collection of model runs}
\usage{
minimize_objective(
  samples,
  modelgroup = "expectation.type",
  reportvars = NULL,
  objfun_to_min = "rms",
  landtypes = NULL
)
}
\arguments{
\item{samples}{Monte Carlo samples, given either as a grand table or a list
of \code{ScenarioInfo} objects}

\item{modelgroup}{Vector of names of columns that define the model groupings.
The default is the single column \code{expectation.type}.}

\item{reportvars}{Vector of names of variables for which to report
expectations.  The default is all parameter values.}

\item{objfun_to_min}{a single objective function to minimize, from the supported
c('bias', 'rms', 'centeredrms', 'nrms', 'centerednrms', 'kge')). Defaults to rms.
Must be of length 1.}

\item{landtypes}{Vector of land types to include in minimization. (default is
to use  \code{c( "Corn", "FiberCrop", "MiscCrop", "OilCrop",
 "OtherGrain", "PalmFruit",  "Rice", "Root_Tuber","SugarCrop",  "Wheat"))}.}
}
\description{
Scans the runs in a given sample of model runs and selects the parameter set from those runs
that minimizes a specified objective function across specified landtypes.
The final minimized value is currently coded as the mean across specified landtypes of the
specified objective function of interest.
}
\details{
For \code{'bias'}, the quantity being minimized is actually the average over specified land
types of \code{abs('bias')}, as a bias of 0 is
perfect performance, and a bias of 0.25 vs -0.25 are equally 'good'.

For \code{'kge'}, the quantity being minimized is actually the average over specified land
types of \code{1 -'kge'}. This is chosen because of the following:
\itemize{
\item{Values of \code{'kge'} can range anywhere from -infinity to +1, with a value of +1 being
perfect model performance, therefore \code{'kge'} is not directly useful as a quantity to be
minimized.}
\item{Values of \code{-'kge'} range from -1 to +infinity, with -1 corresponding to perfect
model performance, as well as being the minimum value achieved by this measure. However,
because this quantity can be either negative or positive, it is possible that cancellation
of errors may occur when calculating the mean across specified land types, leading to
a false minimum.}
\item{Values of \code{1-'kge'} range from 0 to +infinity, with 0 corresponding to perfect
model performance, as well as being the minimum value achieved by this measure.
Additionally, because all values of this quantity are positive, no cancelation of
errors may occur when calculating the mean across specified land types.}
}
}
\author{
ACS May 2020
}
