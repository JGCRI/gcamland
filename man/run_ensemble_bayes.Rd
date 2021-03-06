% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{run_ensemble_bayes}
\alias{run_ensemble_bayes}
\title{Run an ensemble of offline land models and complete Bayesian analysis
of hindcast runs.}
\usage{
run_ensemble_bayes(
  N = 500,
  aOutputDir = "./outputs",
  skip = 0,
  lpdf = get_lpdf(1),
  lprior = uniform_prior,
  aType = "Hindcast",
  aIncludeSubsidies = FALSE,
  aDifferentiateParamByCrop = FALSE,
  aSampleType = "LatinHyperCube",
  aTotalSamplesPlanned = 500,
  logparallel = NULL
)
}
\arguments{
\item{N}{Number of parameter sets to select}

\item{aOutputDir}{Output directory}

\item{skip}{Number of iterations to skip (i.e., if building on another run.)}

\item{lpdf}{Log-likelihood function.  Used only if Bayesian posteriors are
being run.}

\item{lprior}{Log-prior probability density function.  Used only if Bayesian
posteriors are being run.}

\item{aType}{Scenario type: either "Reference" or "Hindcast"}

\item{aIncludeSubsidies}{Boolean indicating subsidies should be added to profit}

\item{aDifferentiateParamByCrop}{Boolean indicating whether all crops should use the same expectation parameters}

\item{aSampleType}{String indicating what type of sampling, currently only "LatinHyperCube" and "Sobol" are supported}

\item{aTotalSamplesPlanned}{Number of samples planned. For Latin Hypercube, we need to know the total before we start.}

\item{logparallel}{Name of directory to use for parallel workers' log files.
If \code{NULL}, then don't write log files.}
}
\value{
List of ScenarioInfo objects for the ensemble members
}
\description{
Parameter combinations are selected by generating a quasi-random
sequence and mapping it to a specified range for each parameter.
Then, each parameter set is run through the offline land model in
each of the Perfect, Adaptive, HybridLinearAdaptive, HybridPerfectAdaptive, and Linear variants.
(I.e., if N parameter sets are selected, then 5N scenarios are run.)
}
\details{
If the scenario type is "Hindcast", then after each model has been run, the
Bayesian analysis will be run so that its results can be stored with the rest
of the ScenarioInfo structure.
}
\section{Output}{

The model results are written to a series of files in the specified output
directory.
The
list of \code{ScenarioInfo} objects is written to a file called
\code{bayes-scenario-info.rds} in the output directory.  This file can be loaded
with a command such as \code{scenaro_list <-
readRDS('output/bayes-scenario-info.rds')}.  These objects contain links to the
model output files, as well as the posterior probability density tables, if
the Bayesian analysis was run.
}

\author{
KVC November 2017
}
