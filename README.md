# Offline version of the GCAM land allocation module
[![Travis-CI Build Status](https://travis-ci.org/JGCRI/gcamland.svg?branch=master)](https://travis-ci.org/JGCRI/gcamland)



## Basic usage

To run an individual scenario, first generate a structure to hold the
scenario parameters.  The scenario structure can be passed directly to
the main model function.  

```R
sceninfo <- ScenarioInfo(aScenarioType = "Hindcast")   # see helpfile for arguments 
outdir <- run_model(sceninfo)
```

The results will be written as a series of csv files in the `outdir` directory.
Subdirectories under that top-level directory will contain model outputs including
price and yield expectations and land allocation.


## Running ensembles

The `run_ensemble` function will generate and run a collection of scenarios with
varying parameters.  The parameters are sampled quasi-randomly using a
[Sobol sequence](https://en.wikipedia.org/wiki/Sobol_sequence).  The arguments to
`run_ensemble` are the number of samples (for _each_ expectation model, so 3 times
that many will actually be run), output directory, and (optionally) scenario type.

```R
registerDoParallel(cores=6)
ensemble_scenarios <- run_ensemble(1000, './ensemble-output')
```

The `run_ensemble` function is parallel enabled using the `doParallel` package.  The
return value is a list of the `ScenarioInfo` structures created.  These structures
have the parameter values, as well as the file names for the output.

## Calculating Bayesian Posteriors

The `run_bayes` function will compute a Bayesian log-posterior probability density
for a list of models returned from `run_ensemble`.  The results of this calculation
are stored in the list of modified `ScenarioInfo` structures returned from the
function, but the `grand_table` function will turn these into a table with parameter
values and log-posteriors.

```R
ensemble_scenarios <- run_bayes(ensemble_scenarios)
post_table <- grand_table(ensemble_scenarios)
```

