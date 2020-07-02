#### Test script for objective function analysis

library('gcamland')
library('doParallel')

## nodefile: File containing the list of nodes
## nproc:    Total number of cores available ($SLURM_NTASKS)
## N:        Number of iterations to run
## outdir:   Output directory
## skip:     Number of iterations to skip (i.e., if resuming from a previous run)
## logdir:   Directory for logs
run_ens_obj_analysis <- function(nodefile, nproc,
                                 N = 500, outdir, skip = 0, logdir=NULL,
                                 aType = "Hindcast",
                                 aIncludeSubsidies = FALSE,
                                 aDifferentiateParamByCrop = FALSE,
                                 aSampleType = "LatinHyperCube",
                                 aTotalSamplesPlanned = 500)
{
    print(getwd())
    nodes <- readr::read_lines(nodefile)
    ncore <- ceiling(nproc / length(nodes)) # cores per node
    nodes <- rep(nodes, ncore)

    cl <- makeCluster(nodes, outfile="")
    registerDoParallel(cl)

    print(system.time(scenObjects <- run_ensemble(N = N, aOutputDir = outdir,
                                   skip = skip,
                                   aType = aType,
                                   aIncludeSubsidies = aIncludeSubsidies,
                                   aDifferentiateParamByCrop = aDifferentiateParamByCrop,
                                   aSampleType = aSampleType,
                                   aTotalSamplesPlanned = aTotalSamplesPlanned,
                                   logparallel=logdir)))


    # Do the objective function calculations for every scenario
    print(system.time(suppressWarnings(scenObjectsEvaluated <- run_objective(scenObjects))))


    # Convert those to a grand_table
    print(system.time(GT <- grand_table_objective(aScenarioList = scenObjectsEvaluated)))

    # save those
    suffix <- sprintf("-%06d",skip)
    filebase <- paste0("grand_table_objective_", suffix, "_", Sys.Date(),".rds")
    GTfile <- file.path(outdir, filebase)
    saveRDS(GT, GTfile)


    # select the parameter set that minimizes average across all crops RMS.
    print(system.time(minimized <- minimize_objective(GTobjective,
                                                      objfun_to_min = 'rms')))

    # and save
    filebase <- paste0("minimized_rms_allcrops_", suffix, "_", Sys.Date(),".rds")
    minfile <- file.path(outdir, filebase)
    saveRDS(minimized, minfile)


    stopCluster(cl)
}


