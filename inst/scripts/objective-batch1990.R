#### Test script for objective function analysis

library('gcamland', lib.loc = '/qfs/people/snyd535/R/x86_64-pc-linux-gnu-library/3.4/')
library('doParallel', lib.loc = '/qfs/people/snyd535/R/x86_64-pc-linux-gnu-library/3.4/')

## nodefile: File containing the list of nodes
## nproc:    Total number of cores available ($SLURM_NTASKS)
## N:        Number of iterations to run
## outdir:   Output directory
## skip:     Number of iterations to skip (i.e., if resuming from a previous run)
## logdir:   Directory for logs
run_ens_obj_analysis <- function(nodefile, nproc,
                                 N = 500, outdir, skip = 0, logdir=NULL,
                                 aType = "Hindcast1990",
                                 aIncludeSubsidies = FALSE,
                                 aDifferentiateParamByCrop = TRUE,
                                 aSampleType = "LatinHyperCube",
                                 aTotalSamplesPlanned = 5000,
                                 aRegion)
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
                                                  aRegion = aRegion,
                                                  aIncludeSubsidies = aIncludeSubsidies,
                                                  aDifferentiateParamByCrop = aDifferentiateParamByCrop,
                                                  aSampleType = aSampleType,
                                                  aTotalSamplesPlanned = aTotalSamplesPlanned,
                                                  logparallel=logdir)))

    print("Starting objective function calculations")
    # Do the objective function calculations for every scenario
    print(system.time(suppressWarnings(scenObjectsEvaluated <- run_objective(scenObjects))))


    # Convert those to a grand_table
    print(system.time(GT <- grand_table_objective(aScenarioList = scenObjectsEvaluated)))

    # save those
    suffix <- sprintf("-%06d",skip)
    filebase <- paste0("grand_table_objective_", suffix, "_", Sys.Date(),".rds")
    GTfile <- file.path(outdir, filebase)
    saveRDS(GT, GTfile)

    stopCluster(cl)
}


