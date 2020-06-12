#### Test script for Monte Carlo runs

library('gcamland')
library('doParallel')

## nodefile: File containing the list of nodes
## nproc:    Total number of cores available ($SLURM_NTASKS)
## N:        Number of iterations to run
## outdir:   Output directory
## skip:     Number of iterations to skip (i.e., if resuming from a previous run)
## logdir:   Directory for logs
run_mc <- function(nodefile, nproc, N, outdir, skip=0, logdir=NULL, aIncludeSubsidies=FALSE, aDifferentiateParamByCrop = FALSE)
{
    print(getwd())
    nodes <- readr::read_lines(nodefile)
    ncore <- ceiling(nproc / length(nodes)) # cores per node
    nodes <- rep(nodes, ncore)

    cl <- makeCluster(nodes, outfile="")
    registerDoParallel(cl)
    print(system.time(run_ensemble_bayes(N, outdir, skip, aIncludeSubsidies=aIncludeSubsidies, aDifferentiateParamByCrop=aDifferentiateParamByCrop, logparallel=logdir)))
    stopCluster(cl)
}


