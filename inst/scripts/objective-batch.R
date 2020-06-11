#### Test script for objective function analysis

library('gcamland')
library('doParallel')

## nodefile: File containing the list of nodes
## nproc:    Total number of cores available ($SLURM_NTASKS)
## N:        Number of iterations to run
## outdir:   Output directory
## skip:     Number of iterations to skip (i.e., if resuming from a previous run)
## logdir:   Directory for logs
run_ens_obj_analysis <- function(nodefile, nproc, N, outdir, skip=0, logdir=NULL)
{
    print(getwd())
    nodes <- readr::read_lines(nodefile)
    ncore <- ceiling(nproc / length(nodes)) # cores per node
    nodes <- rep(nodes, ncore)

    cl <- makeCluster(nodes, outfile="")
    registerDoParallel(cl)

    print(system.time(run_ensemble(N, outdir, skip, logparallel=logdir)))
    stopCluster(cl)

    # TODO: update to include objective function analysis
    # run_objective can be run in parallel and explictly saved.
    # grand_table_objective and MAP_objective need to work on
    # the results of run_objective for all models of interest.
}


