#!/bin/zsh

#SBATCH -n 48
#SBATCH -t 300
#SBATCH -A GCAM

date

tmpdir=`mktemp -d`
nodefile=$tmpdir/nodes.txt

scontrol show hostnames > $nodefile

program=`Rscript -e 'cat(system.file("scripts", "mc-batch.R", package="gcamland"))'`

N=960
tid=$SLURM_ARRAY_TASK_ID
let "skip = tid*N"
outdir="/pic/scratch/$USER/gcamland/output"
logdir="/pic/scratch/$USER/gcamland"

mkdir -p $outdir
mkdir -p $logdir

echo "Run command:"
echo "source('$program'); run_mc('$nodefile', $SLURM_NTASKS, $N, '$outdir', $skip)"

Rscript -e "source('$program'); run_mc('$nodefile', $SLURM_NTASKS, $N, '$outdir', $skip)"
## Use this version instead to write log files:
## Rscript -e "source('$program'); run_mc('$nodefile', $SLURM_NTASKS, $N, '$outdir', '$skip', '$logdir')"

rm -rf $tmpdir

date


