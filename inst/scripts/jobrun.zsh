#!/bin/zsh

#SBATCH -n 48
#SBATCH -t 30
#SBATCH -A GCAM

date

tmpdir=`mktemp -d`
nodefile=$tmpdir/nodes.txt

scontrol show hostnames > $nodefile

program=`Rscript -e 'cat(system.file("scripts", "mc-test.R", package="gcamland"))'`

N=96
outdir="/pic/scratch/$USER/gcamland/output"
logdir="/pic/scratch/$USER/gcamland"

mkdir -p $outdir
mkdir -p $logdir

echo "Run command:"
echo "source('$program'); run_mc('$nodefile', $SLURM_NTASKS, $N, '$outdir', '$logdir')"

Rscript -e "source('$program'); run_mc('$nodefile', $SLURM_NTASKS, $N, '$outdir')"
## Use this version instead to write log files:
## Rscript -e "source('$program'); run_mc('$nodefile', $SLURM_NTASKS, $N, '$outdir', '$logdir')"

rm -rf $tmpdir

date


