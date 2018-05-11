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

echo "source('$program'); run_mc('$nodefile', $SLURM_NTASKS, $N, '$outdir')"

Rscript -e "source('$program'); run_mc('$nodefile', $SLURM_NTASKS, $N, '$outdir')"

rm -rf $tmpdir

date


