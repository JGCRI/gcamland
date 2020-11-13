#!/bin/zsh

#SBATCH -n 48
#SBATCH -t 300
#SBATCH -A IHESD

date

source /etc/profile.d/modules.sh
module unload R
module load R/3.4.3

tmpdir=`mktemp -d`
nodefile=$tmpdir/nodes.txt

scontrol show hostnames > $nodefile

program=`Rscript -e 'cat("/pic/projects/GCAM/Abigail/gcamland/objective-batch1990.R")'`

N=2
TOTAL_SAMPLES=3
PARAM=TRUE
SUBS=FALSE
REGION="USA"
tid=$SLURM_ARRAY_TASK_ID
let "skip = tid*N"
outdir="/pic/projects/GCAM/Abigail/gcamland_output/$REGION"
logdir="/pic/projects/GCAM/Abigail/gcamland_output/$REGION/log"

mkdir -p $outdir
mkdir -p $logdir

echo "Run command:"
echo "source('$program'); run_ens_obj_analysis('$nodefile', $SLURM_NTASKS, $N, '$outdir', $skip, logdir='$logdir', aDifferentiateParamByCrop = $PARAM, aIncludeSubsidies=$SUBS, aTotalSamplesPlanned = $TOTAL_SAMPLES, aRegion = ${(P)REGION})"

Rscript -e "source('$program'); run_ens_obj_analysis('$nodefile', $SLURM_NTASKS, $N, '$outdir', $skip, logdir='$logdir', aDifferentiateParamByCrop = $PARAM, aIncludeSubsidies=$SUBS, aTotalSamplesPlanned = $TOTAL_SAMPLES, aRegion = ${(P)REGION})"
## Use this version instead to write log files:
## Rscript -e "source('$program'); run_ens_obj_analysis('$nodefile', $SLURM_NTASKS, $N, '$outdir', '$skip', '$logdir')"

rm -rf $tmpdir

date


