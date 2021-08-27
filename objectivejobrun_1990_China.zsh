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

program=`Rscript -e 'cat("/pic/projects/GCAM/Abigail/gcamland/objective-batch1990China.R")'`

N=500
TOTAL_SAMPLES=100000
PARAM=TRUE
SUBS=FALSE
REGION="China"
tid=$SLURM_ARRAY_TASK_ID
let "skip = tid*N"
outdir="/pic/projects/GCAM/Abigail/gcamland_output/$REGION"
logdir="/pic/projects/GCAM/Abigail/gcamland_output/$REGION/log"

mkdir -p $outdir
mkdir -p $logdir


echo "Run command:"
echo "source('$program'); run_ens_obj_analysis('$nodefile', $SLURM_NTASKS, $N, '$outdir', $skip, logdir='$logdir', aDifferentiateParamByCrop = $PARAM, aIncludeSubsidies=$SUBS, aTotalSamplesPlanned = $TOTAL_SAMPLES)"

Rscript -e "source('$program'); run_ens_obj_analysis('$nodefile', $SLURM_NTASKS, $N, '$outdir', $skip, logdir='$logdir', aDifferentiateParamByCrop = $PARAM, aIncludeSubsidies=$SUBS, aTotalSamplesPlanned = $TOTAL_SAMPLES)"
## Use this version instead to write log files:
## Rscript -e "source('$program'); run_ens_obj_analysis('$nodefile', $SLURM_NTASKS, $N, '$outdir', '$skip', '$logdir')"

rm -rf $tmpdir

date

