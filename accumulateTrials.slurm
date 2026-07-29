#!/bin/bash
#SBATCH --job-name="accumulateTrials"
#SBATCH --partition="queue1"
#SBATCH --mem-per-cpu=5G
#SBATCH --output=long-job.out
#SBATCH --mail-user=filippo.gambarota@unipd.it
#SBATCH --mail-type ALL
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=70
#SBATCH --time=00:00:00

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export BLIS_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1

source ~/miniconda3/etc/profile.d/conda.sh
conda activate r460
srun Rscript --vanilla -e 'source("renv/activate.R"); source("scripts/03a-resampling.R")'
