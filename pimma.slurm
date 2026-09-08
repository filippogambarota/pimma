#!/bin/bash
#SBATCH --job-name="flipmeta"
#SBATCH --partition="queue1"
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=90
#SBATCH --mem=500G
#SBATCH --time=00:00:00
#SBATCH --output=flipmeta_%j.out
#SBATCH --error=flipmeta_%j.err
#SBATCH --mail-user=filippo.gambarota@unipd.it
#SBATCH --mail-type=ALL

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
secexport BLIS_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1

source ~/miniconda3/etc/profile.d/conda.sh
conda activate r460

srun Rscript --vanilla simulation/sim.R
