#!/bin/bash

#SBATCH --job-name=analysis_NLSM
#SBATCH --output=../log/analysis_NLSM_%a.out
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=1-00:00:00
#SBATCH --array=1-100

ml R

LINE=$(sed -n ${SLURM_ARRAY_TASK_ID}p "analysis_NLSM_params.txt")
seed=$(echo $LINE | cut -d ' ' -f 1)

output=$(echo "../results/analysis-NLSM-seed${seed}.out")

cd ../code/
Rscript analysis_NLSM.R --seed "$seed" > "$output" 2>&1
