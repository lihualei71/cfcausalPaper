#!/bin/bash

#SBATCH --job-name=cfcausal_NLSM
#SBATCH --output=../log/simul_NLSM_%a.out
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --time=1-00:00:00
#SBATCH --array=1-100

ml R

LINE=$(sed -n ${SLURM_ARRAY_TASK_ID}p "simul_NLSM_params.txt")
seed=$(echo $LINE | cut -d ' ' -f 1)
alpha=$(echo $LINE | cut -d ' ' -f 2)

output=$(echo "../results/simul-NLSM-alpha${alpha}-seed${seed}.out")

cd ../code/
Rscript simul_NLSM.R --ntrain "1000" --ntest "5000" --B "50" --alpha "$alpha" --seed "$seed" > "$output" 2>&1
