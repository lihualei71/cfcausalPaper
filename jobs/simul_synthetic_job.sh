#!/bin/bash

#SBATCH --job-name=cfcausal_synthetic
#SBATCH --output=../log/simul_synthetic_%a.out
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=3
#SBATCH --time=1-00:00:00
#SBATCH --array=1-200

ml R

LINE=$(sed -n ${SLURM_ARRAY_TASK_ID}p "simul_synthetic_params.txt")
n=$(echo $LINE | cut -d ' ' -f 1)
d=$(echo $LINE | cut -d ' ' -f 2)
seed=$(echo $LINE | cut -d ' ' -f 3)

output=$(echo "../results/simul-synthetic-n${n}-d${d}-seed${seed}.out")

cd ../code/
Rscript simul_synthetic.R --n "$n" --d "$d" --B "50" --seed "$seed" > "$output" 2>&1
