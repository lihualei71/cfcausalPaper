# Paper Repository

This repository contains the code to implement all examples in our paper: [Conformal Inference of Counterfactuals and Individual Treatment Effects](https://arxiv.org/abs/2006.06138). 

## Introduction
All R scripts are included in the folder `code/`. The bash files to submit jobs to the cluster are included in the folder `jobs/` (note that this depends on your cluster and the bash file might need to be changed accordingly). The outputs and the plots are included in the folder `data/` and the folder `figs/`, respectively. 

## Installing the package
The [cfcausal](https://github.com/lihualei71/cfcausal) package needs to be installed.
```
if (!require("devtools")){
    install.packages("devtools")
}
devtools::install_github("lihualei71/cfcausal")
```

The following packages are required to be installed as well: [grf](https://cran.r-project.org/web/packages/grf/grf.pdf), [randomForest](https://cran.r-project.org/web/packages/randomForest/randomForest.pdf), [gbm](https://cran.r-project.org/web/packages/gbm/gbm.pdf), [bartMachine](https://cran.r-project.org/web/packages/bartMachine/bartMachine.pdf), [causalToolbox](https://github.com/soerenkuenzel/causalToolbox), [tidyverse](https://www.tidyverse.org/), [ggplot2](https://ggplot2.tidyverse.org/) and [argparse](https://cran.r-project.org/web/packages/argparse/index.html).

## R scripts
The folder `code/` contains all R scripts:

- `simul_synthetic.R` is an executable R script that produces the result of a single run of the numerical study in Section 3.6 with four scenarios: homoscedastic/heteroscedastic errors + independent/correlated covariates. It takes four inputs: `--n` for the sample size, `--d` for the dimension, `--B` for the number of bootstrap draws in X-learner and `--seed` for the random seed. The output `res` is a list of length four with each corresponding to a scenario. Each `res[[i]]` is a list of three with `res[[i]]$tau` being the results for CATE, `res[[i]]$Y` being the results for ITE and `res[[i]]$cond` being the conditional coverage for ITE. The object `res` will be stored in `data/` with filename "synthetic_simul_n${n}_d${d}_seed${seed}.RData". It can be implemented both interactively in an R console by setting the parameters in line 23-26, or noninteractively in a shell by running the script with aforementioned inputs. Below is a toy example that runs for a few minutes on a laptop.
```
Rscript simul_synthetic.R --n 500 --d 2 --B 0 --seed 199
```
- `utils_synthetic_expr.R` implements helpers for `simul_synthetic.R`.
- `simul_NLSM.R` is an executable R script that produces the result of a single run of the numerical study in Section 4.4. It takes five inputs: `--B` for the number of bootstrap draws in X-learner, `--alpha` for the level, `--seed` for the random seed, `--ntrain` for the size of training set and `--ntest` for the size of testing set. The output `res` is a list of length two with `res$marginal` being the results for unconditional coverage and `res$cond` being the results for conditional coverage. The object `res` will be stored in `data/` with filename "NLSM_simul_alpha${alpha}_seed${seed}_ntr${ntr}_nte${nte}_B${B}.RData".It can be implemented both interactively in an R console by setting the parameters in line 25-29, or noninteractively in a shell by running the script with aforementioned inputs. Below is a toy example that runs for a few minutes on a laptop.
```
Rscript simul_NLSM.R --ntr 1000 --nte 5000 --B 0 --alpha 0.05 --seed 199
```
- `simul_prep_NLSM.R` generates the synthetic data to be used in `simul_NLSM.R` based on the NLSM data from [ACIC 2018 workshop](https://github.com/grf-labs/grf/blob/master/experiments/acic18/synthetic_data.csv). The raw data is stored in `data/NLSM_data.csv` and the generated data is stored in `data/NLSM_simul.RData`
- `utils_real_expr.R` implements helpers for `simul_NLSM.R`.
- `analysis_NLSM.R` is an executable R script that produces the result of a single run of the numerical study in Section 4.5. It takes one input `--seed` for the random seed. The output `res` is a data.frame. The object `res` will be stored in `data/` with filename "analysis_NLSM_seed${seed}.RData".It can be implemented both interactively in an R console by setting the parameters in line 18, or noninteractively in a shell by running the script with aforementioned inputs. Below is a toy example that runs for a few minutes on a laptop.
```
Rscript analysis_NLSM.R --seed 199
```
- `postprocess_synthetic_simul.R`, `postprocess_NLSM_simul.R` and `postprocess_NLSM_analysis.R` postprocess the results obtained from cluster jobs. See the last section for details.

- `plot_synthetic_simul.R`, `plot_NLSM_simul.R` and `plot_NLSM_analysis.R` generate the plots in the paper. See the last section for details.

## Submitting jobs, Postprocessing results, and generating plots
The folder `jobs/` contains all bash scripts to submit jobs to the cluster. The numerical studies take ~1200 CPU hours in total. To run each .sh file, create the following folders first.
```
mkdir log results raw_data_cluster
```
`log/` stores the system reports, `results/` stores the R stdouts, and `raw_data_cluster` stores the output/results of each job.

Upon finishing all jobs, run `postprocess_synthetic_simul.R`, `postprocess_NLSM_simul.R` and `postprocess_NLSM_analysis.R`. They generate `simul_synthetic_results.RData`, `simul_NLSM_results.RData` and `analysis_NLSM_results.RData` respectively in the folder `data/`, which merge the results from different cores. 

Finally, run `plot_synthetic_simul.R`, `plot_NLSM_simul.R` and `plot_NLSM_analysis.R` to generate the figures in the paper.
