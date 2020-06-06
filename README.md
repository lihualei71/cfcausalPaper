# Paper Repository

This repository contains the code to implement all examples in our paper: [Conformal Inference of Counterfactuals and Individual Treatment Effects](https://arxiv.org/abs/). 

## Introduction
All R files are listed in the folder `code/`. The bash files to submit jobs to the cluster are listed in the folder `jobs/` (note that this depends on your cluster and the bash file might need to be changed correspondingly. See the section "cluster jobs" below for details). The outputs and the plots have already been contained in the folder `data/` and the folder `figs/`, respectively. 

## Installing the package
Run the following code in `code/`

```
if (!require("devtools")){
    install.packages("devtools")
}
devtools::install_github("lihualei71/")
```

The following packages are required to be installed: [grf](https://cran.r-project.org/web/packages/grf/grf.pdf), [randomForest](https://cran.r-project.org/web/packages/randomForest/randomForest.pdf), [gbm](https://cran.r-project.org/web/packages/gbm/gbm.pdf), [bartMachine](https://cran.r-project.org/web/packages/bartMachine/bartMachine.pdf) and [causalToolbox](https://github.com/soerenkuenzel/causalToolbox) 

## R files
The folder `code/` contains all R files:

- `simul_synthetic.R` is an executable R file that produces the result of a single run of the numerical study in Section 3 with four scenarios: homoscedastic/heteroscedastic errors + independent/correlated covariates. It takes four inputs: `--n` for the sample size, `--d` for the dimension, `--B` for the number of bootstrap draws in X-learner and `--seed` for the random seed. The output is a list of length four with each corresponding to a scenario. It can be run both interactively in an R console by setting the parameters in line 23-26 or non-interactively in a shell by calling the script with aforementioned inputs. Below is a toy example that runs for a few minutes on a laptop.
```
Rscript simul_synthetic.R --n 500 --d 2 --B 0 --seed 199
```

### Cluster jobs

### Figures 
