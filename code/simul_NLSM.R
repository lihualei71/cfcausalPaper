library("tidyverse")
library("cfcausal")
source("utils_real_expr.R")

#### Get parameters
if (!interactive()){
    suppressPackageStartupMessages(library("argparse"))

    parser <- ArgumentParser()

    parser$add_argument("--B", type = "integer", default = 0, help = "Nrep for Xlearner")
    parser$add_argument("--alpha", type = "double", default = 0.05, help = "random seed")
    parser$add_argument("--seed", type = "double", default = 100, help = "random seed")
    parser$add_argument("--ntrain", type = "integer", default = 1000, help = "Training sample size")
    parser$add_argument("--ntest", type = "integer", default = 5000, help = "Testing sample size")
    
    args <- parser$parse_args()

    B <- args$B
    alpha <- args$alpha
    seed <- args$seed
    ntrain <- args$ntrain
    ntest <- args$ntest
} else {
    B <- 0
    alpha <- 0.05
    seed <- 199
    ntrain <- 1000
    ntest <- 3000
}

set.seed(seed * 2020)
filename <- paste0("../data/NLSM_simul",
                   "_alpha", alpha,
                   "_seed", seed,
                   "_ntr", ntrain,
                   "_nte", ntest,
                   "_B", B,
                   ".RData")
load("../data/NLSM_simul.RData")

res <- try(ITE_expr(data, ntrain, ntest, alpha, B))
if (class(res) != "try-error"){
    save(res, file = filename)
    print(paste0("seed ", seed, " succeeds!"))
} else {
    print(paste0("seed ", seed, " fails!"))
}
