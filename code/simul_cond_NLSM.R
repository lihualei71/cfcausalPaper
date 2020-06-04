library("tidyverse")
load("../data/NLSM_simul.RData")
library("cfcausal")
source("real_simul_utils_cond.R")

#### Get parameters
if (!interactive()){
    suppressPackageStartupMessages(library("argparse"))

    parser <- ArgumentParser()

    parser$add_argument("--alpha", type = "double", default = 0.05, help = "random seed")
    parser$add_argument("--seed", type = "double", default = 100, help = "random seed")
    parser$add_argument("--ntrain", type = "integer", default = 1000, help = "Training sample size")
    parser$add_argument("--ntest", type = "integer", default = 5000, help = "Testing sample size")
    parser$add_argument("--psshift", type = "double", default = 0, help = "Propensity score shift")
    parser$add_argument("--sddeflate", type = "double", default = 0.5, help = "Standard Deviation deflation")
    parser$add_argument("--rho", type = "double", default = 0, help = "Correlation between Y(1) and Y(0)")
    
    args <- parser$parse_args()

    alpha <- args$alpha
    seed <- args$seed
    ntrain <- args$ntrain
    ntest <- args$ntest
    ps_shift <- args$psshift
    sd_deflate <- args$sddeflate
    rho <- args$rho
} else {
    alpha <- 0.05
    seed <- 199
    ntrain <- 1000
    ntest <- 5000
    ps_shift <- 0
    sd_deflate <- 0.5
    rho <- 0
}

set.seed(seed * 2020)
filename <- paste0("../data/NLSM_simul_cond_alpha", alpha,
                   "_seed", seed,
                   "_ntr", ntrain,
                   "_nte", ntest,
                   "_ps", ps_shift,
                   "_sd", sd_deflate,
                   "_rho", rho,
                   ".RData")
load("../data/NLSM_simul.RData")

## res <- try(real_simul_ITE(data, ntrain, ntest,
##                           ps_shift, sd_deflate, alpha,
##                           B, rho))
## if (class(res) != "try-error"){
##     save(res, file = filename)
##     print(paste0("seed ", seed, " succeeds!"))
## } else {
##     print(paste0("seed ", seed, " fails!"))
## }

res <- list()
set.seed(seed * 2020)
system.time(tmp <- try(real_simul_ITE_cond(data, ntrain, ntest,
                                           ps_shift, sd_deflate,
                                           alpha, rho)))
if (class(tmp) != "try-error"){
    res[[seed]] <- tmp
    print(seed)
} else {
    print(paste0("seed ", seed, " fails!"))
}
res <- do.call(rbind, res)
## save(res, file = "../data/NLSM_simul_ITE.RData")

## ## res <- list()
## ## for (seed in 1:1){
## ##     set.seed(seed * 2020)
## ##     tmp <- try(real_simul_ITE_Cf(data, ntrain, ntest,
## ##                                  ps_shift, sd_deflate,
## ##                                  alpha, B))
## ##     if (class(tmp) != "try-error"){
## ##         res[[seed]] <- tmp
## ##         print(seed)
## ##     } else {
## ##         print(paste0("seed ", seed, " fails!"))
## ##         next
## ##     }
## ## }
## ## res <- do.call(rbind, res)
## ## save(res, file = "../data/NLSM_simul_ITE_Cf.RData")
