library("dplyr")
library("cfcausal")
source("utils_synthetic_expr.R")

#### Get parameters
if (!interactive()){
    suppressPackageStartupMessages(library("argparse"))

    parser <- ArgumentParser()

    parser$add_argument("--n", type = "integer", default = 1000, help = "Sample size")
    parser$add_argument("--d", type = "integer", default = 10, help = "Dimension")
    parser$add_argument("--B", type = "integer", default = 0, help = "Nrep for Xlearner")
    parser$add_argument("--seed", type = "double", default = 1, help = "random seed")
    
    args <- parser$parse_args()

    n <- args$n
    d <- args$d
    B <- args$B    
    seed <- args$seed
} else {
    n <- 500
    d <- 2
    B <- 0    
    seed <- 0
}

ntest <- 10000
set.seed(seed)
filename <- paste0("../data/synthetic_simul",
                   "_n", n,
                   "_d", d,
                   "_seed", seed,
                   ".RData")
res <- list()

## Expr 1 (equation (27) of Wager and Athey (2018)): homoscedastic errors + independent covariates 
Xfun <- function(n, d){
    matrix(runif(n * d), nrow = n, ncol = d)
}
taufun <- function(X){
    2 / (1 + exp(-12 * (X[, 1] - 0.5))) * 2 / (1 + exp(-12 * (X[, 2] - 0.5)))
}
sdfun <- function(X){
    rep(1, nrow(X))
}
psfun <- function(X){
    (1 + pbeta(X[, 1], 2, 4)) / 4
}
errdist <- rnorm

res[[1]] <- Cf_expr(n, d, ntest, Xfun, taufun, sdfun, psfun, errdist, B = B, strata = c("tau"))

save(res, file = filename)
print("Expr 1 finished")

## Expr 2 (equation (27) of Wager and Athey (2018)) heteroscedastic errors + independent covariates
Xfun <- function(n, d){
    matrix(runif(n * d), nrow = n, ncol = d)
}
taufun <- function(X){
    2 / (1 + exp(-12 * (X[, 1] - 0.5))) * 2 / (1 + exp(-12 * (X[, 2] - 0.5)))
}
sdfun <- function(X){
    -log(X[, 1] + 1e-9)
}
psfun <- function(X){
    (1 + pbeta(X[, 1], 2, 4)) / 4
}
errdist <- rnorm

res[[2]] <- Cf_expr(n, d, ntest, Xfun, taufun, sdfun, psfun, errdist, B = B, strata = c("tau", "std"))

save(res, file = filename)
print("Expr 2 finished")

## Expr 3 (equation (27) of Wager and Athey (2018)), homoscedastic errors + correlated covariates 
rho <- 0.9
Xfun <- function(n, d){
    X <- matrix(rnorm(n * d), nrow = n, ncol = d)
    fac <- rnorm(n)
    X <- X * sqrt(1 - rho) + fac * sqrt(rho)
    pnorm(X)
}
taufun <- function(X){
    2 / (1 + exp(-12 * (X[, 1] - 0.5))) * 2 / (1 + exp(-12 * (X[, 2] - 0.5)))
}
sdfun <- function(X){
    rep(1, nrow(X))
}
psfun <- function(X){
    (1 + pbeta(X[, 1], 2, 4)) / 4
}
errdist <- rnorm

res[[3]] <- Cf_expr(n, d, ntest, Xfun, taufun, sdfun, psfun, errdist, B = B, strata = c("tau"))

save(res, file = filename)
print("Expr 3 finished")

## Expr 4 (equation (27) of Wager and Athey (2018)) heteroscedastic errors + correlated covariates 
rho <- 0.9
Xfun <- function(n, d){
    X <- matrix(rnorm(n * d), nrow = n, ncol = d)
    fac <- rnorm(n)
    X <- X * sqrt(1 - rho) + fac * sqrt(rho)
    pnorm(X)
}
taufun <- function(X){
    2 / (1 + exp(-12 * (X[, 1] - 0.5))) * 2 / (1 + exp(-12 * (X[, 2] - 0.5)))
}
sdfun <- function(X){
    -log(X[, 1] + 1e-9)
}
psfun <- function(X){
    (1 + pbeta(X[, 1], 2, 4)) / 4
}
errdist <- rnorm

res[[4]] <- Cf_expr(n, d, ntest, Xfun, taufun, sdfun, psfun, errdist, B = B, strata = c("tau", "std"))

save(res, file = filename)
print("Expr 4 finished")
