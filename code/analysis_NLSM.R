library("tidyverse")
library("cfcausal")
source("utils_real_expr.R")
data <- read.csv("../data/NLSM_data.csv")

#### Get parameters
if (!interactive()){
    suppressPackageStartupMessages(library("argparse"))

    parser <- ArgumentParser()

    parser$add_argument("--seed", type = "double", default = 100, help = "random seed")
    
    args <- parser$parse_args()

    seed <- as.integer(args$seed)
} else {
    seed <- 199
}

set.seed(seed)
filename <- paste0("../raw_data_cluster/analysis_NLSM_seed", seed, ".RData")

X <- data %>% select(-Z, -Y) %>% as.matrix
Y <- data$Y
T <- data$Z
n <- length(Y)

alphalist <- 0.05 * (1:10)
res <- data.frame()
for (alpha in alphalist){
    ## data splitting
    id <- sample(n, n)
    trainid <- id[1:floor(n / 2)]
    testid <- id[(floor(n / 2) + 1):n]
    Xtrain <- X[trainid, ]
    Ytrain <- Y[trainid]
    Ttrain <- T[trainid]
    Xtest <- X[testid, ]
    Ytest <- Y[testid]
    Ttest <- T[testid]

    ## Get confidence intervals
    CItest_fun <- conformalIte(Xtrain, Ytrain, Ttrain,
                               alpha = alpha,
                               algo = "nest",
                               type = "CQR",
                               quantiles = c(alpha / 2, 1 - alpha / 2),
                               outfun = "quantBART",
                               loquantile = 0.5,
                               upquantile = 0.5)
    CItest_CQR <- CItest_fun(Xtest)

    CItrain_fun <- conformalIte(Xtest, Ytest, Ttest,
                                alpha = alpha,
                                algo = "nest",
                                type = "CQR",
                                quantiles = c(alpha / 2, 1 - alpha / 2),
                                outfun = "quantBART",
                                loquantile = 0.5,
                                upquantile = 0.5)
    CItrain_CQR <- CItrain_fun(Xtrain)

    CI_CQR <- matrix(NA, n, 2)
    CI_CQR[trainid, ] <- as.matrix(CItrain_CQR)
    CI_CQR[testid, ] <- as.matrix(CItest_CQR)
    df <- data.frame(
        alpha = alpha,
        avglen = mean(CI_CQR[, 2] - CI_CQR[, 1]),
        dflen = sd(CI_CQR[, 2] - CI_CQR[, 1]),
        pos = mean(CI_CQR[, 1] > 0),
        neg = mean(CI_CQR[, 2] < 0))
    res <- rbind(res, df)
    save(res, file = filename)
    
    print(alpha)
}
