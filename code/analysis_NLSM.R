library("tidyverse")
library("cfcausal")
source("utils_real_expr.R")
data <- read.csv("../data/synthetic_data.csv")

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

set.seed(seed * 2020)
X <- data %>% select(-Z, -Y) %>% as.matrix
Y <- data$Y
T <- data$Z
n <- length(Y)

alphalist <- 0.05 * 10
res <- data.frame()
for (alpha in alphalist){
    id <- sample(n, n)
    trainid <- id[1:floor(n / 2)]
    testid <- id[(floor(n / 2) + 1):n]
    Xtrain <- X[trainid, ]
    Ytrain <- Y[trainid]
    Ttrain <- T[trainid]
    Xtest <- X[testid, ]
    Ytest <- Y[testid]
    Ttest <- T[testid]

    CItest_fun <- conformalIte(Xtrain, Ytrain, Ttrain,
                               alpha = alpha,
                               algo = "nest",
                               type = "CQR",
                               quantiles = c(alpha / 2, 1 - alpha / 2),
                               outfun = "quantBART")
    CItest_CQR <- CItest_fun(Xtest)

    CItrain_fun <- conformalIte(Xtest, Ytest, Ttest,
                                alpha = alpha,
                                algo = "nest",
                                type = "CQR",
                                quantiles = c(alpha / 2, 1 - alpha / 2),
                                outfun = "quantBART")
    CItrain_CQR <- CItrain_fun(Xtrain)

    CI_CQR <- matrix(NA, n, 2)
    CI_CQR[trainid, ] <- CItrain_CQR
    CI_CQR[testid, ] <- CItest_CQR
    df <- data.frame(
        method = "CQR",
        alpha = alpha,
        avglen = mean(CI_CQR[, 2] - CI_CQR[, 1]),
        dflen = sd(CI_CQR[, 2] - CI_CQR[, 1]),
        pos = mean(CI_CQR[, 1] > 0),
        neg = mean(CI_CQR[, 2] < 0))
    res <- rbind(res, df)

    CItest_BART <- bart_inexact_ITE_CI(
        Xtrain, Ytrain, Ttrain, Xtest, alpha = alpha)
    CItrain_BART <- bart_inexact_ITE_CI(
        Xtest, Ytest, Ttest, Xtrain, alpha = alpha)
    CI_BART <- matrix(NA, n, 2)
    CI_BART[trainid, ] <- CItrain_BART
    CI_BART[testid, ] <- CItest_BART
    df <- data.frame(
        method = "BART",
        alpha = alpha,
        avglen = mean(CI_BART[, 2] - CI_BART[, 1]),
        dflen = sd(CI_BART[, 2] - CI_BART[, 1]),
        pos = mean(CI_BART[, 1] > 0),
        neg = mean(CI_BART[, 2] < 0))
    res <- rbind(res, df)
    
    print(alpha)
}
