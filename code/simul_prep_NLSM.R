library("tidyverse")

## Read NLSM data
data <- read.csv("../data/NLSM_data.csv")
X <- data %>% select(-Z, -Y) %>% as.matrix
Y <- data$Y
T <- data$Z
n <- length(Y)

## Data splitting
set.seed(2020)
tmp <- sample(n, n)
ntrain <- ceiling(0.2 * n)
trainid <- tmp[1:ntrain]
valid <- tmp[(ntrain + 1):n]

Xtrain <- X[trainid, ]
Ytrain <- Y[trainid]
Ttrain <- T[trainid]

Y1 <- Ytrain[Ttrain == 1]
X1 <- Xtrain[Ttrain == 1, ]
Y0 <- Ytrain[Ttrain == 0]
X0 <- Xtrain[Ttrain == 0, ]

X <- X[-trainid, ]

## Set \tau(x) based on Section 2 of Carvalho et al. [2019]
gamma <- rnorm(76) * 0.105
Etau <- 0.228 +
        0.05 * (X[, 7] < 0.07) -
        0.05 * (X[, 8] < -0.69) -
        0.08 * (X[, 3] %in% c(1, 13, 14)) + 
        gamma[X[, 1]]

## Fit models
EY0 <- cfcausal:::RF(Y0, X0, X)
IQR0 <- cfcausal:::quantRF(Y0, X0, X, quantiles = c(0.25, 0.75))
IQR0 <- IQR0[, 2] - IQR0[, 1]
IQR1 <- cfcausal:::quantRF(Y1, X1, X, quantiles = c(0.25, 0.75))
IQR1 <- IQR1[, 2] - IQR1[, 1]
ps <- cfcausal:::RF(as.factor(Ttrain), Xtrain, X)
ps <- pmin(pmax(ps, 0.1), 0.9)

## Store the data
data <- list(X = X, EY0 = EY0, Etau = Etau, IQR0 = IQR0, IQR1 = IQR1, ps = ps)
save(data, file = "../data/NLSM_simul.RData")
