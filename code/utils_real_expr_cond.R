library("tidyverse")

cond_coverage <- function(CI, target, fx, nstrata = 10){
    betas <- (0:nstrata) / nstrata
    qt <- quantile(fx, betas)
    qt[1] <- qt[1] - 1e-6
    qt[length(qt)] <- qt[length(qt)] + 1e-6
    strata <- cut(fx, qt)
    levels(strata) <-
        paste0("(",
               round(head(betas, -1) * 100, 0), "%", 
               ", ",
               round(tail(betas, -1) * 100, 0), "%",
               "]")
    cover <- (CI[, 1] <= target & CI[, 2] >= target)
    df <- data.frame(strata = strata, cover = cover)
    df %>% group_by(strata) %>%
        summarize(cr = mean(cover)) %>%
        ungroup()
}

nonconformalInt <- function(X, CI, Xtest,
                            fun = cfcausal:::Boosting,
                            ...){
    pred_hi <- fun(Y = CI[, 2], X = X, Xtest = Xtest, ...)
    pred_lo <- fun(Y = CI[, 1], X = X, Xtest = Xtest, ...)
    CI <- cbind(pred_lo, pred_hi)
    return(CI)
}

bart_ITE_cond <- function(X, Y, T,
                          Xtest, tautest, CATEtest,
                          alpha = 0.05){
    X <- as.data.frame(X)
    X1 <- X[T == 1, ]
    X0 <- X[T == 0, ]
    Y1 <- Y[T == 1]
    Y0 <- Y[T == 0]
    fit1 <- bartMachine::bartMachine(X1, Y1,
                                     verbose = FALSE)
    fit0 <- bartMachine::bartMachine(X0, Y0,
                                     verbose = FALSE)
    CI1 <- bartMachine::calc_prediction_intervals(
                            fit1, new_data = Xtest, pi_conf = 1 - alpha / 2)$interval
    CI0 <- bartMachine::calc_prediction_intervals(
                            fit0, new_data = Xtest, pi_conf = 1 - alpha / 2)$interval

    CI <- cbind(CI1[, 1] - CI0[, 2],
                CI1[, 2] - CI0[, 1])
    return(CI)
}

bart_ITE <- function(X, Y, T, Xtest,
                     alpha = 0.05){
    X <- as.data.frame(X)
    X1 <- X[T == 1, ]
    X0 <- X[T == 0, ]
    Y1 <- Y[T == 1]
    Y0 <- Y[T == 0]
    fit1 <- bartMachine::bartMachine(X1, Y1,
                                     verbose = FALSE)
    fit0 <- bartMachine::bartMachine(X0, Y0,
                                     verbose = FALSE)
    CI1 <- bartMachine::calc_prediction_intervals(
                            fit1, new_data = Xtest, pi_conf = 1 - alpha / 2)$interval
    CI0 <- bartMachine::calc_prediction_intervals(
                            fit0, new_data = Xtest, pi_conf = 1 - alpha / 2)$interval

    return(list(CI1 = CI1, CI0 = CI0))
}

bart_inexact_ITE_cond <- function(X, Y, T,
                                  Xtest, tautest, CATEtest,
                                  alpha = 0.05,
                                  cfprop = 0.5){

    n <- length(Y)
    tmp <- sample(n, n)
    ncf <- ceiling(n * cfprop)
    cfid <- tmp[1:ncf]
    valid <- tmp[(ncf + 1):n]

    Xcf <- X[cfid, ]
    Ycf <- Y[cfid]
    Tcf <- T[cfid]

    Xval <- X[valid, ]
    Yval <- Y[valid]
    Tval <- T[valid]

    Y1cf <- Y0cf <- Ycf
    Y1cf[Tcf == 0] <- NA
    Y0cf[Tcf == 1] <- NA
    inds <- which(Tval == 1)
    
    res <- bart_ITE(Xcf, Ycf, Tcf, Xval, 2 * alpha)
    CIval <- res$CI0
    CIval[inds, 1] <- Yval[inds] - res$CI0[inds, 2]
    CIval[inds, 2] <- Yval[inds] - res$CI0[inds, 1]
    CIval[-inds, 1] <- res$CI1[-inds, 1] - Yval[-inds]
    CIval[-inds, 2] <- res$CI1[-inds, 2] - Yval[-inds]

    CI <- nonconformalInt(Xval, CIval, Xtest)
    return(CI)
}

cfcausal_inexact_ITE_cond <- function(X, Y, T,
                                      Xtest, tautest, CATEtest,
                                      alpha = 0.05,
                                      useCV = FALSE,
                                      ...){
    CIfun <- cfcausal::conformalIte(X, Y, T,
                                    alpha,
                                    algo = "nest",
                                    exact = FALSE,
                                    ...)
    CI <- CIfun(Xtest)
    return(CI)
}

cfcausal_exact_ITE_cond <- function(X, Y, T,
                                    Xtest, tautest, CATEtest,
                                    alpha = 0.05,
                                    useCV = FALSE,
                                    ...){
    CIfun <- cfcausal::conformalIte(X, Y, T,
                                    alpha,
                                    algo = "nest",
                                    exact = TRUE,
                                    ...)
    CI <- CIfun(Xtest)
    return(CI)
}

cfcausal_naive_ITE_cond <- function(X, Y, T,
                                    Xtest, tautest, CATEtest,
                                    alpha = 0.05,
                                    useCV = FALSE,
                                    ...){
    CIfun <- cfcausal::conformalIte(X, Y, T,
                                    alpha,
                                    algo = "naive",
                                    ...)
    CI <- CIfun(Xtest)
    return(CI)
}

summary_CI_cond <- function(CI, tau, CATE, std, types, name){
    res <- data.frame()
    if ("CATE" %in% types){
        res_CATE <- cond_coverage(CI, tau, CATE)
        res_CATE <- data.frame(type = "CATE", method = name,
                               res_CATE)
        res <- rbind(res, res_CATE)
    }
    if ("std" %in% types){
        res_std <- cond_coverage(CI, tau, std)
        res_std <- data.frame(type = "std", method = name,
                              res_std)
        res <- rbind(res, res_std)
    }
    return(res)
}

gen_data <- function(data,
                     ntrain = 10000,
                     ntest = 10000,
                     ps_shift = 0.05,
                     sd_deflate = 0.1,
                     rho = 1){
    X <- data$X
    EY0 <- data$EY0
    Etau <- data$Etau
    EY1 <- EY0 + Etau
    IQR0 <- data$IQR0
    IQR1 <- data$IQR1
    ps <- data$ps

    n <- length(ps)
    sigma0 <- sd_deflate * IQR0
    sigma1 <- sd_deflate * IQR1
    std <- sqrt(sigma0^2 + sigma1^2 + 2*rho*sigma0*sigma1)
    err0 <- rnorm(n)
    err1 <- err0 * rho + rnorm(n) * sqrt(1 - rho^2)
    Y0 <- EY0 + sigma0 * err0
    Y1 <- EY1 + sigma1 * err1

    ids <- sample(n, n, FALSE)
    trainid <- ids[1:ntrain]
    testid <- ids[ntrain + 1:ntest]
    Xtrain <- X[trainid, ]
    pstrain <- pmin(ps[trainid] + ps_shift, 1)
    Ttrain <- as.numeric(runif(ntrain) <= pstrain)
    Y1train <- Y1[trainid]
    Y0train <- Y0[trainid]
    Ytrain <- ifelse(Ttrain == 1, Y1train, Y0train)
    
    Xtest <- X[testid, ]
    Y1test <- Y1[testid]
    Y0test <- Y0[testid]
    tautest <- Y1test - Y0test
    CATEtest <- Etau[testid]
    stdtest <- std[testid]
    pstest <- pmin(ps[testid] + ps_shift, 1)    
    Ttest <- as.numeric(runif(ntest) <= pstest)
    Ytest <- ifelse(Ttest == 1, Y1test, Y0test)

    list(Xtrain = Xtrain, Ttrain = Ttrain, Ytrain = Ytrain,
         Xtest = Xtest, Ttest = Ttest, Ytest = Ytest,
         tautest = tautest, CATEtest = CATEtest,
         stdtest = stdtest)
}

real_simul_ITE_cond <- function(data,
                                ntrain = 3000,
                                ntest = 3000,
                                ps_shift = 0,
                                sd_deflate = 0.5,
                                alpha = 0.05,
                                rho = 1,
                                types = c("CATE", "std")){
    data_simul <- gen_data(data, ntrain, ntest,
                           ps_shift, sd_deflate,
                           rho)
    Xtrain <- data_simul$Xtrain
    Ytrain <- data_simul$Ytrain
    Ttrain <- data_simul$Ttrain
    Xtest <- data_simul$Xtest
    tautest <- data_simul$tautest
    CATEtest <- data_simul$CATEtest
    stdtest <- data_simul$stdtest

    res <- data.frame()

    bart_exact_CI <- try(bart_ITE_cond(
        Xtrain, Ytrain, Ttrain,
        Xtest, tautest, CATEtest,
        alpha = alpha))
    if (class(bart_exact_CI) != "try-error"){
        bart_exact_res <- summary_CI_cond(bart_exact_CI, tautest, CATEtest, stdtest, types, "BART_exact")
        res <- rbind(res, bart_exact_res)
    }

    bart_inexact_CI <- try(bart_inexact_ITE_cond(
        Xtrain, Ytrain, Ttrain,
        Xtest, tautest, CATEtest,
        alpha = alpha))
    if (class(bart_inexact_CI) != "try-error"){
        bart_inexact_res <- summary_CI_cond(bart_inexact_CI, tautest, CATEtest, stdtest, types, "BART_inexact")
        res <- rbind(res, bart_inexact_res)
    }

    CQR_params <- expand.grid(
        method = c("RF", "Boosting", "BART"),
        q = c(alpha / 2),
        usecv = c(FALSE))
    for (i in 1:nrow(CQR_params)){
        method <- as.character(CQR_params[i, 1])
        q <- as.numeric(CQR_params[i, 2])
        useCV <- as.logical(CQR_params[i, 3])
        outfun <- switch(method,
                         RF = cfcausal:::quantRF,
                         Boosting = cfcausal:::quantBoosting,
                         BART = cfcausal:::quantBART)
        if (!useCV){
            name0 <- paste0("CQR-", method, "-", q)
        } else {
            name0 <- paste0("CQR-CV-", method, "-", q)
        }
        cfcausal_inexact_CI <-
            try(cfcausal_inexact_ITE_cond(
                Xtrain, Ytrain, Ttrain,
                Xtest, tautest, CATEtest,
                alpha, useCV = useCV,
                quantiles = c(q, 1-q),
                outfun = outfun))
        if (class(cfcausal_inexact_CI) != "try-error"){
            cfcausal_inexact_res <- summary_CI_cond(cfcausal_inexact_CI, tautest, CATEtest, stdtest, types, "cfcausal_inexact")
            res <- rbind(res, cfcausal_inexact_res)
        }

        cfcausal_exact_CI <-
            try(cfcausal_exact_ITE_cond(
                Xtrain, Ytrain, Ttrain,
                Xtest, tautest, CATEtest,
                alpha, useCV = useCV,
                quantiles = c(q, 1-q),
                outfun = outfun))
        if (class(cfcausal_exact_CI) != "try-error"){
            cfcausal_exact_res <- summary_CI_cond(cfcausal_exact_CI, tautest, CATEtest, stdtest, types, "cfcausal_exact")
            res <- rbind(res, cfcausal_exact_res)
        }

        cfcausal_naive_CI <-
            try(cfcausal_naive_ITE_cond(
                Xtrain, Ytrain, Ttrain,
                Xtest, tautest, CATEtest,
                alpha, useCV = useCV,
                quantiles = c(q, 1-q),
                outfun = outfun))
        if (class(cfcausal_naive_CI) != "try-error"){
            cfcausal_naive_res <- summary_CI_cond(cfcausal_naive_CI, tautest, CATEtest, stdtest, types, "cfcausal_naive")
            res <- rbind(res, cfcausal_naive_res)
        }        
    }

    return(res)
}
