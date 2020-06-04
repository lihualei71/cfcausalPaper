summary_CI <- function(target, CI){
    len <- mean(CI[, 2] - CI[, 1])
    cr <- mean(target >= CI[, 1] & target <= CI[, 2])

    return(list(cr = cr, len = len))
}

summary_CI_cond <- function(target, CI, fx, nstrata = 10){
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

CF_ITE_CI <- function(X, Y, T, Xtest, alpha = 0.05){
    fit <- grf::causal_forest(X, Y, T)
    pred <- predict(fit, Xtest, estimate.variance = TRUE)
    cutoff <- qnorm(alpha / 2, lower.tail = FALSE)
    CI <- data.frame(low = pred[, 1] - cutoff * sqrt(pred[, 2]),
                     high = pred[, 1] + cutoff * sqrt(pred[, 2]))
    return(CI)
}

xlearner_ITE_CI <- function(X, Y, T, Xtest, alpha = 0.05, B = 1){
    xl_rf <- causalToolbox::X_RF(feat = X, tr = T, yobs = Y, nthread = 1)
    CI <- causalToolbox::CateCI(xl_rf, Xtest, B = B,
                                verbose = FALSE, nthread = 1)[, 2:3]
    return(CI)
}

bart_naive_ITE_CI <- function(X, Y, T, Xtest, alpha = 0.05){
    X <- as.data.frame(X)
    X1 <- X[T == 1, ]
    X0 <- X[T == 0, ]
    Y1 <- Y[T == 1]
    Y0 <- Y[T == 0]
    fit1 <- bartMachine::bartMachine(X1, Y1, verbose = FALSE)
    fit0 <- bartMachine::bartMachine(X0, Y0, verbose = FALSE)
    CI1 <- bartMachine::calc_prediction_intervals(fit1, new_data = Xtest, pi_conf = 1 - alpha / 2)$interval
    CI0 <- bartMachine::calc_prediction_intervals(fit0, new_data = Xtest, pi_conf = 1 - alpha / 2)$interval

    CI <- cbind(CI1[, 1] - CI0[, 2],
                CI1[, 2] - CI0[, 1])
    return(list(CI = CI, CI1 = CI1, CI0 = CI0))
}

bart_inexact_ITE_CI <- function(X, Y, T, Xtest, alpha = 0.05,
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
    
    res <- bart_naive_ITE_CI(Xcf, Ycf, Tcf, Xval, 2 * alpha)
    CI <- res$CI0
    CI[inds, 1] <- Yval[inds] - res$CI0[inds, 2]
    CI[inds, 2] <- Yval[inds] - res$CI0[inds, 1]
    CI[-inds, 1] <- res$CI1[-inds, 1] - Yval[-inds]
    CI[-inds, 2] <- res$CI1[-inds, 2] - Yval[-inds]

    pred_hi <- cfcausal:::Boosting(Y = CI[, 2], X = Xval, Xtest = Xtest)
    pred_lo <- cfcausal:::Boosting(Y = CI[, 1], X = Xval, Xtest = Xtest)
    CI <- cbind(pred_lo, pred_hi)
    return(CI)
}

CQR_inexact_ITE_CI <- function(X, Y, T, Xtest, alpha = 0.05,
                               ...){
    CIfun <- cfcausal::conformalIte(X, Y, T,
                                    alpha,
                                    algo = "nest",
                                    exact = FALSE,
                                    ...)
    CI <- CIfun(Xtest)
    return(CI)
}

CQR_exact_ITE_CI <- function(X, Y, T, Xtest, alpha = 0.05, ...){
    CIfun <- cfcausal::conformalIte(X, Y, T,
                                    alpha,
                                    algo = "nest",
                                    exact = TRUE,
                                    ...)
    CI <- CIfun(Xtest)
    return(CI)
}

CQR_naive_ITE_CI <- function(X, Y, T, Xtest, alpha = 0.05, ...){
    CIfun <- cfcausal::conformalIte(X, Y, T,
                                    alpha,
                                    algo = "naive",
                                    ...)
    CI <- CIfun(Xtest)
    return(CI)
}

gen_data <- function(data, ntrain = 1000, ntest = 5000){
    X <- data$X
    EY0 <- data$EY0
    Etau <- data$Etau
    EY1 <- EY0 + Etau
    IQR0 <- data$IQR0
    IQR1 <- data$IQR1
    ps <- data$ps

    n <- length(ps)
    sigma0 <- 0.5 * IQR0
    sigma1 <- 0.5 * IQR1
    std <- sqrt(sigma0^2 + sigma1^2)
    err0 <- rnorm(n)
    err1 <- rnorm(n)
    Y0 <- EY0 + sigma0 * err0
    Y1 <- EY1 + sigma1 * err1

    ids <- sample(n, n, FALSE)
    trainid <- ids[1:ntrain]
    testid <- ids[ntrain + 1:ntest]
    Xtrain <- X[trainid, ]
    pstrain <- pmin(ps[trainid], 1)
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
    pstest <- pmin(ps[testid], 1)    
    Ttest <- as.numeric(runif(ntest) <= pstest)
    Ytest <- ifelse(Ttest == 1, Y1test, Y0test)

    list(Xtrain = Xtrain, Ttrain = Ttrain, Ytrain = Ytrain,
         Xtest = Xtest, Ttest = Ttest, Ytest = Ytest,
         tautest = tautest, CATEtest = CATEtest,
         stdtest = stdtest)
}

ITE_expr <- function(data,
                     ntrain = 3000,
                     ntest = 3000,
                     alpha = 0.05,
                     B = 0){
    data_simul <- gen_data(data, ntrain, ntest)
    Xtrain <- data_simul$Xtrain
    Ytrain <- data_simul$Ytrain
    Ttrain <- data_simul$Ttrain
    Xtest <- data_simul$Xtest
    tautest <- data_simul$tautest
    CATEtest <- data_simul$CATEtest
    stdtest <- data_simul$stdtest

    res <- data.frame()
    res_cond <- data.frame()
    
    CF_CI <- try(CF_ITE_CI(Xtrain, Ytrain, Ttrain, Xtest, alpha))
    if (class(CF_CI) != "try-error"){
        df <- summary_CI(tautest, CF_CI)
        df <- data.frame(method = "CF", df)
        res <- rbind(res, df)
        df_cond_tau <- summary_CI_cond(tautest, CF_CI, CATEtest, 10)
        df_cond_tau <- data.frame(method = "CF",
                                  type = "tau",
                                  df_cond_tau)
        res_cond <- rbind(res_cond, df_cond_tau)
        df_cond_std <- summary_CI_cond(tautest, CF_CI, stdtest, 10)
        df_cond_std <- data.frame(method = "CF",
                                  type = "std",
                                  df_cond_std)
        res_cond <- rbind(res_cond, df_cond_std)
    }

    if (B > 0 && alpha == 0.05){
        xlearner_CI <- try(xlearner_ITE_CI(Xtrain, Ytrain, Ttrain, Xtest, alpha, B))
        if (class(xlearner_CI) != "try-error"){
            df <- summary_CI(tautest, xlearner_CI)
            df <- data.frame(method = "xlearner", df)
            res <- rbind(res, df)
            df_cond_tau <- summary_CI_cond(tautest, xlearner_CI, CATEtest, 10)
            df_cond_tau <- data.frame(method = "xlearner",
                                      type = "tau",
                                      df_cond_tau)
            res_cond <- rbind(res_cond, df_cond_tau)
            df_cond_std <- summary_CI_cond(tautest, xlearner_CI, stdtest, 10)
            df_cond_std <- data.frame(method = "xlearner",
                                      type = "std",
                                      df_cond_std)
            res_cond <- rbind(res_cond, df_cond_std)
        }
    }
    
    bart_naive_CI <- try(bart_naive_ITE_CI(Xtrain, Ytrain, Ttrain, Xtest, alpha)$CI)
    if (class(bart_naive_CI) != "try-error"){
        df <- summary_CI(tautest, bart_naive_CI)
        df <- data.frame(method = "bart_naive", df)
        res <- rbind(res, df)
        df_cond_tau <- summary_CI_cond(tautest, bart_naive_CI, CATEtest, 10)
        df_cond_tau <- data.frame(method = "bart_naive",
                                  type = "tau",
                                  df_cond_tau)
        res_cond <- rbind(res_cond, df_cond_tau)
        df_cond_std <- summary_CI_cond(tautest, bart_naive_CI, stdtest, 10)
        df_cond_std <- data.frame(method = "bart_naive",
                                  type = "std",
                                  df_cond_std)
        res_cond <- rbind(res_cond, df_cond_std)
    }

    bart_inexact_CI <- try(bart_inexact_ITE_CI(Xtrain, Ytrain, Ttrain, Xtest, alpha))
    if (class(bart_inexact_CI) != "try-error"){
        df <- summary_CI(tautest, bart_inexact_CI)
        df <- data.frame(method = "bart_inexact", df)
        res <- rbind(res, df)
        df_cond_tau <- summary_CI_cond(tautest, bart_inexact_CI, CATEtest, 10)
        df_cond_tau <- data.frame(method = "bart_inexact",
                                  type = "tau",
                                  df_cond_tau)
        res_cond <- rbind(res_cond, df_cond_tau)
        df_cond_std <- summary_CI_cond(tautest, bart_inexact_CI, stdtest, 10)
        df_cond_std <- data.frame(method = "bart_inexact",
                                  type = "std",
                                  df_cond_std)
        res_cond <- rbind(res_cond, df_cond_std)
    }

    outfun <- "quantBART"
    quantiles <- c(alpha / 2, 1 - alpha / 2)
    CQR_inexact_CI <-
        try(CQR_inexact_ITE_CI(Xtrain, Ytrain, Ttrain,
                               Xtest, alpha, 
                               quantiles = quantiles,
                               outfun = outfun))
    if (class(CQR_inexact_CI) != "try-error"){
        df <- summary_CI(tautest, CQR_inexact_CI)
        df <- data.frame(method = "CQR_inexact", df)
        res <- rbind(res, df)
        df_cond_tau <- summary_CI_cond(tautest, CQR_inexact_CI, CATEtest, 10)
        df_cond_tau <- data.frame(method = "CQR_inexact",
                                  type = "tau",
                                  df_cond_tau)
        res_cond <- rbind(res_cond, df_cond_tau)
        df_cond_std <- summary_CI_cond(tautest, CQR_inexact_CI, stdtest, 10)
        df_cond_std <- data.frame(method = "CQR_inexact",
                                  type = "std",
                                  df_cond_std)
        res_cond <- rbind(res_cond, df_cond_std)
    }

    CQR_exact_CI <-
        try(CQR_exact_ITE_CI(Xtrain, Ytrain, Ttrain,
                             Xtest, alpha, 
                             quantiles = quantiles,
                             outfun = outfun))
    if (class(CQR_exact_CI) != "try-error"){
        df <- summary_CI(tautest, CQR_exact_CI)
        df <- data.frame(method = "CQR_exact", df)
        res <- rbind(res, df)
        df_cond_tau <- summary_CI_cond(tautest, CQR_exact_CI, CATEtest, 10)
        df_cond_tau <- data.frame(method = "CQR_exact",
                                  type = "tau",
                                  df_cond_tau)
        res_cond <- rbind(res_cond, df_cond_tau)
        df_cond_std <- summary_CI_cond(tautest, CQR_exact_CI, stdtest, 10)
        df_cond_std <- data.frame(method = "CQR_exact",
                                  type = "std",
                                  df_cond_std)
        res_cond <- rbind(res_cond, df_cond_std)
    }

    CQR_naive_CI <-
        try(CQR_naive_ITE_CI(Xtrain, Ytrain, Ttrain,
                             Xtest, alpha, 
                             quantiles = quantiles,
                             outfun = outfun))
    if (class(CQR_naive_CI) != "try-error"){
        df <- summary_CI(tautest, CQR_naive_CI)
        df <- data.frame(method = "CQR_naive", df)
        res <- rbind(res, df)
        df_cond_tau <- summary_CI_cond(tautest, CQR_naive_CI, CATEtest, 10)
        df_cond_tau <- data.frame(method = "CQR_naive",
                                  type = "tau",
                                  df_cond_tau)
        res_cond <- rbind(res_cond, df_cond_tau)
        df_cond_std <- summary_CI_cond(tautest, CQR_naive_CI, stdtest, 10)
        df_cond_std <- data.frame(method = "CQR_naive",
                                  type = "std",
                                  df_cond_std)
        res_cond <- rbind(res_cond, df_cond_std)
    }

    list(marginal = res, cond = res_cond)
}