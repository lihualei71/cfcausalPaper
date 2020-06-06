## Get the coverage and the average length of confidence
## intervals CI
summary_CI <- function(target, CI){
    len <- mean(CI[, 2] - CI[, 1])
    cr <- mean(target >= CI[, 1] & target <= CI[, 2])

    return(list(cr = cr, len = len))
}

## Get the conditional coverage of confidence intervals CI
## with fx being the stratified into nstrata folds based on
## its quantiles
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

## Get counterfactual intervals by Causal Forest
CF_Cf_CI <- function(X, Y, T, Xtest){
    fit <- grf::causal_forest(X, Y, T)
    pred <- predict(fit, Xtest, estimate.variance = TRUE)
    CI <- data.frame(low = pred[, 1] - 1.96 * sqrt(pred[, 2]),
                     high = pred[, 1] + 1.96 * sqrt(pred[, 2]))

    return(list(tau = CI, Y = CI))
}

## Get counterfactual intervals by X-learner
xlearner_Cf_CI <- function(X, Y, T, Xtest,
                           B = 50){
    if (B == 0){
        df_tau <- df_Y <- list(cr = NA, len = NA)
        return(list(tau = df_tau, Y1 = df_Y))
    }

    xl_rf <- causalToolbox::X_RF(feat = X, tr = T, yobs = Y, nthread = 1)
    cate_esti_rf <- causalToolbox::EstimateCate(xl_rf, Xtest)
    CI <- causalToolbox::CateCI(xl_rf, Xtest, B = B,
                                verbose = FALSE, nthread = 1)[, 2:3]
    return(list(tau = CI, Y = CI))
}

## Get counterfactual intervals by BART
bart_Cf_CI <- function(X, Y, Xtest){
    ids <- !is.na(Y)
    X <- as.data.frame(X)
    Xtest <- as.data.frame(Xtest)
    fit <- bartMachine::bartMachine(X[ids, ], Y[ids],
                                    verbose = FALSE)
    CI_tau <- bartMachine::calc_credible_intervals(fit, new_data = Xtest, ci_conf = 0.95)
    CI_Y <- bartMachine::calc_prediction_intervals(fit, new_data = Xtest, pi_conf = 0.95)$interval
    return(list(tau = CI_tau, Y = CI_Y))
}

## Get counterfactual intervals by weighted split CQR
CQR_Cf_CI <- function(X, Y, Xtest, outfun, quantiles){
    res <- cfcausal::conformalCf(X, Y, quantiles = quantiles, outfun = outfun, psfun = NULL, useCV = FALSE)
    CI <- predict(res, Xtest, alpha = 0.05)
    return(list(tau = CI, Y = CI))    
}

## The function to implement one run of the simulation study
## in Section 3.
##
## Inputs:
##   n: sample size
##   d: dimension
##   ntest: number of testing points
##   Xfun: the function to generate X
##   taufun: the function to generate E[Y(1)]
##   sdfun: the function to generate \sigma(x)
##   psfun: the function to generate e(x)
##   errdist: the error distribution
##   B: the number of bootstrap draws for X-learner. Default ##      to be 50 since it is very slow
##   strata: variables to be stratified with "tau" for CATE
##           and "std" for \sigma(x)
##
## Outputs:
##   tau: a data.frame with coverage and other information of CATE
##   Y: a data.frame with coverage and other information of ITE
##   cond: a data.frame with conditional coverage of ITE
Cf_expr <- function(n, d, ntest,
                    Xfun, taufun, sdfun, psfun, errdist,
                    B = 50,
                    strata = c("tau", "std")){
    ## Generate data
    X <- Xfun(n, d)
    Y0 <- rep(0, n)
    tau <- taufun(X)
    std <- sdfun(X)
    Y1 <- tau + std * errdist(n)
    ps <- psfun(X)
    T <- as.numeric(runif(n) < ps)
    Y <- Y0
    Y[T == 1] <- Y1[T == 1]
    Xtest <- Xfun(ntest, d)
    tautest <- taufun(Xtest)
    stdtest <- sdfun(Xtest)
    Y1test <- tautest + stdtest * errdist(n)
    pstest <- psfun(Xtest)

    res_tau <- data.frame()
    res_Y <- data.frame()
    res_cond <- data.frame()

    ## Causal Forest
    CF_CI <- try(CF_Cf_CI(X, Y, T, Xtest))
    if (class(CF_CI) != "try-error"){
        df_tau <- summary_CI(tautest, CF_CI$tau)
        df_tau <- data.frame(method = "CF",
                             cr = df_tau$cr,
                             len = df_tau$len)
        res_tau <- rbind(res_tau, df_tau)
        df_Y <- summary_CI(Y1test, CF_CI$Y)
        df_Y <- data.frame(method = "CF",
                           cr = df_Y$cr,
                           len = df_Y$len)
        res_Y <- rbind(res_Y, df_Y)
        if ("tau" %in% strata){
            df_cond_tau <- summary_CI_cond(Y1test, CF_CI$Y, tautest, 10)
            df_cond_tau <- data.frame(method = "CF",
                                      type = "tau",
                                      df_cond_tau)
            res_cond <- rbind(res_cond, df_cond_tau)
        }
        if ("std" %in% strata){
            df_cond_std <- summary_CI_cond(Y1test, CF_CI$Y, stdtest, 10)
            df_cond_std <- data.frame(method = "CF",
                                      type = "std",
                                      df_cond_std)
            res_cond <- rbind(res_cond, df_cond_std)
        }
    }

    ## X-learner
    if (B > 0){
        xlearner_CI <- try(xlearner_Cf_CI(X, Y, T, Xtest, B))
        if (class(xlearner_CI) != "try-error"){
            df_tau <- summary_CI(tautest, xlearner_CI$tau)
            df_tau <- data.frame(method = "xlearner",
                                 cr = df_tau$cr,
                                 len = df_tau$len)
            res_tau <- rbind(res_tau, df_tau)
            df_Y <- summary_CI(Y1test, xlearner_CI$Y)
            df_Y <- data.frame(method = "xlearner",
                               cr = df_Y$cr,
                               len = df_Y$len)
            res_Y <- rbind(res_Y, df_Y)
            if ("tau" %in% strata){
                df_cond_tau <- summary_CI_cond(Y1test, xlearner_CI$Y, tautest, 10)
                df_cond_tau <- data.frame(method = "xlearner",
                                          type = "tau",
                                          df_cond_tau)
                res_cond <- rbind(res_cond, df_cond_tau)
            }
            if ("std" %in% strata){
                df_cond_std <- summary_CI_cond(Y1test, xlearner_CI$Y, stdtest, 10)
                df_cond_std <- data.frame(method = "xlearner",
                                          type = "std",
                                          df_cond_std)
                res_cond <- rbind(res_cond, df_cond_std)
            }
        }
    }

    ## BART
    Y[T == 0] <- NA
    bart_CI <- try(bart_Cf_CI(X, Y, Xtest))
    if (class(bart_CI) != "try-error"){
        df_tau <- summary_CI(tautest, bart_CI$tau)
        df_tau <- data.frame(method = "bart",
                             cr = df_tau$cr,
                             len = df_tau$len)
        res_tau <- rbind(res_tau, df_tau)
        df_Y <- summary_CI(Y1test, bart_CI$Y)
        df_Y <- data.frame(method = "bart",
                           cr = df_Y$cr,
                           len = df_Y$len)
        res_Y <- rbind(res_Y, df_Y)
        if ("tau" %in% strata){
            df_cond_tau <- summary_CI_cond(Y1test, bart_CI$Y, tautest, 10)
            df_cond_tau <- data.frame(method = "bart",
                                      type = "tau", 
                                      df_cond_tau)
            res_cond <- rbind(res_cond, df_cond_tau)
        }
        if ("std" %in% strata){
            df_cond_std <- summary_CI_cond(Y1test, bart_CI$Y, stdtest, 10)
            df_cond_std <- data.frame(method = "bart",
                                      type = "std",
                                      df_cond_std)
            res_cond <- rbind(res_cond, df_cond_std)
        }
    }

    ## CQR
    for (outfun in c("quantRF", "quantBoosting", "quantBART")){
        name <- paste0("CQR-", outfun)
        quantiles <- c(0.025, 0.975)
        CQR_CI <- try(CQR_Cf_CI(X, Y, Xtest, outfun, quantiles))
        if (class(CQR_CI) != "try-error"){
            df_tau <- summary_CI(tautest, CQR_CI$tau)
            df_tau <- data.frame(method = name,
                                 cr = df_tau$cr,
                                 len = df_tau$len)
            res_tau <- rbind(res_tau, df_tau)
            df_Y <- summary_CI(Y1test, CQR_CI$Y)
            df_Y <- data.frame(method = name,
                               cr = df_Y$cr,
                               len = df_Y$len)
            res_Y <- rbind(res_Y, df_Y)
            if ("tau" %in% strata){
                df_cond_tau <- summary_CI_cond(Y1test, CQR_CI$Y, tautest, 10)
                df_cond_tau <- data.frame(method = name,
                                          type = "tau",
                                          df_cond_tau)
                res_cond <- rbind(res_cond, df_cond_tau)
            }
            if ("std" %in% strata){
                df_cond_std <- summary_CI_cond(Y1test, CQR_CI$Y, stdtest, 10)
                df_cond_std <- data.frame(method = name,
                                          type = "std",
                                          df_cond_std)
                res_cond <- rbind(res_cond, df_cond_std)
            }
        }
    }

    return(list(tau = res_tau, Y = res_Y, cond = res_cond))
}
