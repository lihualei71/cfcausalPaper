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

bartCf_CI_cond <- function(X, Y, Xtest,
                           ndpost = 100){
    ids <- !is.na(Y)
    X <- as.data.frame(X)
    Xtest <- as.data.frame(Xtest)
    fit <- bartMachine::bartMachine(X[ids, ], Y[ids],
                                    verbose = FALSE)
    CI <- bartMachine::calc_prediction_intervals(fit, new_data = Xtest, pi_conf = 0.95)$interval
    return(CI)
}

conformalCf_CI_cond <- function(X, Y, Xtest, 
                                type,
                                outfun = NULL,
                                psfun = NULL,
                                quantiles = NULL,
                                useCV = FALSE){
    if (is.null(outfun)){
        outfun <- switch(type,
                         CQR = cfcausal:::quantRF,
                         mean = cfcausal:::RF)
    }
    res <- cfcausal::conformalCf(X, Y, type = type, quantiles = quantiles, outfun = outfun, useCV = useCV)
    CI <- predict(res, Xtest, alpha = 0.05)
    return(CI)
}

summary_CI_cond <- function(CI, Y1, tau, std, types, name){
    res <- data.frame()
    if ("tau" %in% types){
        res_tau <- cond_coverage(CI, Y1, tau)
        res_tau <- data.frame(type = "tau", method = name,
                              res_tau)
        res <- rbind(res, res_tau)
    }
    if ("std" %in% types){
        res_std <- cond_coverage(CI, Y1, std)
        res_std <- data.frame(type = "std", method = name,
                              res_std)
        res <- rbind(res, res_std)
    }
    return(res)
}

Cf_expr_cond <- function(n, d, ntest,
                         Xfun, taufun, sdfun, psfun, errdist,
                         B = 50,
                         ndpost = 100,
                         types = c("tau", "std")){
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
    Y[T == 0] <- NA    

    res <- data.frame()

    bart_CI <- try(bartCf_CI_cond(X, Y, Xtest))
    if (class(bart_CI) != "try-error"){
        bart_res <- summary_CI_cond(bart_CI, Y1test, tautest, stdtest, types, "BART")
        res <- rbind(res, bart_res)
    }
    
    CQR_params <- expand.grid(
        method = c("RF", "Boosting", "BART"),
        q = c(0.025),
        useCV = c(FALSE))
    for (i in 1:nrow(CQR_params)){
        method <- as.character(CQR_params[i, 1])
        q <- as.numeric(CQR_params[i, 2])
        useCV <- as.logical(CQR_params[i, 3])
        outfun <- switch(method,
                         RF = cfcausal:::quantRF,
                         Boosting = cfcausal:::quantBoosting,
                         BART = cfcausal:::quantBART)
        if (!useCV){
            name <- paste0("CQR-", method, "-", q)
        } else {
            name <- paste0("CQR-CV-", method, "-", q)
        }
        CQR_CI <- try(conformalCf_CI_cond(
            X, Y, Xtest, 
            "CQR",
            outfun = outfun,
            psfun = psfun,
            quantiles = c(q, 1 - q),
            useCV = useCV))
        if (class(CQR_CI) != "try-error"){
            CQR_res <- summary_CI_cond(CQR_CI, Y1test, tautest, stdtest, types, name)
            res <- rbind(res, CQR_res)
        }
    }

    return(res)
}
