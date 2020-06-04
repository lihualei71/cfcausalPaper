library("tidyverse")

params <- read.table("../jobs/simul_NLSM_params.txt")

res_marginal <- list()
res_cond <- list()
for (i in 1:nrow(params)){
    filename <- paste0("../raw_data_cluster/NLSM_simul",
                       "_alpha", params[i, 2],
                       "_seed", params[i, 1],
                       "_ntr", 1000,
                       "_nte", 5000,
                       "_B", 50,
                       ".RData")
    tmp <- try(load(filename))
    if (class(tmp) == "try-error"){
        next
    }
    res_marginal[[i]] <- data.frame(res$marginal, alpha = params[i, 2])
    res_cond[[i]] <- data.frame(res$cond, alpha = params[i, 2])
}
res_marginal <- do.call(rbind, res_marginal)
res_cond <- do.call(rbind, res_cond)

res <- list(marginal = res_marginal, cond = res_cond)
save(res, file = "../data/simul_NLSM_results.RData")
