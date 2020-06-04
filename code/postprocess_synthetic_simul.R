library("tidyverse")

params <- read.table("../jobs/simul_synthetic_params.txt")

res_tau <- list()
res_Y <- list()
res_cond <- list()
for (i in 1:nrow(params)){
    filename <- paste0("../raw_data_cluster/synthetic_simul",
                       "_n", params[i, 1],
                       "_d", params[i, 2],
                       "_seed", params[i, 3],
                       ".RData")
    load(filename)
    res_tau[[i]] <- lapply(1:length(res), function(k){
        df <- res[[k]]$tau
        df$exprid <- k
        df
    }) %>% do.call(rbind, .) %>%
        mutate(n = params[i, 1],
               d = params[i, 2])
    res_Y[[i]] <- lapply(1:length(res), function(k){
        df <- res[[k]]$Y
        df$exprid <- k
        df
    }) %>% do.call(rbind, .) %>%
        mutate(n = params[i, 1],
               d = params[i, 2])
    res_cond[[i]] <- lapply(1:length(res), function(k){
        df <- res[[k]]$cond
        df$exprid <- k
        df
    }) %>% do.call(rbind, .) %>%
        mutate(n = params[i, 1],
               d = params[i, 2])
}

res_tau <- do.call(rbind, res_tau)
res_Y <- do.call(rbind, res_Y)
res_cond <- do.call(rbind, res_cond)

res <- list(tau = res_tau, Y = res_Y, cond = res_cond)
save(res, file = "../data/simul_synthetic_results.RData")
