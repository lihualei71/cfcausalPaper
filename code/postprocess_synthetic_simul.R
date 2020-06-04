library("tidyverse")

params <- read.table("cfcausal_simul1_params.txt")

res_tau <- list()
res_Y <- list()
for (i in 1:nrow(params)){
    filename <- paste0("../raw_data_cluster/simul1_n", params[i, 1], "_d", params[i, 2], "_seed", params[i, 3], ".RData")
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
}
res_tau <- do.call(rbind, res_tau)
res_Y <- do.call(rbind, res_Y)

res <- list(tau = res_tau, Y = res_Y)
save(res, file = "../data/simul1.RData")
