library("tidyverse")

params <- read.table("cfcausal_simul_cond_params.txt")

res_cr <- list()
for (i in 1:nrow(params)){
    filename <- paste0("../raw_data_cluster/simul_cond_n", params[i, 1], "_d", params[i, 2], "_seed", params[i, 3], ".RData")
    tmp <- try(load(filename))
    if (class(tmp) != "try-error"){
        res_cr[[i]] <- lapply(1:length(res), function(k){
            df <- res[[k]]
            df$exprid <- k
            df
        }) %>% do.call(rbind, .) %>%
            mutate(n = params[i, 1],
                   d = params[i, 2])
    }
}
res <- do.call(rbind, res_cr)
save(res, file = "../data/simul_cond.RData")
