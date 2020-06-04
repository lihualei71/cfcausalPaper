library("tidyverse")

params <- read.table("NLSM_simul2_params.txt")

df <- list()
for (i in 1:nrow(params)){
    filename <- paste0("../raw_data_cluster/NLSM_simul_alpha", params[i, 3],
                       "_seed", params[i, 2],
                       "_ntr", 1000,
                       "_nte", 5000,
                       "_ps", 0,
                       "_sd", 0.5,
                       "_B", 50,
                       "_rho", params[i, 1],
                       ".RData")
    tmp <- try(load(filename))
    if (class(tmp) == "try-error"){
        next
    }
    df[[i]] <- data.frame(res,
                          rho = params[i, 1],
                          alpha = params[i, 3])
}
res <- do.call(rbind, df)

save(res, file = "../data/NLSM_simul_ITE.RData")
