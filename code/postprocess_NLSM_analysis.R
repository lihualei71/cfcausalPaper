library("tidyverse")

params <- read.table("NLSM_analysis_params.txt")

df <- list()
for (i in 1:nrow(params)){
    filename <- paste0("../raw_data_cluster/NLSM_analysis_seed", params[i, 1],
                       ".RData")
    tmp <- try(load(filename))
    if (class(tmp) == "try-error"){
        next
    }
    df[[i]] <- data.frame(res)
}
res <- do.call(rbind, df)

save(res, file = "../data/NLSM_analysis.RData")
