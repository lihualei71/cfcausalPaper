library("tidyverse")

params <- read.table("../jobs/analysis_NLSM_params.txt")

df <- list()
for (i in 1:nrow(params)){
    filename <- paste0("../raw_data_cluster/analysis_NLSM",
                       "_seed", params[i, 1],
                       ".RData")
    tmp <- try(load(filename))
    if (class(tmp) == "try-error"){
        next
    }
    df[[i]] <- data.frame(res)
}
res <- do.call(rbind, df)

save(res, file = "../data/analysis_NLSM_results.RData")
