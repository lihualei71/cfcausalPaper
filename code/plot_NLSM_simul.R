library("tidyverse")
library("ggplot2")

load("../data/NLSM_simul_ITE.RData")

res <- res %>% filter(rho == 0, alpha == 0.05)

for (rh in unique(res$rho)){
    for (al in unique(res$alpha)){
        method_levels <- c("CF", "xlearner", "bart", "bart_inexact", paste0("CQR-BART-", al / 2, "_exact"), paste0("CQR-BART-", al / 2, "_inexact"), paste0("CQR-BART-", al / 2, "_naive"))
        method_labels <- c("Causal Forest", "X-learner", "BART (Naive)", "BART (Inexact)", "CQR (Exact)", "CQR (Inexact)", "CQR (Naive)")
        res %>% filter(rho == rh) %>%
            filter(alpha == al) %>%
            filter(method %in% method_levels) %>%
            mutate(method = factor(method,
                                   levels = method_levels,
                                   labels = method_labels)) %>%
            ggplot(aes(x = method, y = cr)) +
            geom_boxplot() +
            geom_hline(yintercept = 1 - al, color = "red") +
            ylim(c(0, 1)) +
            xlab("Method") +
            ylab(paste0("Empirical Coverage of ITE (alpha = ", al, ")")) + 
            coord_flip() +
            theme_bw() +
            theme(panel.grid = element_blank())
        ggsave(paste0("../figs/NLSM_simul_ITE_coverage_rho", rh, "_alpha", al, "_paper.pdf"), last_plot(), width = 5, height = 4)

        res %>% filter(rho == rh) %>%
            filter(alpha == al) %>%            
            filter(method %in% method_levels) %>%        
            mutate(method = factor(method,
                                   levels = method_levels,
                                   labels = method_labels)) %>%
            ggplot(aes(x = method, y = len)) +
            geom_boxplot() +
            xlab("Method") +
            ylab(paste0("Length of CI (alpha = ", al, ")")) + 
            coord_flip() +
            theme_bw() +
            theme(panel.grid = element_blank())
        ggsave(paste0("../figs/NLSM_simul_ITE_len_rho", rh, "_alpha", al, "_paper.pdf"), last_plot(), width = 5, height = 4)
    }
}
