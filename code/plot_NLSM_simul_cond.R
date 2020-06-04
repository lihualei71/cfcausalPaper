library("tidyverse")
library("ggplot2")

load("../data/NLSM_simul_ITE_cond.RData")

method_levels <- c("BART_inexact", "CQR-RF-0.025_inexact", "CQR-Boosting-0.025_inexact", "CQR-BART-0.025_inexact")
method_labels <- c("BART (inexact)", "CQR-RF (inexact)", "CQR-Boosting (inexact)", "CQR-BART (inexact)")
    
for (rh in unique(res$rho)){
    for (al in unique(res$alpha)){
        res %>% filter(rho == rh) %>%
            filter(alpha == al) %>%
            filter(type == "CATE") %>%
            filter(method %in% method_levels) %>%
            mutate(method = factor(method,
                                   levels = method_levels,
                                   labels = method_labels)) %>%
            ggplot(aes(x = strata, y = cr)) +
            geom_boxplot() +
            facet_grid( ~ method) +            
            geom_hline(yintercept = 1 - al, color = "red") +
            ylim(c(0.8, 1)) +
            xlab("Strata of CATE") +
            ylab(paste0("Empirical Conditional Coverage of ITE (alpha = ", al, ")")) + 
            coord_flip() +
            theme_bw() +
            theme(panel.grid = element_blank())
        ggsave(paste0("../figs/NLSM_simul_ITE_cond_coverage_rho", rh, "_alpha", al, "_tau.pdf"), last_plot(), width = 9, height = 4)

        res %>% filter(rho == rh) %>%
            filter(alpha == al) %>%
            filter(type == "std") %>%            
            filter(method %in% method_levels) %>%
            mutate(method = factor(method,
                                   levels = method_levels,
                                   labels = method_labels)) %>%
            ggplot(aes(x = strata, y = cr)) +
            geom_boxplot() +
            facet_grid( ~ method) +            
            geom_hline(yintercept = 1 - al, color = "red") +
            ylim(c(0.8, 1)) +
            xlab("Strata of conditional variance") +
            ylab(paste0("Empirical Conditional Coverage of ITE (alpha = ", al, ")")) + 
            coord_flip() +
            theme_bw() +
            theme(panel.grid = element_blank())
        ggsave(paste0("../figs/NLSM_simul_ITE_cond_coverage_rho", rh, "_alpha", al, "_std.pdf"), last_plot(), width = 9, height = 4)        
    }
}
