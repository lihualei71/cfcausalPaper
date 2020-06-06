library("tidyverse")
library("ggplot2")

load("../data/simul_NLSM_results.RData")

method_levels <- c("CF", "xlearner", "bart_naive", "bart_inexact", "CQR_naive", "CQR_exact", "CQR_inexact")
method_labels <- c("Causal Forest", "X-learner", "BART (Naive)", "BART (Inexact)", "CQR (Naive)", "CQR (Exact)", "CQR (Inexact)")

## Coverage of ITE
res$marginal %>% 
    mutate(method = factor(method,
                           levels = method_levels,
                           labels = method_labels)) %>%
    ggplot(aes(x = method, y = cr)) +
    geom_boxplot() +
    geom_hline(yintercept = 0.95, color = "red") +
    ylim(c(0, 1)) +
    xlab("Method") +
    ylab(paste0("Empirical Coverage of ITE (alpha = 0.05)")) + 
    coord_flip() +
    theme_bw() +
    theme(panel.grid = element_blank())

ggsave(paste0("../figs/simul_NLSM_coverage_paper.pdf"), last_plot(), width = 5, height = 4)

## Average length of ITE
res$marginal %>%
    mutate(method = factor(method,
                           levels = method_levels,
                           labels = method_labels)) %>%
    ggplot(aes(x = method, y = len)) +
    geom_boxplot() +
    xlab("Method") +
    ylab(paste0("Length of CI (alpha = 0.05)")) + 
    coord_flip() +
    theme_bw() +
    theme(panel.grid = element_blank())

ggsave(paste0("../figs/simul_NLSM_len_paper.pdf"), last_plot(), width = 5, height = 4)

## Conditional coverage of ITE with CATE being stratified
method_levels <- c("bart_naive", "bart_inexact", "CQR_naive", "CQR_exact", "CQR_inexact")
method_labels <- c("BART (Naive)", "BART (Inexact)", "CQR (Naive)", "CQR (Exact)", "CQR (Inexact)")

levels(res$cond$strata) <- seq(0.05, 0.95, 0.1)

res$cond %>% 
    filter(method %in% method_levels) %>%
    mutate(method = factor(method,
                           levels = method_levels,
                           labels = method_labels)) %>%
    mutate(type = factor(type, levels = c("tau", "std"),
                         labels = c("CATE", "Var(ITE | X)"))) %>%
    group_by(type, method, strata) %>%
    summarize(cr_med = median(cr),
              cr_up = quantile(cr, 0.95),
              cr_lo = quantile(cr, 0.05)) %>%
    ungroup() %>%
    mutate(strata = as.numeric(as.character(strata))) %>%
    ggplot(aes(x = strata)) +    
    geom_line(aes(y = cr_med), color = "blue") +
    geom_ribbon(aes(ymin = cr_lo, ymax = cr_up), alpha = 0.25, fill = "blue") +
    facet_grid(type ~ method) +            
    geom_hline(yintercept = 0.95, color = "red") +
    scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) +    
    ylim(c(0.75, 1)) +
    xlab("Percentile of the stratifying variable") +
    ylab(paste0("Conditional Coverage of ITE (alpha = 0.05)")) + 
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 15))

ggsave(paste0("../figs/simul_NLSM_cond_paper.pdf"), last_plot(), width = 9, height = 3.5)
