library("tidyverse")
library("ggplot2")

load("../data/simul_cond.RData")

method_levels <- c("BART", "CQR-RF-0.025", "CQR-Boosting-0.025", "CQR-BART-0.025")
method_labels <- c("BART", "CQR-RF", "CQR-Boosting", "CQR-BART")

res %>%
    filter(type == "tau") %>%
    filter(method %in% method_levels) %>%
    filter(d == 10) %>%
    mutate(exprid = factor(exprid,
                           levels = 1:4,
                           labels = c("Homo. + Indep.",
                                      "Hetero. + Indep.",
                                      "Homo. + Corr.",
                                      "Hetero. + Corr."))) %>%
    mutate(method = factor(method,
                           levels = method_levels,
                           labels = method_labels)) %>%
    ggplot(aes(x = strata, y = cr)) +
    geom_boxplot() +
    facet_grid(method ~ exprid) +
    geom_hline(yintercept = 0.95, color = "red") +
    ylim(c(0, 1)) +
    xlab("Strata of CATE") +
    ylab("Empirical Conditional Coverage of ITE (alpha = 0.05)") + 
    coord_flip() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 15))

ggsave("../figs/simul_cond_d10_coverage_tau.pdf", last_plot(),
       width = 9, height = 12)

res %>%
    filter(type == "tau") %>%    
    filter(method %in% method_levels) %>%    
    filter(d == 100) %>%
    mutate(exprid = factor(exprid,
                           levels = 1:4,
                           labels = c("Homo. + Indep.",
                                      "Hetero. + Indep.",
                                      "Homo. + Corr.",
                                      "Hetero. + Corr."))) %>%
    mutate(method = factor(method,
                           levels = method_levels,
                           labels = method_labels)) %>%
    ggplot(aes(x = strata, y = cr)) +
    geom_boxplot() +
    facet_grid(method ~ exprid) +
    geom_hline(yintercept = 0.95, color = "red") +
    ylim(c(0, 1)) +
    xlab("Strata of CATE") +
    ylab("Empirical Conditional Coverage of ITE (alpha = 0.05)") + 
    coord_flip() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 15))

ggsave("../figs/simul_cond_d100_coverage_tau.pdf", last_plot(),
       width = 9, height = 12)

res %>%
    filter(type == "std") %>%
    filter(method %in% method_levels) %>%
    filter(d == 10) %>%
    mutate(exprid = factor(exprid,
                           levels = 1:4,
                           labels = c("Homo. + Indep.",
                                      "Hetero. + Indep.",
                                      "Homo. + Corr.",
                                      "Hetero. + Corr."))) %>%
    mutate(method = factor(method,
                           levels = method_levels,
                           labels = method_labels)) %>%
    ggplot(aes(x = strata, y = cr)) +
    geom_boxplot() +
    facet_grid(method ~ exprid) +
    geom_hline(yintercept = 0.95, color = "red") +
    ylim(c(0, 1)) +
    xlab("Strata of conditional variance") +
    ylab("Empirical Conditional Coverage of ITE (alpha = 0.05)") + 
    coord_flip() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 15))

ggsave("../figs/simul_cond_d10_coverage_std.pdf", last_plot(),
       width = 6, height = 12)

res %>%
    filter(type == "std") %>%    
    filter(method %in% method_levels) %>%    
    filter(d == 100) %>%
    mutate(exprid = factor(exprid,
                           levels = 1:4,
                           labels = c("Homo. + Indep.",
                                      "Hetero. + Indep.",
                                      "Homo. + Corr.",
                                      "Hetero. + Corr."))) %>%
    mutate(method = factor(method,
                           levels = method_levels,
                           labels = method_labels)) %>%
    ggplot(aes(x = strata, y = cr)) +
    geom_boxplot() +
    facet_grid(method ~ exprid) +
    geom_hline(yintercept = 0.95, color = "red") +
    ylim(c(0, 1)) +
    xlab("Strata of conditional variance") +    
    ylab("Empirical Conditional Coverage of ITE (alpha = 0.05)") + 
    coord_flip() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 15))

ggsave("../figs/simul_cond_d100_coverage_std.pdf", last_plot(),
       width = 6, height = 12)
