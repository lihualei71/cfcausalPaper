library("tidyverse")
library("ggplot2")

load("../data/simul1.RData")

method_levels <- c("CF", "xlearner", "BART", "CQR-RF-0.025", "CQR-Boosting-0.025", "CQR-BART-0.025")
method_labels <- c("Causal Forest", "X-learner", "BART", "CQR-RF", "CQR-Boosting", "CQR-BART")

res$tau %>% 
    filter(!grepl("oracle", method)) %>%
    filter(method %in% method_levels) %>%
    mutate(d = factor(d,
                      levels = c(10, 100),
                      labels = c("d = 10",
                                 "d = 100"))) %>%
    mutate(exprid = factor(exprid,
                           levels = 1:4,
                           labels = c("Homo. + Indep.",
                                      "Hetero. + Indep.",
                                      "Homo. + Corr.",
                                      "Hetero. + Corr."))) %>%
    mutate(method = factor(method,
                           levels = method_levels,
                           labels = method_labels)) %>%
    ggplot(aes(x = method, y = cr)) +
    geom_boxplot() +
    facet_grid(d ~ exprid) +
    geom_hline(yintercept = 0.95, color = "red") +
    ylim(c(0, 1)) +
    xlab("Method") + ylab("Empirical Coverage of CATE (alpha = 0.05)") + 
    coord_flip() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 15))

ggsave("../figs/simul1_coverage_tau_paper.pdf", last_plot(),
       width = 9, height = 6)

res$Y %>%
    filter(!grepl("oracle", method)) %>%
    filter(method %in% method_levels) %>%    
    mutate(d = factor(d,
                      levels = c(10, 100),
                      labels = c("d = 10",
                                 "d = 100"))) %>%
    mutate(exprid = factor(exprid,
                           levels = 1:4,
                           labels = c("Homo. + Indep.",
                                      "Hetero. + Indep.",
                                      "Homo. + Corr.",
                                      "Hetero. + Corr."))) %>%
    mutate(method = factor(method,
                           levels = method_levels,
                           labels = method_labels)) %>%
    ggplot(aes(x = method, y = cr)) +
    geom_boxplot() +
    facet_grid(d ~ exprid) +
    geom_hline(yintercept = 0.95, color = "red") +
    ylim(c(0, 1)) +
    xlab("Method") + ylab("Empirical Coverage of Y(1)-Y(0) (alpha = 0.05)") + 
    coord_flip() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 15))

ggsave("../figs/simul1_coverage_paper.pdf", last_plot(),
       width = 9, height = 6)

res$Y %>%
    filter(!grepl("oracle", method)) %>%
    filter(method %in% method_levels) %>%    
    mutate(d = factor(d,
                      levels = c(10, 100),
                      labels = c("d = 10",
                                 "d = 100"))) %>%
    mutate(exprid = factor(exprid,
                           levels = 1:4,
                           labels = c("Homo. + Indep.",
                                      "Hetero. + Indep.",
                                      "Homo. + Corr.",
                                      "Hetero. + Corr."))) %>%
    mutate(method = factor(method,
                           levels = method_levels,
                           labels = method_labels)) %>%
    ggplot(aes(x = method, y = cr)) +
    geom_boxplot() +
    facet_grid(d ~ exprid) +
    geom_hline(yintercept = 0.95, color = "red") +
    ylim(c(0.75, 1)) +
    xlab("Method") + ylab("Empirical Coverage of Y(1)-Y(0) (alpha = 0.05)") + 
    coord_flip() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 15))

ggsave("../figs/simul1_coverage_zoomin_paper.pdf", last_plot(),
       width = 9, height = 6)

res$Y %>%
    filter(!grepl("oracle", method)) %>%
    filter(method %in% method_levels) %>%    
    mutate(d = factor(d,
                      levels = c(10, 100),
                      labels = c("d = 10",
                                 "d = 100"))) %>%
    mutate(exprid = factor(exprid,
                           levels = 1:4,
                           labels = c("Homo. + Indep.",
                                      "Hetero. + Indep.",
                                      "Homo. + Corr.",
                                      "Hetero. + Corr."))) %>%
    mutate(method = factor(method,
                           levels = method_levels,
                           labels = method_labels)) %>%
    ggplot(aes(x = method, y = len)) +
    geom_boxplot() +
    facet_grid(d ~ exprid) +
    xlab("Method") + ylab("Average Length of Interval estimates of Y(1)-Y(0) (alpha = 0.05)") +
    expand_limits(y = 0) + 
    coord_flip() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 15))

ggsave("../figs/simul1_len_paper.pdf", last_plot(),
       width = 9, height = 6)
