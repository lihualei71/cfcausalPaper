library("tidyverse")
library("ggplot2")

load("../data/simul_synthetic_results.RData")

method_levels <- c("CF", "xlearner", "bart", "CQR-quantRF", "CQR-quantBoosting", "CQR-quantBART")
method_labels <- c("Causal Forest", "X-learner", "BART", "CQR-RF", "CQR-Boosting", "CQR-BART")

## Coverage of CATE
res$tau %>% 
    mutate(d = factor(d,
                      levels = c(10, 100),
                      labels = c("d = 10",
                                 "d = 100"))) %>%
    mutate(exprid = factor(exprid,
                           levels = 1:4,
                           labels = c("Homosc. + Ind.",
                                      "Heterosc. + Ind.",
                                      "Homosc. + Corr.",
                                      "Heterosc. + Corr."))) %>%
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

ggsave("../figs/simul_synthetic_coverage_tau_paper.pdf", last_plot(),
       width = 9, height = 6)

## Coverage of ITE
res$Y %>%
    mutate(d = factor(d,
                      levels = c(10, 100),
                      labels = c("d = 10",
                                 "d = 100"))) %>%
    mutate(exprid = factor(exprid,
                           levels = 1:4,
                           labels = c("Homosc. + Ind.",
                                      "Heterosc. + Ind.",
                                      "Homosc. + Corr.",
                                      "Heterosc. + Corr."))) %>%
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

ggsave("../figs/simul_synthetic_coverage_paper.pdf", last_plot(),
       width = 9, height = 6)

## Average length of ITE intervals
res$Y %>%
    mutate(d = factor(d,
                      levels = c(10, 100),
                      labels = c("d = 10",
                                 "d = 100"))) %>%
    mutate(exprid = factor(exprid,
                           levels = 1:4,
                           labels = c("Homosc. + Ind.",
                                      "Heterosc. + Ind.",
                                      "Homosc. + Corr.",
                                      "Heterosc. + Corr."))) %>%
    mutate(method = factor(method,
                           levels = method_levels,
                           labels = method_labels)) %>%
    ggplot(aes(x = method, y = len)) +
    geom_boxplot() +
    facet_grid(d ~ exprid) +
    geom_hline(yintercept = 3.92, color = "blue") +    
    xlab("Method") + ylab("Average Length of Interval estimates of Y(1)-Y(0) (alpha = 0.05)") +
    expand_limits(y = 0) + 
    coord_flip() +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 15))

ggsave("../figs/simul_synthetic_len_paper.pdf", last_plot(),
       width = 9, height = 6)

## Conditional Coverage of ITE with CATE being stratified and d = 10
levels(res$cond$strata) <- seq(0.05, 0.95, 0.1)

res$cond %>%
    filter(type == "tau") %>%
    filter(d == 10) %>%
    mutate(exprid = factor(exprid,
                           levels = 1:4,
                           labels = c("Homosc. + Ind.",
                                      "Heterosc. + Ind.",
                                      "Homosc. + Corr.",
                                      "Heterosc. + Corr."))) %>%
    mutate(method = factor(method,
                           levels = method_levels,
                           labels = method_labels)) %>%
    group_by(exprid, method, strata) %>%
    summarize(cr_med = median(cr),
              cr_up = quantile(cr, 0.95),
              cr_lo = quantile(cr, 0.05)) %>%
    ungroup() %>%
    mutate(strata = as.numeric(as.character(strata))) %>%
    ggplot(aes(x = strata)) +
    geom_line(aes(y = cr_med), color = "blue") +
    geom_ribbon(aes(ymin = cr_lo, ymax = cr_up), alpha = 0.25, fill = "blue") +
    facet_grid(exprid ~ method) +
    geom_hline(yintercept = 0.95, color = "red") +
    scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) +
    ylim(c(0, 1)) +
    xlab("Percentile of CATE") +
    ylab("Conditional Coverage of ITE (alpha = 0.05)") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 12.5))
    
ggsave("../figs/simul_synthetic_cond_d10_tau_paper.pdf", last_plot(),
       width = 11, height = 6.5)

## Conditional Coverage of ITE with CATE being stratified and d = 100
res$cond %>%
    filter(type == "tau") %>%
    filter(d == 100) %>%
    mutate(exprid = factor(exprid,
                           levels = 1:4,
                           labels = c("Homosc. + Ind.",
                                      "Heterosc. + Ind.",
                                      "Homosc. + Corr.",
                                      "Heterosc. + Corr."))) %>%
    mutate(method = factor(method,
                           levels = method_levels,
                           labels = method_labels)) %>%
    group_by(exprid, method, strata) %>%
    summarize(cr_med = median(cr),
              cr_up = quantile(cr, 0.95),
              cr_lo = quantile(cr, 0.05)) %>%
    ungroup() %>%
    mutate(strata = as.numeric(as.character(strata))) %>%
    ggplot(aes(x = strata)) +
    geom_line(aes(y = cr_med), color = "blue") +
    geom_ribbon(aes(ymin = cr_lo, ymax = cr_up), alpha = 0.25, fill = "blue") +
    facet_grid(exprid ~ method) +
    geom_hline(yintercept = 0.95, color = "red") +
    scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) +
    ylim(c(0, 1)) +
    xlab("Percentile of CATE") +
    ylab("Conditional Coverage of ITE (alpha = 0.05)") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 12.5))
    
ggsave("../figs/simul_synthetic_cond_d100_tau_paper.pdf", last_plot(),
       width = 11, height = 6.5)

## Conditional Coverage of ITE with sigma(x) being stratified and d = 10
res$cond %>%
    filter(type == "std") %>%
    filter(d == 10) %>%
    mutate(exprid = factor(exprid,
                           levels = 1:4,
                           labels = c("Homosc. + Ind.",
                                      "Heterosc. + Ind.",
                                      "Homosc. + Corr.",
                                      "Heterosc. + Corr."))) %>%
    mutate(method = factor(method,
                           levels = method_levels,
                           labels = method_labels)) %>%
    group_by(exprid, method, strata) %>%
    summarize(cr_med = median(cr),
              cr_up = quantile(cr, 0.95),
              cr_lo = quantile(cr, 0.05)) %>%
    ungroup() %>%
    mutate(strata = as.numeric(as.character(strata))) %>%
    ggplot(aes(x = strata)) +
    geom_line(aes(y = cr_med), color = "blue") +
    geom_ribbon(aes(ymin = cr_lo, ymax = cr_up), alpha = 0.25, fill = "blue") +
    facet_grid(exprid ~ method) +
    geom_hline(yintercept = 0.95, color = "red") +
    scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) +
    ylim(c(0, 1)) +
    xlab("Percentile of conditional variance") +
    ylab("Conditional Coverage of ITE (alpha = 0.05)") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 12.5))
    
ggsave("../figs/simul_synthetic_cond_d10_std_paper.pdf", last_plot(),
       width = 11, height = 4)

## Conditional Coverage of ITE with sigma(x) being stratified and d = 100
res$cond %>%
    filter(type == "std") %>%
    filter(d == 100) %>%
    mutate(exprid = factor(exprid,
                           levels = 1:4,
                           labels = c("Homosc. + Ind.",
                                      "Heterosc. + Ind.",
                                      "Homosc. + Corr.",
                                      "Heterosc. + Corr."))) %>%
    mutate(method = factor(method,
                           levels = method_levels,
                           labels = method_labels)) %>%
    group_by(exprid, method, strata) %>%
    summarize(cr_med = median(cr),
              cr_up = quantile(cr, 0.95),
              cr_lo = quantile(cr, 0.05)) %>%
    ungroup() %>%
    mutate(strata = as.numeric(as.character(strata))) %>%
    ggplot(aes(x = strata)) +
    geom_line(aes(y = cr_med), color = "blue") +
    geom_ribbon(aes(ymin = cr_lo, ymax = cr_up), alpha = 0.25, fill = "blue") +
    facet_grid(exprid ~ method) +
    geom_hline(yintercept = 0.95, color = "red") +
    scale_x_continuous(breaks = seq(0.1, 0.9, 0.2)) +
    ylim(c(0, 1)) +
    xlab("Percentile of conditional variance") +
    ylab("Conditional Coverage of ITE (alpha = 0.05)") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          strip.text = element_text(size = 12.5))
    
ggsave("../figs/simul_synthetic_cond_d100_std_paper.pdf", last_plot(),
       width = 11, height = 4)
