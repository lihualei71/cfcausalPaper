library("tidyverse")
library("ggplot2")

load("../data/NLSM_analysis.RData")

res <- res %>% filter(algo == "nest")

res <- res %>%
    group_by(algo, alpha) %>%
    summarize(pos_lo = quantile(pos, 0.05),
              pos_up = quantile(pos, 0.95),
              pos = median(pos),
              neg_lo = quantile(neg, 0.05),
              neg_up = quantile(neg, 0.95),
              neg = median(neg),
              len_lo = quantile(avglen, 0.05),
              len_up = quantile(avglen, 0.95),
              len = median(avglen))

res %>%
    ggplot(aes(x = alpha, y = len)) +
    geom_line() +
    geom_point() + 
    geom_ribbon(aes(ymin = len_lo, ymax = len_up), alpha = 0.25) +
    xlab("alpha") +
    ylab("Average length of intervals") +
    ggtitle("(a)") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5))
ggsave("../figs/NLSM_analysis_len_paper.pdf", last_plot(),
       width = 4, height = 3.5)

res %>%
    ggplot(aes(x = alpha, y = pos)) +
    geom_line() +
    geom_point() + 
    geom_ribbon(aes(ymin = pos_lo, ymax = pos_up), alpha = 0.25) +
    xlab("alpha") +
    ylab("Fraction of positives") +
    ggtitle("(b)") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5))
ggsave("../figs/NLSM_analysis_pos_paper.pdf", last_plot(),
       width = 4, height = 3.5)


res %>%
    ggplot(aes(x = alpha, y = neg)) +
    geom_line() +
    geom_point() + 
    geom_ribbon(aes(ymin = neg_lo, ymax = neg_up), alpha = 0.25) +
    xlab("alpha") +
    ylab("Fraction of negatives") +
    ggtitle("(c)") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5))
ggsave("../figs/NLSM_analysis_neg_paper.pdf", last_plot(),
       width = 4, height = 3.5)

