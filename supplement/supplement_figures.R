# This script generates data visualizations found in the Supplementary
# Materials for "Talking to Talkers."

source("data_prep/data_agg.R")

library(tidyverse)
library(rstatix)

colorpalette <- c("#5BC3B0", "#ECA24E")

#### FIGURE S1 ####
# New CHI types per month
new_chi_types_plot <-  ggplot(data = all_vocab_by_month %>%
                                filter(month > 8),
                              aes(x = month, y = new_chi_types,
                                  color = gender)) +
  geom_jitter(shape = 1, alpha = .8, size = 2, width = .2, height = .05) +
  geom_smooth(se = FALSE) +
  stat_summary(aes(group = gender), fun.data = mean_se,
               geom = "pointrange",
               color = "black",
               position = position_dodge(width = .2)) +
  scale_y_continuous(limits = c(1, 100), breaks = c(1, 10, 100)) +
  coord_trans(y = "log10") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(8, 18), breaks = seq(8, 18, 2)) +
  scale_color_manual(name = "Child gender",
                     values = colorpalette,
                     breaks = c("F", "M"),
                     labels = c("Female", "Male")) +
  labs(x = NULL, y = "New noun types produced", title = NULL)

# This is a duplicate of the figure in the main text,
# with only the y-axis label changed
chi_types_plot2 <- ggplot(data = all_vocab_by_month_long %>%
                            filter(vocab_measure == "chi_types" & month > 8),
                          aes(x = month, y = score + 1, color = gender)) +
  geom_jitter(shape = 1, alpha = .8, size = 2, width = .2, height = .05) +
  geom_smooth(se = FALSE) +
  stat_summary(aes(group = gender), fun.data = mean_se,
               geom = "pointrange",
               color = "black",
               position = position_dodge(width = .2)) +
  scale_y_continuous(limits = c(1, 100), breaks = c(1, 10, 100)) +
  coord_trans(y = "log10") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(8, 18), breaks = seq(8, 18, 2)) +
  scale_color_manual(name = "Child gender",
                     values = colorpalette,
                     breaks = c("F", "M"),
                     labels = c("Female", "Male")) +
  labs(x = NULL, y = "Total cumulative noun types produced", title = NULL)

figure_s1 <- ggarrange(new_chi_types_plot,
                       chi_types_plot2,
                       ncol = 2,
                       common.legend = TRUE)

#### FIGURE S2 ####
## Input age*gender interaction plot
input_tokens_plot <- ggplot(data = input_by_month,
                            aes(x = month, y = numtokens, color = gender)) +
  geom_point(aes(shape = tokens_outlier),
             position = position_dodge(width = 0.4)) +
  scale_shape_manual(values = c(0, 4), guide = "none") +
  geom_smooth(data = input_by_month %>% filter(tokens_outlier == FALSE),
              se = FALSE) +
  stat_summary(aes(group = gender), fun.data = mean_se,
               geom = "pointrange",
               color = "black",
               position = position_dodge(width = 0.4)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(5.5, 17.5), breaks = seq(6, 18, 2)) +
  scale_color_manual(name = "Child gender",
                     values = colorpalette,
                     breaks = c("F", "M"),
                     labels = c("Female", "Male")) +
  labs(x = "Age (months)", y = "Noun tokens heard", title = NULL)

input_types_plot <- ggplot(data = input_by_month,
                           aes(x = month, y = numtypes, color = gender)) +
  geom_point(aes(shape = types_outlier),
             position = position_dodge(width = 0.4)) +
  scale_shape_manual(values = c(0, 4), guide = "none") +
  geom_smooth(data = input_by_month %>% filter(types_outlier == FALSE),
              se = FALSE) +
  stat_summary(aes(group = gender), fun.data = mean_se,
               geom = "pointrange",
               color = "black",
               position = position_dodge(width = .2)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(5.5, 17.5), breaks = seq(6, 18, 2)) +
  scale_color_manual(name = "Child gender",
                     values = colorpalette,
                     breaks = c("F", "M"),
                     labels = c("Female", "Male")) +
  labs(x = "Age (months)", y = "Noun types heard", title = NULL)

figure_s2 <- ggarrange(input_tokens_plot,
                       input_types_plot,
                       ncol = 1,
                       common.legend = TRUE)

#### FIGURE S3 ####

cdi_talk_onset_plot <- ggplot(data = talk_onset,
                              aes(x = gender, y = rep_1stprodN_mo,
                                  color = gender)) +
  geom_jitter(size = 3, shape = 21, stroke = 1, width = 0.1, height = 0.1) +
  scale_color_manual(values = colorpalette) +
  stat_summary(fun.data = mean_se, geom = "pointrange",
               size = 1, color = "black") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(8, 18), breaks = seq(8, 18, 2)) +
  scale_x_discrete(labels = c("M" = "Male", "F" = "Female")) +
  labs(x = "Child gender", y = "Age (months)",
       title = "Age of first word \n (reported)")

cdi_prod_plot <- ggplot(data = all_vocab_18mo,
                        aes(x = gender, y = CDIprod,
                            color = gender)) +
  scale_color_manual(values = colorpalette) +
  geom_jitter(size = 3, shape = 21, stroke = 1, height = 0, width = .1) +
  scale_y_log10() +
  stat_summary(fun.data = mean_se, geom = "pointrange",
               size = 1, color = "black") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("M" = "Male", "F" = "Female")) +
  labs(x = "Child gender",
       y = "Words produced",
       title = "CDI productive \n vocabulary")

figure_s3 <- ggarrange(cdi_talk_onset_plot,
                       cdi_prod_plot,
                       nrow = 1)

#### FIGURE S4 ####

lena_by_mo <- read_csv("data/lena_counts_ransubj.csv") %>%
  mutate(subj = as.factor(subj)) %>%
  left_join(gender)

lena_by_subj <- lena_by_mo %>%
  group_by(subj, gender) %>%
  summarise(mean_awc = mean(awc_perhr),
            mean_ctc = mean(ctc_perhr),
            mean_cvc = mean(cvc_perhr)) %>%
  ungroup() %>%
  mutate(awc_outlier = outlier_calc(mean_awc,
                                    group_mean = mean(mean_awc, na.rm = TRUE),
                                    group_sd = sd(mean_awc, na.rm = TRUE)),
         ctc_outlier = outlier_calc(mean_ctc,
                                  group_mean = mean(mean_ctc),
                                  group_sd = sd(mean_ctc)),
         cvc_outlier = outlier_calc(mean_cvc,
                                  group_mean = mean(mean_cvc),
                                  group_sd = sd(mean_cvc)))

lena_by_subj_long <- lena_by_subj %>%
  pivot_longer(cols = c(mean_awc, mean_ctc, mean_cvc),
               names_to = "which_metric",
               values_to = "count_perhr")

figure_s3 <- ggplot(data = lena_by_subj_long,
       aes(x = gender, y = count_perhr, color = gender)) +
  geom_jitter(size = 3, shape = 21, stroke = 1,
              height = 0, width = .15) +
  facet_wrap(~which_metric,
             scales = "free",
             labeller = labeller(which_metric = c("mean_awc" = "Adult Words",
                                                  "mean_ctc" = "Conversational Turns",
                                                  "mean_cvc" = "Child Vocalizations"))) +
  scale_color_manual(values = colorpalette) +
  stat_summary(fun.data = mean_se,
               geom = "pointrange",
               color = "black", size = .6) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = c("M" = "Male",
                              "F" = "Female")) +
  labs(x = "Child gender",
       y = "Count per hour",
       title = "LENA metrics by gender")
