# This file generates the figures found in "Talking to talkers."

# source("data_prep/data_agg.R")
library(ggpubr)
library(Hmisc)
library(viridis)

colorpalette <- c("#5BC3B0", "#ECA24E")
colorpalette_dark <- c("#449485", "#BA7C36")

#### FIGURE 2 ####
# Talk onset by gender
talk_onset_plot <- ggplot(data = talk_onset,
  aes(x = gender, y = obs_1stprodN_mo, color = gender)) +
  geom_jitter(size = 3, shape = 21, stroke = 1, width = 0.15) +
  scale_color_manual(values = colorpalette) +
  stat_summary(fun.data = mean_se,
               geom = "pointrange",
               color = "black", size = .6) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(8, 18), breaks = seq(8, 18, 2)) +
  scale_x_discrete(labels = c("M" = "Male", "F" = "Female")) +
  labs(x = "Child gender", y = "Age (months)", title = "Age of \n first word")

# Total types produced by gender
types_produced_plot <- ggplot(data = all_vocab_18mo,
  aes(x = gender, y = chi_types + 1, color = gender)) +
  geom_jitter(size = 3, shape = 21, stroke = 1,
              height = 0, width = .15) +
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
    y = "Noun types produced",
    title = "Noun types \n produced") +
  scale_y_continuous(breaks = c(1, 10, 100)) +
  coord_trans(y = "log10")

# Total tokens produced by gender
tokens_produced_plot <- ggplot(data = all_vocab_18mo,
  aes(x = gender, y = chi_tokens + 1, color = gender)) +
  scale_color_manual(values = colorpalette) +
  geom_jitter(size = 3, shape = 21, stroke = 1,
              height = 0, width = .15) +
  stat_summary(fun.data = mean_se,
               geom = "pointrange",
               color = "black", size = .6) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = c("M" = "Male",
                              "F" = "Female")) +
  labs(x = "Child gender",
       y = "Noun tokens produced",
    title = "Noun tokens \n produced") +
  scale_y_continuous(breaks = c(1, 10, 100, 1000)) +
  coord_trans(y = "log10")

figure2 <- ggarrange(talk_onset_plot,
                     types_produced_plot,
                     tokens_produced_plot,
                     nrow = 1)

#### FIGURE 3 ####
## CHI tokens age*gender interaction
chi_tokens_plot <- ggplot(data = all_vocab_by_month_long %>%
         filter(vocab_measure == "chi_tokens" & month > 8),
       aes(x = month, y = score + 1, color = gender)) +
  geom_jitter(shape = 1, alpha = .9, size = 2, width = .2, height = .05) +
  geom_smooth(se = FALSE) +
  stat_summary(aes(group = gender), fun.data = mean_se,
               geom = "pointrange",
               color = "black",
               position = position_dodge(width = .2)) +
  scale_y_continuous(limits = c(1, 300), breaks = c(1, 10, 100)) +
  coord_trans(y = "log10") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(8, 18), breaks = seq(8, 18, 2)) +
  scale_color_manual(name = "Child gender",
    values = colorpalette,
    breaks = c("F", "M"),
    labels = c("Female", "Male")) +
  labs(x = "Age (months)", y = "Noun tokens produced", title = NULL)

## CHI types age*gender interaction
chi_types_plot <- ggplot(data = all_vocab_by_month_long %>%
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
  labs(x = NULL, y = "Noun types produced", title = NULL)

figure3 <- ggarrange(chi_types_plot,
                     chi_tokens_plot,
                     ncol = 1,
                     common.legend = TRUE)

##### FIGURE 4 #####
# Mean input tokens by gender
tokens_by_gender_plot <- ggplot(data = input_means_by_subj,
                                aes(x = gender, y = mean_tokens,
                                    color = gender,
                                    shape = tokens_input_outlier)) +
  geom_violin(data = input_means_by_subj %>%
                filter(tokens_input_outlier == FALSE)) +
  geom_jitter(size = 2, stroke = 1,
              height = 0, width = .15, alpha = .7) +
  scale_shape_manual(values = c(0, 2)) +
  stat_summary(fun.data = mean_se,
               geom = "pointrange", shape = 15,
               color = "black", size = .6,
               data = input_means_by_subj %>%
                 filter(tokens_input_outlier == FALSE)) +
  scale_color_manual(values = colorpalette) +
  labs(x = "Child gender", y = "Mean noun tokens", title = "Noun tokens heard") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c(
    "F" = "Female",
    "M" = "Male"))

# Mean input types by gender
types_by_gender_plot <- ggplot(data = input_means_by_subj,
                               aes(x = gender, y = mean_types,
                                   color = gender,
                                   shape = types_input_outlier)) +
  geom_violin(data = input_means_by_subj %>%
                filter(types_input_outlier == FALSE)) +
  geom_jitter(size = 2, stroke = 1,
              height = 0, width = .15, alpha = .7) +
  scale_shape_manual(values = c(0, 2)) +
  stat_summary(fun.data = mean_se,
               geom = "pointrange", shape = 15,
               color = "black", size = .6,
               data = input_means_by_subj %>%
                 filter(types_input_outlier == FALSE)) +
  scale_color_manual(values = colorpalette) +
  labs(x = "Child gender", y = "Mean noun types", title = "Noun types heard") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c(
    "F" = "Female",
    "M" = "Male"))

figure4 <- ggarrange(types_by_gender_plot, tokens_by_gender_plot)

#### FIGURE 5 ####

tokens_by_parent_plot <-
  ggplot(data = input_by_subj_parent,
         aes(x = gender, y = mean_tokens, color = gender)) +
  geom_violin(data = input_by_subj_parent %>% filter(tokens_outlier == FALSE),
              alpha = .7) +
  geom_jitter(aes(shape = tokens_outlier),
              size = 2, stroke = 1,
              height = 0, width = .15, alpha = .7) +
  scale_shape_manual(values = c(0, 2)) +
  stat_summary(fun.data = mean_se,
               geom = "pointrange", shape = 15,
               color = "black", size = .6,
               data = input_by_subj_parent %>% filter(tokens_outlier == FALSE)) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 11)) +
  scale_color_manual(values = colorpalette,
                     breaks = c("F", "M")) +
  scale_x_discrete(
    breaks = c("F", "M"),
    labels = c("Female", "Male")) +
  facet_wrap(~parent,
             labeller = labeller(parent = c("MOT" = "Mother",
                                            "FAT" = "Father"))) +
  labs(x = "Child gender", y = "Mean noun tokens",
       title = "Mean noun tokens by parent") +
  scale_y_continuous(breaks = c(1, 100, 375, 750)) +
  coord_trans(y = "sqrt")

types_by_parent_plot <-
  ggplot(data = input_by_subj_parent,
         aes(x = gender, y = mean_types, color = gender)) +
  geom_violin(data = input_by_subj_parent %>% filter(types_outlier == FALSE),
              alpha = .7) +
  geom_jitter(aes(shape = types_outlier),
              size = 2, stroke = 1,
              height = 0, width = .15, alpha = .7) +
  scale_shape_manual(values = c(0, 2)) +
  stat_summary(fun.data = mean_se,
               geom = "pointrange", shape = 15,
               color = "black", size = .6,
               data = input_by_subj_parent %>% filter(types_outlier == FALSE)) +
  theme_bw(base_size = 12) +
  scale_color_manual(values = colorpalette,
                     breaks = c("F", "M")) +
  scale_x_discrete(
    breaks = c("F", "M"),
    labels = c("Female", "Male")) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 11)) +
  facet_wrap(~parent,
             labeller = labeller(parent = c("MOT" = "Mother",
                                            "FAT" = "Father"))) +
  labs(x = "Child gender", y = "Mean noun types",
       title = "Mean noun types by parent") +
  scale_y_continuous(breaks = c(1, 30, 100, 200)) +
  coord_trans(y = "sqrt")

figure_5 <- ggarrange(types_by_parent_plot,
                      tokens_by_parent_plot)

#### FIGURE 6 ####

utt_types_summary <- utt_types_long %>%
  group_by(gender, utt_type) %>%
  summarise(mean_prop = mean(prop)) %>%
  mutate(utt_type = factor(utt_type,
                           levels = c("d", "q", "n", "r", "s", "i")))

figure_6 <- ggplot(data = utt_types_summary,
                   aes(x = gender, y = mean_prop, fill = utt_type)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Child gender", y = "Proportion of input") +
  theme_bw(base_size = 12) +
  scale_x_discrete(
    breaks = c("F", "M"),
    labels = c("Female", "Male")) +
  scale_fill_viridis_d(
    name = "Utterance Type",
    breaks = c("d", "q", "n", "r", "s", "i"),
    labels = c("declarative", "question", "short-phrase",
               "reading", "singing", "imperative"))
# This is actually correct; they are really that close

#### FIGURE 7 ####
# Change in input types, pre- to post-talk
prepost_diffs_types <- ggplot(
  data = input_by_talk_status %>% filter(which_metric == "mean_types" & !is.na(posttalk_diff)),
  aes(y = reorder(subj, posttalk_diff), x = posttalk_diff,
      color = gender, shape = subj)) +
  scale_shape_manual(values = 0:41, guide = "none") +
  scale_color_manual(
    name = "Child gender",
    values = colorpalette,
    breaks = c("F", "M"),
    labels = c("Female", "Male")) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_segment(aes(xend = 0, yend = subj)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  labs(x = "Change in mean noun types \n before/after talk onset",
       y = "", title = "Noun types heard")

# Change in input tokens, pre- to post-talk
prepost_diffs_tokens <- ggplot(data = input_by_talk_status %>%
                                 filter(which_metric == "mean_tokens" & !is.na(posttalk_diff)),
                               aes(y = reorder(subj, posttalk_diff),
                                   x = posttalk_diff, color = gender,
                                   shape = subj)) +
  scale_shape_manual(values = 0:41, guide = "none") +
  scale_color_manual(
    name = "Child gender",
    values = colorpalette,
    breaks = c("F", "M"),
    labels = c("Female", "Male")) +
  geom_point() +
  geom_vline(xintercept = 0) +
  geom_segment(aes(xend = 0, yend = subj)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) +
  labs(x = "Change in mean noun tokens \n before/after talk onset",
       y = "", title = "Noun tokens heard")

figure7 <- ggarrange(prepost_diffs_types,
                     prepost_diffs_tokens,
                     common.legend = TRUE)

#### FIGURE 8 ####

utt_types_talk_summary <- utt_types_talk_long %>%
  group_by(utt_type, posttalk) %>%
  summarise(mean_prop = mean(prop)) %>%
  mutate(utt_type = factor(utt_type,
                           levels = c("d", "q", "n", "r", "s", "i")))

figure_8 <- ggplot(data = utt_types_talk_summary,
                   aes(x = posttalk, y = mean_prop, fill = utt_type)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Talk status", y = "Proportion of input") +
  theme_bw(base_size = 12) +
  scale_x_continuous(
    breaks = c(0, 1),
    labels = c("Before Talk Onset", "After Talk Onset")) +
  scale_fill_viridis_d(
    name = "Utterance Type",
    breaks = c("d", "q", "n", "r", "s", "i"),
    labels = c("declarative", "question", "short-phrase",
               "reading", "singing", "imperative"))

#### FIGURE 9 ####
threeway_types_plot <- ggplot(data = input_by_month %>%
                                filter(types_outlier == FALSE),
                              aes(x = month, y = numtypes,
                                  shape = factor(posttalk),
                                  linetype = factor(posttalk))) +
  geom_point(color = "gray", position = position_dodge(width = 0.55),
             alpha = 0.6) +
  stat_smooth(aes(fill = gender, alpha = factor(posttalk)),
              color = "black", method = "lm") +
  scale_alpha_manual(values = c("0" = 0.35,
                                "1" = 0.55),
                     name = "Talk status",
                     breaks = c(0, 1),
                     labels = c("Not yet talking", "Talking")) +
  scale_linetype_manual(
    name = "Talk status",
    values = c(1, 2),
    breaks = c(0, 1),
    labels = c("Not yet talking", "Talking")) +
  scale_fill_manual(
    name = "Child gender",
    values = colorpalette,
    breaks = c("F", "M"),
    labels = c("Female", "Male")) +
  scale_shape_manual(
    name = "Talk status",
    values = c(0, 2),
    breaks = c(0, 1),
    labels = c("Not yet talking", "Talking")) +
  theme_bw(base_size = 10) +
  scale_x_continuous(limits = c(5.5, 17.5), breaks = seq(6, 18, 2)) +
  labs(x = "Age (months)", y = "Noun types in input") +
  coord_trans(y = "sqrt") +
  facet_wrap(~gender,
             labeller = labeller(gender = c("M" = "Male",
                                            "F" = "Female")))

threeway_tokens_plot <- ggplot(data = input_by_month %>%
                                 filter(tokens_outlier == FALSE),
                               aes(x = month, y = numtokens,
                                   shape = factor(posttalk),
                                   linetype = factor(posttalk))) +
  geom_point(color = "gray", position = position_dodge(width = 0.55),
             alpha = 0.6) +
  stat_smooth(aes(fill = gender, alpha = factor(posttalk)),
              color = "black", method = "lm") +
  scale_alpha_manual(values = c("0" = 0.35,
                                "1" = 0.55),
                     name = "Talk status",
                     breaks = c(0, 1),
                     labels = c("Not yet talking", "Talking")) +
  scale_linetype_manual(
    name = "Talk status",
    values = c(1, 2),
    breaks = c(0, 1),
    labels = c("Not yet talking", "Talking")) +
  scale_fill_manual(
    name = "Child gender",
    values = colorpalette,
    breaks = c("F", "M"),
    labels = c("Female", "Male")) +
  scale_shape_manual(
    name = "Talk status",
    values = c(0, 2),
    breaks = c(0, 1),
    labels = c("Not yet talking", "Talking")) +
  theme_bw(base_size = 10) +
  scale_x_continuous(limits = c(5.5, 17.5), breaks = seq(6, 18, 2)) +
  labs(x = "Age (months)", y = "Noun tokens in input") +
  coord_trans(y = "sqrt") +
  facet_wrap(~gender,
             labeller = labeller(gender = c("M" = "Male",
                                            "F" = "Female")))

figure_9 <- ggarrange(threeway_types_plot,
                      threeway_tokens_plot,
                      common.legend = TRUE,
                      ncol = 1)
