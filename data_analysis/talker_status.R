# This script runs the statistical analyses reported in the sections
# "Effect of Talker Status" and "Interaction of Age, Talk Status, and Gender"
# of "Talking to talkers."

# source("data_prep/data_agg.R")
library(lme4)
library(lmerTest)
library(broom.mixed)
library(rstatix)

#### OVERALL INPUT ####

# Types
paired_means_types_test <- t.test(data = input_by_talk_status_long,
                                  mean_types ~ posttalk,
                                  paired = TRUE,
                                  conf.int = TRUE)

talk_types_effsize <- effectsize::cohens_d(data = input_by_talk_status_long %>%
                                               mutate(posttalk = as_factor(posttalk),
                                                      posttalk = relevel(posttalk, ref = "1")),
                                             mean_types ~ posttalk,
                                             paired = TRUE) %>%
  as_tibble()

# Tokens
paired_means_tokens_test <- t.test(data = input_by_talk_status_long,
                                   mean_tokens ~ posttalk,
                                   paired = TRUE,
                                   conf.int = TRUE)

talk_tokens_effsize <- effectsize::cohens_d(data = input_by_talk_status_long %>%
                                               mutate(posttalk = as_factor(posttalk),
                                                      posttalk = relevel(posttalk, ref = "1")),
                                             mean_tokens ~ posttalk,
                                             paired = TRUE) %>%
  as_tibble()

summary_talkstatus <- input_by_talk_status %>%
  group_by(which_metric) %>%
  summarise(pretalk_mean = mean(pretalk),
            posttalk_mean = mean(posttalk, na.rm = TRUE))

# How many go up?
# Types
posttalk_increase_types <- input_by_talk_status %>%
  filter(which_metric=="mean_types") %>%
  mutate(pos_incr = posttalk - pretalk > 0) %>%
  ungroup() %>%
  summarise(count_pos_incr = sum(pos_incr, na.rm = TRUE))

N_posttalk_increase_types <- posttalk_increase_types$count_pos_incr[1]
binom_test_types <- binom.test(N_posttalk_increase_types, 41)

# Tokens
posttalk_increase_tokens <- input_by_talk_status %>%
  filter(which_metric=="mean_tokens") %>%
  mutate(pos_incr = posttalk - pretalk > 0) %>%
  ungroup() %>%
  summarise(count_pos_incr = sum(pos_incr, na.rm = TRUE))

N_posttalk_increase_tokens <- posttalk_increase_tokens$count_pos_incr[1]
binom_test_tokens <- binom.test(N_posttalk_increase_tokens, 41)

#### UTTERANCE TYPES ####
propd_talk_wilcox <- wilcox.test(mean_propd ~ posttalk, data = utt_types_by_subj_talk, conf.int = TRUE)
propi_talk_wilcox <- wilcox.test(mean_propi ~ posttalk, data = utt_types_by_subj_talk, conf.int = TRUE)
propn_talk_wilcox <- wilcox.test(mean_propn ~ posttalk, data = utt_types_by_subj_talk, conf.int = TRUE)
propq_talk_wilcox <- wilcox.test(mean_propq ~ posttalk, data = utt_types_by_subj_talk, conf.int = TRUE)
propr_talk_wilcox <- wilcox.test(mean_propr ~ posttalk, data = utt_types_by_subj_talk, conf.int = TRUE)
props_talk_wilcox <- wilcox.test(mean_props ~ posttalk, data = utt_types_by_subj_talk, conf.int = TRUE)

propn_prepost <- utt_types_by_subj_talk %>%
  dplyr::select(subj, posttalk, mean_propn) %>%
  group_by(posttalk) %>%
  summarise(mean = mean(mean_propn, na.rm = TRUE),
            sd = sd(mean_propn, na.rm = TRUE))

propn_prepost_diff <- utt_types_by_subj_talk %>%
  dplyr::select(subj, posttalk, mean_propn) %>%
  pivot_wider(names_from = posttalk, values_from = mean_propn) %>%
  rename("pre" = "0",
         "post" = "1") %>%
  mutate(diff = post - pre) %>%
  ungroup() %>%
  summarise(mean_diff = mean(diff, na.rm = TRUE),
            sd_diff = sd(diff, na.rm = TRUE))

utt_types_by_subj_talk %>%
  group_by(posttalk) %>%
  summarise(mean_n = mean(mean_propn))

# Gender*talk status interactions
propd_mod <- lm(data = utt_types_by_subj_talk, mean_propd ~ gender + posttalk)
propd_mod_rx <- lm(data = utt_types_by_subj_talk, mean_propd ~ gender * posttalk)
propd_int_aov <- anova(propd_mod, propd_mod_rx) %>% tidy()

propi_mod <- lm(data = utt_types_by_subj_talk, mean_propi ~ gender + posttalk)
propi_mod_rx <- lm(data = utt_types_by_subj_talk, mean_propi ~ gender * posttalk)
propi_int_aov <- anova(propi_mod, propi_mod_rx) %>% tidy()

propn_mod <- lm(data = utt_types_by_subj_talk, mean_propn ~ gender + posttalk)
propn_mod_rx <- lm(data = utt_types_by_subj_talk, mean_propn ~ gender * posttalk)
propn_int_aov <- anova(propn_mod, propn_mod_rx) %>% tidy()

propq_mod <- lm(data = utt_types_by_subj_talk, mean_propq ~ gender + posttalk)
propq_mod_rx <- lm(data = utt_types_by_subj_talk, mean_propq ~ gender * posttalk)
propq_int_aov <- anova(propq_mod, propq_mod_rx) %>% tidy()

propr_mod <- lm(data = utt_types_by_subj_talk, mean_propr ~ gender + posttalk)
propr_mod_rx <- lm(data = utt_types_by_subj_talk, mean_propr ~ gender * posttalk)
propr_int_aov <- anova(propr_mod, propr_mod_rx) %>% tidy()

props_mod <- lm(data = utt_types_by_subj_talk, mean_props ~ gender + posttalk)
props_mod_rx <- lm(data = utt_types_by_subj_talk, mean_props ~ gender * posttalk)
props_int_aov <- anova(props_mod, props_mod_rx) %>% tidy()

#### AGE, TALK STATUS, AND GENDER INTERACTIONS ####

# Types
mod_all2ways_types <- lmer(data = input_by_month %>%
                             filter(types_outlier == FALSE),
                           sqrt(numtypes) ~ month * posttalk + gender * posttalk + month * gender + (1|subj))

mod_3way_types <- lmer(data = input_by_month %>%
                         filter(types_outlier == FALSE),
                       sqrt(numtypes) ~ month * posttalk * gender + (1|subj))

aov_3way_types <- anova(mod_all2ways_types, mod_3way_types,
                        refit = FALSE, ddf = "lme4") %>%
  tidy()

# Tokens
mod_all2ways_tokens <- lmer(data = input_by_month %>%
                              filter(tokens_outlier == FALSE),
                            sqrt(numtokens) ~ month * posttalk + gender * posttalk + month * gender + (1|subj))

mod_3way_tokens <- lmer(data = input_by_month %>%
                          filter(tokens_outlier == FALSE),
                        sqrt(numtokens) ~ month * posttalk * gender + (1|subj))

aov_3way_tokens <- anova(mod_all2ways_tokens, mod_3way_tokens,
                         refit = FALSE, ddf = "lme4") %>%
  tidy()
