
# source("data_prep/data_agg.R")
library(lme4)
library(lmerTest)
library(broom.mixed)

##### TALK ONSET #####

# Age of first observed word (nouns)
talk_onset_F_obs <- talk_onset %>%
  filter(gender == "F") %>%
  summarise(mean_chi_onset = mean(obs_1stprodN_mo, na.rm = TRUE),
            sd_chi_onset = sd(obs_1stprodN_mo, na.rm = TRUE))

talk_onset_M_obs <- talk_onset %>%
  filter(gender == "M") %>%
  summarise(mean_chi_onset = mean(obs_1stprodN_mo, na.rm = TRUE),
            sd_chi_onset = sd(obs_1stprodN_mo, na.rm = TRUE))

talk_onset_obs <- t.test(obs_1stprodN_mo ~ gender,
                         data = talk_onset, conf.int = TRUE)

# Age of first observed word (not limiting to nouns)
talk_onset_effsize <- effectsize::cohens_d(obs_1stprodN_mo ~ gender,
                                          data = talk_onset) %>% as_tibble()

talk_onset_any_F_obs <- talk_onset %>%
  filter(gender == "F") %>%
  summarise(mean_chi_onset = mean(obs_1stprod_any_mo, na.rm = TRUE),
            sd_chi_onset = sd(obs_1stprod_any_mo, na.rm = TRUE))

talk_onset_any_M_obs <- talk_onset %>%
  filter(gender == "M") %>%
  summarise(mean_chi_onset = mean(obs_1stprod_any_mo, na.rm = TRUE),
            sd_chi_onset = sd(obs_1stprod_any_mo, na.rm = TRUE))

talk_onset_any_obs <- t.test(log10(obs_1stprod_any_mo) ~ gender,
                             data = talk_onset, conf.int = TRUE)

#### OVERALL CHILD PRODUCTIONS BY GENDER ####
chi_types_ttest <- t.test(chi_types_log ~ gender,
                           data = all_vocab_18mo,
                           paired = FALSE,
                           conf.int = TRUE)

chi_types_effsize <- effectsize::cohens_d(chi_types_log ~ gender,
                                          data = all_vocab_18mo) %>% as_tibble()

chi_tokens_ttest <- t.test(chi_tokens_log ~ gender,
                            data = all_vocab_18mo,
                            paired = FALSE,
                            conf.int = TRUE)

chi_tokens_effsize <- effectsize::cohens_d(chi_tokens_log ~ gender,
                                           data = all_vocab_18mo) %>% as_tibble()

chi_types_summary <- all_vocab_18mo %>%
  group_by(gender) %>%
  summarise(mean_types = mean(chi_types),
            sd_types = sd(chi_types))

chi_tokens_summary <- all_vocab_18mo %>%
  group_by(gender) %>%
  summarise(mean_tokens = mean(chi_tokens),
            sd_tokens = sd(chi_tokens))

#### CHILD PRODUCTIONS OVER TIME BY GENDER ####

# CHI types
mod_CHI_types_0 <- lmer(log_chi_types ~ month + (1|subj),
                        data = all_vocab_by_month %>% filter(month > 8))

mod_CHI_types_gender <- lmer(log_chi_types ~ month + gender + (1|subj),
                           data = all_vocab_by_month %>% filter(month > 8))

types_gender_summary <- mod_CHI_types_gender %>% tidy()

CHI_types_gender_anova <- anova(mod_CHI_types_0, mod_CHI_types_gender,
                                refit = FALSE) %>%
  broom.mixed::tidy()

mod_CHI_types_bymoRX <- lmer(log_chi_types ~ month * gender + (1|subj),
                             data = all_vocab_by_month %>%
                               filter(month > 8))

CHI_monthXgender_types <- anova(mod_CHI_types_gender,
                                mod_CHI_types_bymoRX,
                                refit = FALSE) %>% tidy()

# CHI tokens
mod_CHI_tokens_0 <- lmer(log_chi_tokens ~ month + (1|subj),
                     data = all_vocab_by_month %>% filter(month > 8))

mod_CHI_tokens_gender <- lmer(log_chi_tokens ~ month + gender + (1|subj),
                        data = all_vocab_by_month %>% filter(month > 8))

CHI_tokens_gender_anova <- anova(mod_CHI_tokens_0,
                                 mod_CHI_tokens_gender,
                             refit = FALSE) %>% tidy()

mod_tokens_bymoRX <- lmer(log_chi_tokens ~ month * gender + (1|subj),
                          data = all_vocab_by_month %>% filter(month > 8))

tokens_monthXgender_anova <- anova(mod_CHI_tokens_gender,
                                   mod_tokens_bymoRX,
                                   refit = FALSE) %>% tidy()
