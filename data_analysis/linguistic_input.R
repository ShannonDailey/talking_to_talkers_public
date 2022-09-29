
# source("data_prep/data_agg.R")
library(lme4)
library(lmerTest)
library(broom.mixed)
library(rstatix)

##### OVERALL INPUT BY GENDER #####

# Types
input_by_gender_types <- t.test(sqrt(mean_types) ~ gender,
                                data = input_means_by_subj %>%
                                  filter(types_input_outlier == FALSE),
                                conf.int = TRUE)

input_types_effsize <- effectsize::cohens_d(sqrt(mean_types) ~ gender,
                                            data = input_means_by_subj %>%
                                              filter(types_input_outlier == FALSE)) %>%
  as_tibble()

mean_types_F <- input_means_by_subj %>%
  filter(gender == "F") %>%
  filter(types_input_outlier == FALSE) %>% 
  ungroup() %>%
  summarise(mean = mean(mean_types, na.rm = TRUE),
            sd = sd(mean_types, na.rm = TRUE))

mean_types_M <- input_means_by_subj %>%
  filter(gender == "M") %>%
  filter(types_input_outlier == FALSE) %>% 
  ungroup() %>%
  summarise(mean = mean(mean_types, na.rm = TRUE),
            sd = sd(mean_types, na.rm = TRUE))

# Tokens
input_by_gender_tokens <- t.test(sqrt(mean_tokens) ~ gender,
                                      data = input_means_by_subj %>%
                                   filter(tokens_input_outlier == FALSE),
                                      conf.int = TRUE)

input_tokens_effsize <- effectsize::cohens_d(sqrt(mean_tokens) ~ gender,
                                            data = input_means_by_subj %>%
                                              filter(tokens_input_outlier == FALSE)) %>%
  as_tibble()

mean_tokens_F <- input_means_by_subj %>%
  filter(gender == "F") %>%
  filter(tokens_input_outlier == FALSE) %>% 
  ungroup() %>%
  summarise(mean = mean(mean_tokens, na.rm = TRUE),
            sd = sd(mean_tokens, na.rm = TRUE))

mean_tokens_M <- input_means_by_subj %>%
  filter(gender == "M") %>%
  filter(tokens_input_outlier == FALSE) %>%
  ungroup() %>%
  summarise(mean = mean(mean_tokens, na.rm = TRUE),
            sd = sd(mean_tokens, na.rm = TRUE))

#### EFFECT OF PARENT GENDER ####

# Types
mod_parentgend_types <- lm(sqrt(mean_types) ~ parent + gender,
                           data = input_by_subj_parent %>%
                             filter(types_outlier == FALSE))

mod_parent_types_summary <- apa_print(mod_parentgend_types)

mod_parentXgend_types <- lm(sqrt(mean_types) ~ parent * gender,
                            data = input_by_subj_parent %>%
                              filter(types_outlier == FALSE))

mod_parent_types_int_summary <- apa_print(mod_parentXgend_types)

aov_parentXgender_types <- anova(mod_parentgend_types,
                                 mod_parentXgend_types) %>% tidy()

# Tokens
mod_parentgend_tokens <- lm(sqrt(mean_tokens) ~ parent + gender,
                            data = input_by_subj_parent %>%
                              filter(tokens_outlier == FALSE))

mod_parent_tokens_summary <- apa_print(mod_parentgend_tokens)

mod_parentXgend_tokens <- lm(sqrt(mean_tokens) ~ parent * gender,
                             data = input_by_subj_parent %>%
                               filter(tokens_outlier == FALSE))

mod_parent_tokens_int_summary <- apa_print(mod_parentXgend_tokens)

aov_parentXgender_tokens <- anova(mod_parentgend_tokens,
                                  mod_parentXgend_tokens) %>%
  tidy()

##### UTTERANCE TYPES #####

propd_wilcox <- wilcox.test(mean_propd ~ gender, data = utt_types_by_subj, conf.int = TRUE)
propi_wilcox <- wilcox.test(mean_propi ~ gender, data = utt_types_by_subj, conf.int = TRUE)
propn_wilcox <- wilcox.test(mean_propn ~ gender, data = utt_types_by_subj, conf.int = TRUE)
propq_wilcox <- wilcox.test(mean_propq ~ gender, data = utt_types_by_subj, conf.int = TRUE)
propr_wilcox <- wilcox.test(mean_propr ~ gender, data = utt_types_by_subj, conf.int = TRUE)
props_wilcox <- wilcox.test(mean_props ~ gender, data = utt_types_by_subj, conf.int = TRUE)

#### ATTEMPTED REPLICATION OF PREVIOUS FINDINGS ####

# Input from mothers
input_by_gender_MOTtokens <- t.test(sqrt(mean_tokens) ~ gender,
                                    data = input_by_subj_parent %>%
                                      filter(parent == "MOT" & tokens_outlier == FALSE),
                                    conf.int = TRUE)

input_by_gender_MOTtypes <- t.test(sqrt(mean_types) ~ gender,
                                   data = input_by_subj_parent %>%
                                     filter(parent == "MOT" & types_outlier == FALSE),
                                   conf.int = TRUE)

# Questions and commands from mothers
propq_mom_wilcox <- wilcox.test(prop_q ~ gender, data = utt_types_mom_only, conf.int = TRUE)
propi_mom_wilcox <- wilcox.test(prop_i ~ gender, data = utt_types_mom_only, conf.int = TRUE)

utt_types_mom_summary <- utt_types_mom_only %>%
  group_by(gender) %>%
  summarise(mean_q = mean(prop_q),
            sd_q = sd(prop_q),
            mean_i = mean(prop_i),
            sd_i = sd(prop_i))

# Input from fathers
input_by_gender_FATtokens <- t.test(sqrt(mean_tokens) ~ gender,
                                    data = input_by_subj_parent %>%
                                      filter(parent == "FAT" & tokens_outlier == FALSE),
                                    conf.int = TRUE)

input_by_gender_FATtypes <- t.test(sqrt(mean_types) ~ gender,
                                   data = input_by_subj_parent %>%
                                     filter(parent == "FAT" & types_outlier == FALSE),
                                   conf.int = TRUE)

# Questions and commands from fathers
propq_dad_wilcox <- wilcox.test(prop_q ~ gender, data = utt_types_dad_only, conf.int = TRUE)
propi_dad_wilcox <- wilcox.test(prop_i ~ gender, data = utt_types_dad_only, conf.int = TRUE)

utt_types_dad_summary <- utt_types_dad_only %>%
  group_by(gender) %>%
  summarise(mean_q = mean(prop_q),
            mean_i = mean(prop_i))
