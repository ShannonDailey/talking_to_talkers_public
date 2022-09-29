
# source("data_prep/data_agg.R")
library(tidyverse)
library(rstatix)

#### SUPPLEMENTAL ANALYSIS OF INFANTS’ EARLY PRODUCTIONS ####

mod_new_CHI_types_0 <- lmer(new_chi_types ~ month + (1|subj),
                            data = all_vocab_by_month %>% filter(month > 8))

mod_new_CHI_types_gender <- lmer(new_chi_types ~ month + gender + (1|subj),
                                 data = all_vocab_by_month %>% filter(month > 8))

new_types_gender_summary <- mod_new_CHI_types_gender %>% tidy()

new_CHI_types_gender_anova <- anova(mod_new_CHI_types_0, mod_new_CHI_types_gender,
                                    refit = FALSE) %>% tidy()

mod_new_CHI_types_bymoRX <- lmer(new_chi_types ~ month * gender + (1|subj),
                                 data = all_vocab_by_month %>% filter(month > 8))

new_CHI_monthXgender_types <- anova(mod_new_CHI_types_gender,
                                    mod_new_CHI_types_bymoRX,
                                    refit = FALSE) %>% tidy()

#### ANALYSES OF LENA AUTOMATED SPEECH ESTIMATES ####

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

awc_ttest <- t.test(data = lena_by_subj %>% filter(awc_outlier == FALSE),
                    mean_awc ~ gender,
                    conf.int = TRUE)

awc_gender <- lena_by_subj %>%
  filter(awc_outlier == FALSE) %>%
  group_by(gender) %>%
  summarise(sd = sd(mean_awc),
            mean = mean(mean_awc)) %>%
  pivot_wider(names_from = gender,
              values_from = c(mean, sd))

cvc_gender <- lena_by_subj %>%
  filter(cvc_outlier == FALSE) %>%
  group_by(gender) %>%
  summarise(sd = sd(mean_cvc),
            mean = mean(mean_cvc)) %>%
  pivot_wider(names_from = gender,
              values_from = c(mean, sd))

ctc_gender <- lena_by_subj %>%
  filter(ctc_outlier == FALSE) %>%
  group_by(gender) %>%
  summarise(sd = sd(mean_ctc),
            mean = mean(mean_ctc)) %>%
  pivot_wider(names_from = gender,
              values_from = c(mean, sd))

ctc_ttest <- t.test(data = lena_by_subj %>%
                      filter(ctc_outlier == FALSE),
                    mean_ctc ~ gender,
                    conf.int = TRUE)

cvc_ttest <- t.test(data = lena_by_subj %>%
                      filter(cvc_outlier == FALSE),
                    mean_cvc ~ gender,
                    conf.int = TRUE)

#### ANALYSES OF PARENT-REPORTED PRODUCTIVE VOCABULARY (MCDI) ####

# Reported talk onset by gender
talk_onset_F_rep <- talk_onset %>%
  filter(gender == "F") %>%
  summarise(mean_chi_onset = mean(rep_1stprodN_mo, na.rm = TRUE),
            sd_chi_onset = sd(rep_1stprodN_mo, na.rm = TRUE))

talk_onset_M_rep <- talk_onset %>%
  filter(gender == "M") %>%
  summarise(mean_chi_onset = mean(rep_1stprodN_mo, na.rm = TRUE),
            sd_chi_onset = sd(rep_1stprodN_mo, na.rm = TRUE))

talk_onset_rep <- t.test(rep_1stprodN_mo ~ gender,
                         data = talk_onset, conf.int = TRUE)
talk_onset_any_rep <- t.test(rep_1stprod_any_mo ~ gender,
                             data = talk_onset, conf.int = TRUE)

# Reported vocabulary by gender
cdi_summary <- all_vocab_18mo %>%
  group_by(gender) %>%
  summarise(mean_CDIprod = mean(CDIprod),
            sd_CDIprod = sd(CDIprod),
            median_CDIprod = median(CDIprod))

cdi_ttest <- t.test(log10(CDIprod) ~ gender,
                    data = all_vocab_18mo, conf.int = TRUE)

## Power analysis
coin::wilcox_test(CDIprod ~ gender, data = all_vocab_18mo, conf.int = TRUE)

# effect size = Z/sqrt(N)
1.6337/sqrt(44)

pwr::pwr.chisq.test(w = 0.246, N = NULL, df = 1, sig.level = .05, power = 0.8)
