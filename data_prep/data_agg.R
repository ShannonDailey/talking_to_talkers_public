# This script imports and aggregates various data frames for
# visualizations and analyses for "Talking to Talkers."
# Shannon Dailey, shannon.dailey@duke.edu

#### IMPORT DATA AND FUNCTIONS ####

all_bl_global <- read_csv("data/all_basiclevel_ransubj.csv",
                            col_types = cols(subj = col_factor()))

talk_onset <- read_csv("data/talk_onset_ransubj.csv",
                       col_types = cols(subj = col_factor(),
                                        gender = col_factor()))

gender <- talk_onset %>%
  select(subj, gender)

cdi_by_month <- read_csv("data/cdi_ransubj.csv",
                         col_types = cols(subj = col_factor()))

# This is an alternate form of blabr::big_aggregate() that collapses across aud/vid
source("data_prep/functions/big_agg_month.R")

# This function pulls the top 3 hours from all_basiclevel
source("data_prep/functions/top3_function.R")

outlier_calc <- function(value, group_mean, group_sd, sd_threshold = 3) {
  upper_bound <- group_mean + (sd_threshold * group_sd)
  lower_bound <- group_mean - (sd_threshold * group_sd)
  ifelse(value >= lower_bound & value <= upper_bound, FALSE, TRUE)
}

##### CHILD PRODUCTIONS (BASIC LEVEL) #####

# Using full recordings for overall child production counts
# Using top 3 hours for all input analyses and monthly production analyses
# (for consistency in rec length over time)

# Need all rows to include files that CHI doesn't say anything
blank <- all_bl_global %>%
  dplyr::select(subj, month) %>%
  distinct()

# Using global for CHI types
chi_types <- all_bl_global %>%
  filter(speaker == "CHI") %>%
  group_by(subj, month) %>%
  summarise(chi_types = n_distinct(global_bl))

chi_tokens <- all_bl_global %>%
  filter(speaker == "CHI") %>%
  group_by(subj, month, speaker) %>%
  tally() %>%
  spread(speaker, n) %>%
  rename(chi_tokens = "CHI")

# This finds only the *new* types each child says each month
# for supplemental analysis
new_chi_types <- all_bl_global %>%
  filter(speaker == "CHI") %>%
  arrange(subj, month, global_bl) %>%
  group_by(subj) %>%
  distinct(global_bl, .keep_all = TRUE) %>%
  group_by(subj, month) %>%
  summarise(new_chi_types = n()) 

chi_prods_by_month <- blank %>%
  left_join(chi_tokens) %>%
  left_join(chi_types) %>%
  left_join(new_chi_types) %>%
  mutate(chi_tokens = replace_na(chi_tokens, 0),
         chi_types = replace_na(chi_types, 0),
         new_chi_types = replace_na(new_chi_types, 0),
         month = as.integer(as.character(month)))

chi_tokens_total <- chi_prods_by_month %>%
  group_by(subj) %>%
  summarise(chi_tokens_total = sum(chi_tokens))

chi_types_total <- all_bl_global %>%
  filter(speaker == "CHI") %>%
  group_by(subj) %>%
  summarise(chi_types_total = n_distinct(global_bl))

chi_prods_by_subj <- chi_tokens_total %>%
  left_join(chi_types_total) %>%
  mutate(chi_types_total = replace_na(chi_types_total, 0))

##### CDI #####
# For total/noun vocabulary correlation
total_noun_cdi <- cdi_by_month %>%
  filter(month == 18)

#### ALL VOCABULARY ####
# vocab per kid, per month, per source (observed and cdi)
all_vocab_by_month <- chi_prods_by_month %>%
  full_join(cdi_by_month) %>%
  mutate(log_chi_tokens = log10(chi_tokens + 1),
         log_chi_types = log10(chi_types + 1),
         log_cdi_prod = log10(CDIprod + 1),
         log_cdi_comp = log10(CDIcomp + 1)) %>%
  left_join(gender)
# this includes 18mo CDI
# only 564 rows instead of 572 (44*13) because of 8 missing 18mo CDIs

all_vocab_by_month_long <- all_vocab_by_month %>%
  pivot_longer(cols = c(chi_tokens, chi_types,
                        CDIcomp, CDIprod,
                        log_chi_tokens, log_chi_types,
                        log_cdi_prod, log_cdi_comp),
               names_to = "vocab_measure",
               values_to = "score")

cdi_by_subj <- cdi_by_month %>%
  group_by(subj) %>%
  # using the last CDI we have, since
  # we are missing several 18mo CDIs
  filter(month == max(month)) %>%
  dplyr::select(subj, month, CDIprod, CDIcomp)

all_vocab_18mo <- cdi_by_subj %>%
  left_join(chi_prods_by_subj) %>%
  dplyr::select(-month) %>%
  rename("chi_tokens" = "chi_tokens_total",
         "chi_types" = "chi_types_total") %>%
  left_join(gender) %>%
  ungroup() %>%
  group_by(gender) %>%
  mutate(chi_tokens_log = log10(chi_tokens + 1),
         chi_types_log = log10(chi_types + 1),
         tokens_prod_outlier = outlier_calc(chi_tokens_log,
                                       group_mean = mean(chi_tokens_log),
                                       group_sd = sd(chi_tokens_log)),
         types_prod_outlier = outlier_calc(chi_types_log,
                                      group_mean = mean(chi_types_log),
                                      group_sd = sd(chi_types_log)))

#### INPUT DATA ####

# This is top 3 hours:
all_bl <- all_bl_global %>%
  # removing NAs
  filter(!is.na(basic_level)) %>%
  top3hours_allbl()

# This aggregates to the subj-month level
all_bl_mo_nochi <- big_agg_month(all_bl,
                                 exclude_chi = TRUE,
                                 exclude = c("CHI", "CHItypes"))

input_by_month <- all_bl_mo_nochi %>%
  ungroup() %>%
  left_join(gender) %>%
  mutate(tokens_outlier = outlier_calc(numtokens,
                                       group_mean = mean(numtokens, na.rm = TRUE),
                                       group_sd = sd(numtokens, na.rm = TRUE)),
         types_outlier = outlier_calc(numtypes,
                                      group_mean = mean(numtypes, na.rm = TRUE),
                                      group_sd = sd(numtypes, na.rm = TRUE)),
         month = as.numeric(as.character(month)),
         posttalk = as.numeric(posttalk),
         onset_diff_month = month - noun_chi_onset) %>%
  left_join(chi_prods_by_month)

input_means_by_subj <- input_by_month %>%
  group_by(subj, gender) %>%
  summarise(mean_tokens = mean(numtokens),
            mean_types = mean(numtypes)) %>%
  group_by(gender) %>%
  mutate(sqrt_tokens = sqrt(mean_tokens),
         sqrt_types = sqrt(mean_types),
         tokens_input_outlier = outlier_calc(sqrt_tokens,
                                             group_mean = mean(sqrt_tokens,
                                                               na.rm = TRUE),
                                             group_sd = sd(sqrt_tokens,
                                                           na.rm = TRUE)),
         types_input_outlier = outlier_calc(sqrt_types,
                                            group_mean = mean(sqrt_types,
                                                              na.rm = TRUE),
                                            group_sd = sd(sqrt_types,
                                                          na.rm = TRUE))) %>%
  left_join(all_vocab_18mo)

#### INPUT BY TALK STATUS ####

input_by_talk_status_long <- input_by_month %>%
  # 3 subjs never said a word in our recordings by 17mo
  # so they are excluded in talk status analyses
  filter(!(subj %in% c("255", "172", "950"))) %>%
  group_by(subj, posttalk, gender) %>%
  summarise(mean_tokens = mean(numtokens),
            mean_types = mean(numtypes)) %>%
  ungroup() %>%
  group_by(posttalk) %>%
  mutate(tokens_input_outlier = outlier_calc(mean_tokens,
                                             group_mean = mean(mean_tokens,
                                                               na.rm = TRUE),
                                             group_sd = sd(mean_tokens,
                                                           na.rm = TRUE)),
         types_input_outlier = outlier_calc(mean_types,
                                            group_mean = mean(mean_types,
                                                              na.rm = TRUE),
                                            group_sd = sd(mean_types,
                                                          na.rm = TRUE))) %>%
  ungroup()

input_by_talk_status <- input_by_talk_status_long %>%
  pivot_longer(cols = c(mean_tokens, mean_types),
               names_to = "which_metric",
               values_to = "count") %>%
  pivot_wider(names_from = posttalk,
              values_from = count) %>%
  dplyr::rename(posttalk = "1",
                pretalk = "0") %>%
  mutate(posttalk_diff = posttalk - pretalk)

#### INPUT BY PARENT ####
tokens_by_parent <- input_by_month %>%
  # excluding 1 subj due to same-sex parents
  filter(subj != "861") %>%
  group_by(subj, gender) %>%
  summarise(FAT = mean(FAT, na.rm = TRUE),
            MOT = mean(MOT, na.rm = TRUE)) %>%
  pivot_longer(cols = c(FAT, MOT),
               names_to = "parent",
               values_to = "mean_tokens") %>%
  filter(!(subj == "861" & parent == "FAT"))

types_by_parent <- input_by_month %>%
  filter(subj != "861") %>%
  group_by(subj, gender) %>%
  summarise(FAT = mean(FATtypes, na.rm = TRUE),
            MOT = mean(MOTtypes, na.rm = TRUE)) %>%
  pivot_longer(cols = c(FAT, MOT),
               names_to = "parent",
               values_to = "mean_types") %>%
  filter(!(subj == "861" & parent == "FAT"))

parent_input1 <- tokens_by_parent %>%
  left_join(types_by_parent)

mom_w_outliers <- parent_input1 %>%
  filter(parent == "MOT") %>%
  ungroup() %>%
  mutate(sqrt_tokens = sqrt(mean_tokens),
         sqrt_types = sqrt(mean_types),
         tokens_outlier = outlier_calc(sqrt_tokens,
                                       group_mean = mean(sqrt_tokens, na.rm = TRUE),
                                       group_sd = sd(sqrt_tokens, na.rm = TRUE)),
         types_outlier = outlier_calc(sqrt_types,
                                      group_mean = mean(sqrt_types, na.rm = TRUE),
                                      group_sd = sd(sqrt_types, na.rm = TRUE)))

dad_w_outliers <- parent_input1 %>%
  filter(parent == "FAT") %>%
  ungroup() %>%
  mutate(sqrt_tokens = sqrt(mean_tokens),
         sqrt_types = sqrt(mean_types),
         tokens_outlier = outlier_calc(sqrt_tokens,
                                       group_mean = mean(sqrt_tokens, na.rm = TRUE),
                                       group_sd = sd(sqrt_tokens, na.rm = TRUE)),
         types_outlier = outlier_calc(sqrt_types,
                                      group_mean = mean(sqrt_types, na.rm = TRUE),
                                      group_sd = sd(sqrt_types, na.rm = TRUE)))

input_by_subj_parent <- mom_w_outliers %>%
  rbind(dad_w_outliers)
# 86 rows because excluding subj 861


#### UTTERANCE TYPES ####
utt_types_by_subj <- input_by_month %>%
  mutate(propd = d/numtokens,
         propi = i/numtokens,
         propn = n/numtokens,
         propq = q/numtokens,
         propr = r/numtokens,
         props = s/numtokens) %>%
  group_by(subj, gender) %>%
  summarise(mean_propd = mean(propd),
            mean_propi = mean(propi),
            mean_propn = mean(propn),
            mean_propq = mean(propq),
            mean_propr = mean(propr),
            mean_props = mean(props))

utt_types_by_subj_talk <- input_by_month %>%
  mutate(propd = d/numtokens,
         propi = i/numtokens,
         propn = n/numtokens,
         propq = q/numtokens,
         propr = r/numtokens,
         props = s/numtokens) %>%
  group_by(subj, gender, posttalk) %>%
  summarise(mean_propd = mean(propd),
            mean_propi = mean(propi),
            mean_propn = mean(propn),
            mean_propq = mean(propq),
            mean_propr = mean(propr),
            mean_props = mean(props))

utt_types_talk_long <- utt_types_by_subj_talk %>%
  pivot_longer(cols = c(mean_propd,
                        mean_propi,
                        mean_propn,
                        mean_propq,
                        mean_propr,
                        mean_props),
               names_to = "utt_type",
               names_prefix = "mean_prop",
               values_to = "prop")

utt_types_long <- utt_types_by_subj %>%
  pivot_longer(cols = c(mean_propd,
                        mean_propi,
                        mean_propn,
                        mean_propq,
                        mean_propr,
                        mean_props),
               names_to = "utt_type",
               names_prefix = "mean_prop",
               values_to = "prop")

utt_types_mom_only <- all_bl %>%
  filter(speaker == "MOT") %>%
  dplyr::filter(utterance_type %in%
                  c("d", "i", "q", "r", "s", "n")) %>%
  dplyr::group_by(subj, month, utterance_type) %>%
  dplyr::tally() %>%
  tidyr::spread(utterance_type, n) %>%
  mutate(d = replace_na(d, 0),
         i = replace_na(i, 0),
         q = replace_na(q, 0),
         r = replace_na(r, 0),
         s = replace_na(s, 0),
         n = replace_na(n, 0),
         total = d + i + q + r + s + n) %>%
  dplyr::group_by(subj) %>%
  summarise(mean_d = mean(d),
         mean_i = mean(i),
         mean_q = mean(q),
         mean_r = mean(r),
         mean_s = mean(s),
         mean_n = mean(n),
         mean_total = mean(total)) %>%
  mutate(prop_d = mean_d / mean_total,
         prop_i = mean_i / mean_total,
         prop_q = mean_q / mean_total,
         prop_r = mean_r / mean_total,
         prop_s = mean_s / mean_total,
         prop_n = mean_n / mean_total) %>%
  left_join(gender)

utt_types_dad_only <- all_bl %>%
  filter(speaker == "FAT") %>%
  dplyr::filter(utterance_type %in%
                  c("d", "i", "q", "r", "s", "n")) %>%
  dplyr::group_by(subj, month, utterance_type) %>%
  dplyr::tally() %>%
  tidyr::spread(utterance_type, n) %>%
  mutate(d = replace_na(d, 0),
         i = replace_na(i, 0),
         q = replace_na(q, 0),
         r = replace_na(r, 0),
         s = replace_na(s, 0),
         n = replace_na(n, 0),
         total = d + i + q + r + s + n) %>%
  dplyr::group_by(subj) %>%
  summarise(mean_d = mean(d),
            mean_i = mean(i),
            mean_q = mean(q),
            mean_r = mean(r),
            mean_s = mean(s),
            mean_n = mean(n),
            mean_total = mean(total)) %>%
  mutate(prop_d = mean_d / mean_total,
         prop_i = mean_i / mean_total,
         prop_q = mean_q / mean_total,
         prop_r = mean_r / mean_total,
         prop_s = mean_s / mean_total,
         prop_n = mean_n / mean_total) %>%
  left_join(gender)
