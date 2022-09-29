# # This script was used to prep public-facing data files for all analyses for "Talking to Talkers."
# # It cannot be run without lab-internal files, but is provided for transparency.
# # Shannon Dailey, shannon.dailey@duke.edu
# 
# library(tidyverse)
# library(feather)
# library(blabr)
# 
# setwd("~/Desktop/github/shannondailey/talking_to_talkers/")
# 
# #### RANDOM SUBJECT NUMBERS ####
# 
# # This is the dictionary for our deidentified subject numbers
# # to use for our public-facing code and data files.
# random_subjnums <- read_csv("notshared/random_subnums.csv",
#                             col_types = cols(number = col_factor(),
#                                              subj = col_factor()))
# 
# #### GENDER ####
# gender <- read_csv("notshared/seedlings_gender.csv",
#                    col_types = cols(gender = col_factor(),
#                                     subj = col_factor()))
# 
# gender_ransubj <- gender %>%
#   left_join(random_subjnums) %>%
#   select(-subj) %>%
#   rename(subj = number)
# 
# #### BASIC LEVEL ####
# # blabr:::get_latest_tag('all_basiclevel')
# 
# all_bl_global <- blabr:::make_new_global_basic_level(all_basiclevel_version = "0.2.0") %>%
#   left_join(gender) %>%
#   left_join(random_subjnums) %>%
#   select(-subj, -SubjectNumber, -id) %>%
#   rename(subj = number)
# 
# write_csv(all_bl_global, "data/all_basiclevel_ransubj.csv")
# 
# ##### TALK ONSET #####
# # Using talk onset data from Point, Walk, Talk
# # https://github.com/CharlotteMoore927/PointWalkTalkPublic
# 
# talk_onset_pwt <- read_csv("notshared/subjlevel_pwt_obsrep_mo_ransubj.csv",
#                            col_types = cols(subj = col_factor())) %>%
#   dplyr::select(subj, obs_1stprodN_mo, obs_1stprod_any_mo,
#                 rep_1stprodN_mo, rep_1stprod_any_mo) %>%
#   left_join(gender_ransubj)
# 
# write_csv(talk_onset_pwt, "data/talk_onset_ransubj.csv")
# 
# ##### CDI #####
# cdi_prep <- get_cdi_spreadsheet(version = "0.0.8") %>%
#   filter(SeedlingsFinalSample=="Y") %>%
#   mutate(subj = fct_recode(subj,
#                            "01" = "1",
#                            "02" = "2",
#                            "03" = "3",
#                            "04" = "4",
#                            "06" = "6",
#                            "07" = "7",
#                            "08" = "8",
#                            "09" = "9")) %>%
#   left_join(random_subjnums) %>%
#   select(-subj, -SubjectNumber, -Date_Completed,
#          -(Child_gender:Talk_labeling), -SeedlingsFinalSample,
#          -(Gestures_showobject:Gestures_wearglasses)) %>%
#   rename(subj = number)
# 
# # Aggregating nouns only for vocab correlation
# noun_totals <- cdi_prep %>%
#   filter(month == 18) %>%
#   dplyr::select(subj, month, Talk_animal:Talk_uncle) %>%
#   gather(key = word, value = nounsum, Talk_animal:Talk_uncle) %>%
#   mutate(nounsum = as.factor(nounsum)) %>%
#   group_by(subj, month, nounsum) %>%
#   tally() %>%
#   filter(nounsum == "Understands and Says") %>%
#   rename("CDInounprod" = "n") %>%
#   dplyr::select(subj, month, CDInounprod)
# 
# cdi <- cdi_prep %>%
#   left_join(noun_totals) %>%
#   dplyr::select(subj, month, CDIcomp, CDIprod, CDInounprod)
# 
# write_csv(cdi, "data/cdi_ransubj.csv")
# 
# #### TOP HOURS MARKER ####
# 
# top3hours <- read_csv("notshared/all_cha_top5_ms.csv") %>%
#   dplyr::select(file, region_num, onset, offset) %>%
#   filter(region_num < 4) %>%
#   separate(file, c("subj", "month")) %>%
#   mutate(subj = as_factor(subj),
#          month = as_factor(month)) %>%
#   left_join(random_subjnums) %>%
#   select(-subj) %>%
#   rename(subj = number)
# 
# write_csv(top3hours, "data/top3hours_ransubj.csv")
# 
# #### LENA ####
# 
# lena_data <- read_csv("notshared/LENA_data_01-10-22.csv") %>%
#   left_join(random_subjnums) %>%
#   select(-subj, -subj_month, -duration, -duration_hrs) %>%
#   rename(subj = number)
# 
# write_csv(lena_data, "data/lena_counts_ransubj.csv")
