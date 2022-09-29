
# source("data_prep/data_agg.R")

# Total annotations counts
n_chis <- all_bl_global %>%
  filter(speaker == "CHI") %>%
  nrow()

n_total_nonchis <- all_bl %>%
  filter(speaker != "CHI") %>%
  nrow()

total_parent_tokens <- input_by_month %>%
  ungroup() %>%
  # One participant is excluded from all mother-father analyses
  # due to same-sex parents
  filter(subj != "861") %>%
  summarise(fat_tokens = sum(FAT),
            mot_tokens = sum(MOT))

total_parent_types <- all_bl %>%
  filter(speaker %in% c("MOT", "FAT")) %>%
  ungroup() %>%
  group_by(speaker) %>%
  summarise(types = n_distinct(basic_level))

# Proportion of input from parents
total_parent_count <- all_bl %>%
  filter(speaker %in% c("MOT", "FAT", "MT2")) %>%
  tally()

total_mom_count <- all_bl %>%
  filter(speaker %in% c("MOT", "MT2")) %>%
  tally()

total_dad_count <- all_bl %>%
  filter(speaker %in% c("FAT")) %>%
  tally()

total_count <- all_bl %>%
  tally()

total_parent_prop <- total_parent_count/total_count
total_mom_prop <- total_mom_count/total_count
total_dad_prop <- total_dad_count/total_count

# Noun to total vocab correlations
rep_obs_vocab_corr <- cor.test(all_vocab_18mo$CDIprod,
                               all_vocab_18mo$chi_types)

noun_total_vocab_corr <- cor.test(total_noun_cdi$CDIprod,
                                  total_noun_cdi$CDInounprod)

# Number of speakers per file

all_bl_global_agg <- all_bl_global %>%
  filter(subj != "861") %>%
  mutate(SubjectNumber = paste0(subj, "_", month)) %>%
  big_aggregate()

all_bl_global_agg %>%
  filter(audio_video == "audio") %>%
  nrow()

all_bl_global_agg %>%
  filter(audio_video == "audio") %>%
  filter(FAT > 0 & MOT > 0) %>%
  nrow()

all_bl_global_agg %>%
  filter(audio_video == "audio") %>%
  filter(FAT == 0) %>%
  nrow()

all_bl_global_agg %>%
  filter(audio_video == "audio") %>%
  filter(MOT == 0) %>%
  nrow()

all_bl_global_agg %>%
  filter(audio_video == "video") %>%
  nrow()

all_bl_global_agg %>%
  filter(audio_video == "video") %>%
  filter(FAT > 0 & MOT > 0) %>%
  nrow()

all_bl_global_agg %>%
  filter(audio_video == "video") %>%
  filter(FAT == 0) %>%
  nrow()

all_bl_global_agg %>%
  filter(audio_video == "video") %>%
  filter(MOT == 0) %>%
  nrow()

all_bl_global_agg %>%
  group_by(audio_video) %>%
  summarise(mean_speakers = mean(numspeakers))
