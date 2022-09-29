library(blabr)
# This is an alternate version of big_aggregate()
# that runs the *monthly* numbers (i.e. NOT divided by audio-video)

count_mot_fat_noav <- function(x) {
  # print(colnames(x))
  x %>%
    dplyr::filter(speaker %in% c("MOT","FAT"))%>%
    dplyr::group_by(subj, month, speaker)%>%
    dplyr::tally()%>%
    tidyr::spread(speaker, n)
}

count_mot_fat_types_noav <- function(x) {
  # print(colnames(x))
  x %>%
    dplyr::filter(speaker %in% c("MOT","FAT"))%>%
    dplyr::group_by(subj, month, speaker)%>%
    summarise(types = n_distinct(basic_level)) %>%
    pivot_wider(names_from = "speaker", values_from = "types") %>% 
    mutate(MOT = replace_na(MOT, 0),
           FAT = replace_na(FAT, 0)) %>% 
    rename("MOTtypes" = "MOT",
           "FATtypes" = "FAT")
}

count_utterance_noav <- function(x) {
  x %>%
    dplyr::filter(utterance_type %in%c("d","i","q","r","s","n"))%>%
    dplyr::group_by(subj, month, utterance_type)%>%
    dplyr::tally()%>%
    tidyr::spread(utterance_type, n)
}

count_object_present_noav <- function(x) {
  x %>%
    dplyr::filter(object_present %in% c("n","y"))%>%
    dplyr::group_by(subj, month, object_present)%>%
    dplyr::tally()%>%
    tidyr::spread(object_present, n)%>%
    dplyr::mutate(prop_op = y/(n+y))%>%
    dplyr::rename(y_op = y,
                  n_op = n)
}

count_device_and_toy_noav <- function(x) {
  x %>%
    dplyr::filter(speaker %in% c("TOY","TVN","TVM","TVS","TVB"))%>%
    dplyr::group_by(subj, month, speaker)%>%
    dplyr::tally()%>%
    tidyr::spread(speaker, n)
}

add_chi_noun_onset <- function(x) {
  x %>%
    dplyr::filter(speaker == "CHI")%>%
    dplyr::group_by(subj)%>%
    summarise(noun_chi_onset = min(as.numeric(as.character(month))))%>%
    dplyr::right_join(x)
}

chi_noun_onset <- function(x) {
  x %>%
    dplyr::select(subj, noun_chi_onset)%>%
    dplyr::distinct()
}

big_agg_month <- function(x, exclude = NULL, output = NULL, exclude_chi = FALSE) {

  six_to_seventeen_home_noun_chi_onset <- add_chi_noun_onset(x) %>% chi_noun_onset()
  
  if (exclude_chi == TRUE) {
    x <- x %>% filter(speaker != "CHI")
  }
  
  fat_mot_count <- count_mot_fat_noav(x)
  fat_mot_types_count <- count_mot_fat_types_noav(x)
  six_to_seventeen_home_device_count <- count_device_and_toy_noav(x)
  six_to_seventeen_home_utt_count <- count_utterance_noav(x)
  six_to_seventeen_home_op <- count_object_present_noav(x)
  
  big_df <- x %>%
    dplyr::group_by(subj, month)%>%
    summarise(numspeakers = n_distinct(speaker),
              numtokens = n(),
              numtypes = n_distinct(basic_level))%>%
    dplyr::left_join(fat_mot_count)%>%
    dplyr::left_join(fat_mot_types_count) %>% 
    dplyr::left_join(six_to_seventeen_home_utt_count)%>%
    dplyr::left_join(six_to_seventeen_home_device_count)%>%
    dplyr::left_join(six_to_seventeen_home_op)%>%

    dplyr::mutate_each(funs(replace(., which(is.na(.)), 0)))%>%
    dplyr::group_by(subj, month)%>%
    dplyr::mutate(prop_mom = MOT/numtokens,
                  prop_dad = FAT/numtokens,
                  prop_parent = prop_mom+prop_dad,
                  tech_tokens = (TVS+TOY),
                  prop_tech = tech_tokens/numtokens,
                  propd = d/numtokens,
                  propi = i/numtokens,
                  propn = n/numtokens,
                  propq = q/numtokens,
                  propr = r/numtokens,
                  props = s/numtokens,
                  type_token_ratio = numtypes/numtokens,
                  ent_subj_av = entropy::entropy(c(d/numtokens,
                                                   q/numtokens,
                                                   s/numtokens,
                                                   r/numtokens,
                                                   n/numtokens,
                                                   i/numtokens),unit = "log2"),
                  sum_prop_ut = round(sum(c(d/numtokens,
                                            q/numtokens,
                                            s/numtokens,
                                            r/numtokens,
                                            n/numtokens,
                                            i/numtokens)),2)) %>%
    dplyr::left_join(six_to_seventeen_home_noun_chi_onset) %>%
    dplyr::mutate(posttalk =  ifelse(as.numeric(as.character(month))<noun_chi_onset|
                                       is.na(noun_chi_onset),F,T))
  
  if (!is.null(exclude)) {
    big_df = big_df[,!(names(big_df) %in% exclude)]
  }
  
  return(big_df)
}
