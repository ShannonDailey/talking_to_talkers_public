

top3hours_allbl <- function(all_bl) {
  
  top3hours_marker <- read_csv("data/top3hours_ransubj.csv") %>%
    rename(ms_start_tophour = onset,
           ms_end_tophour = offset) %>%
    mutate(subj = as_factor(subj),
           month = as_factor(month))
  
  audio_bl_wmarker <- all_bl %>%
    filter(audio_video=="audio") %>%
    left_join(top3hours_marker)
  
  region1 <- audio_bl_wmarker %>%
    filter(region_num==1) %>%
    filter(onset >= ms_start_tophour &
             onset < ms_end_tophour)
  
  region2 <- audio_bl_wmarker %>%
    filter(region_num==2) %>%
    filter(onset >= ms_start_tophour &
             onset < ms_end_tophour)
  
  region3 <- audio_bl_wmarker %>%
    filter(region_num==3) %>%
    filter(onset >= ms_start_tophour &
             onset < ms_end_tophour)
  
  videos <- all_bl %>%
    filter(audio_video=="video") %>%
    mutate(region_num = NA)
  
  top3hours_all_basiclevel <- region1 %>%
    rbind(region2) %>%
    rbind(region3) %>%
    dplyr::select(-ms_start_tophour,-ms_end_tophour) %>%
    rbind(videos)
  
  return(top3hours_all_basiclevel)
  
}

