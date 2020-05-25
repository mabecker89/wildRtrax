abmi <- data.frame(julian = 90:210) %>%
  crossing(time_index = 1:4) %>%
  mutate(blocks = case_when(
    julian %in% 90:139 & time_index == 1 ~ 9,
    julian %in% 140:159 & time_index == 1 ~ 10,
    julian %in% 160:179 & time_index == 1 ~ 11,
    julian %in% 180:210 & time_index == 1 ~ 12,
    julian %in% 90:104 & time_index == 3 ~ 1,
    julian %in% 105:119 & time_index == 4 ~ 2,
    julian %in% 120:139 & time_index == 3 ~ 3,
    julian %in% 140:149 & time_index == 3 ~ 4,
    julian %in% 150:159 & time_index == 4 ~ 5,
    julian %in% 160:169 & time_index == 3 ~ 6,
    julian %in% 170:179 & time_index == 4 ~ 7,
    julian %in% 180:210 & time_index == 4 ~ 8,
    TRUE ~ NA_real_
  ))
