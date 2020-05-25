#-------------------------------------------------------

# Testing functions

# Required packages
library(tuneR)
library(furrr)
library(dplyr)
library(lubridate)
library(stringr)
library(fs)
library(pipeR)
library(tidyr)
library(tibble)
library(soundecology)
library(tictoc)

root <- "B:/ABMI/2019/01/ABMI-0409/ABMI-0409-NE"

# 1. Scanner

wt_aru_scanner <- function(path, file_type) {

# Is it only ever going to be wav or wac files? We'll need to be able to handle other audio formats.
# readWave won't work if not wav.
safe_readWave <- safely(readWave)

plan(multiprocess)

tic()
dfraw <-
  # First list then retrieve file size
  dir_ls(path = root, recurse = TRUE, regexp = "\\.wav$|\\.wac$") %>>%
  "Scanning audio files ..." %>>%
  future_map_dbl(., .f = ~ file_size(.), .progress = TRUE) %>%
  enframe() %>%
  # Convert size to megabytes
  mutate(size_Mb = round(value / 10e6, digits = 2)) %>%
  select(filepath = name, size_Mb) %>%
  mutate(filename = str_replace(basename(filepath), "\\..*", "")) %>%
  # Parse station key and recording date time
  separate(filename, into = c("station_key", "recording_date_time"),
           sep = "_", extra = "merge", remove = FALSE) %>%
  mutate(recording_date_time = ymd_hms(recording_date_time),
         julian = yday(recording_date_time),
         year = year(recording_date_time)) %>%
  arrange(station_key, recording_date_time) %>%
  # Create time index
  group_by(station_key, year, julian) %>%
  mutate(time_index = row_number()) %>%
  ungroup() %>>%
  # Obtain metadata from audio files
  "Audio files scanned. Extracting metadata ..." %>>%
  mutate(data = future_map(.x = filepath,
                           .f = ~ readWave(., from = 0, to = Inf, units = "seconds", header = TRUE),
                           .progress = TRUE),
         length_seconds = future_map(.x = data, .f = ~ round(.x$samples / .x$sample.rate)),
         sample_rate = future_map(.x = data, .f = ~ pluck(.x$sample.rate)),
         n_channels = future_map(.x = data, .f = ~ pluck(.x$channels))) %>%
  select(filepath, filename, size_Mb, station_key, recording_date_time, year, julian, time_index,
         length_seconds:n_channels)
toc()

return(dfraw)

}


# 2. Assign

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

dfraw1 <- dfraw %>%
  left_join(abmi, by = c("julian", "time_index")) %>%
  filter(!is.na(blocks)) %>%
  group_by(station_key, blocks) %>%
  sample_n(1)

tic()
dfraw2 <- dfraw1 %>%
  mutate(data = future_map(.x = filepath, .f = ~ readWave(., from = 0, to = 180, units = "seconds")),
         results = future_map(.x = data, .f = ~ ndsi(., fft_w = 2048, anthro_min = 0, anthro_max = 2000,
                                                     bio_min = 2000, bio_max = 8000))) %>%
  select(-data)
toc()


tic()
dfraw1$anthrophony <- 0
dfraw1$biophony <- 0
for (i in 1:nrow(dfraw1)) {
  if(dfraw1$size_Mb[i]>0) {
    x<-readWave(dfraw1$filepath[i],from=0,to=180, units="seconds")
    results<-ndsi(x,fft_w=2048,anthro_min=0,anthro_max=2000,bio_min=2000,bio_max=8000)
    if(dfraw1$n_channels[i]=="mono"){

    } else {
      dfraw1$anthrophony[i]<-(results$anthrophony_left+results$anthrophony_right)/2
      dfraw1$biophony[i]<-(results$biophony_left+results$biophony_right)/2}
  }}
toc()











