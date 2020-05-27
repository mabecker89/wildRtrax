#' Scans directories of audio data and returns the filepath, filename, file size, date, time, station key, sample rate, length and number of channels to be used as filters for wt_aru_assign or other uses
#'
#' @param path
#' @param file_type
#'
#' @import furr fs pipeR tibble stringr lubridate tidyverese bioacoustics soundecology seewave tuneR
#' @return dfraw
#' @export
#'
#' @examples z<-wt_aru_scanner('/volumes/budata/abmi/2019/01','wav')
#'
#'
----------------------------

# Alternative structure for scanner

# All required packages
library(furrr)
library(fs)
library(pipeR)
library(tibble)
library(stringr)
library(lubridate)
library(tidyverse)
library(bioacoustics)
library(soundecology)
library(seewave)
library(tuneR)

# For parallel processing:
plan(multiprocess)

wt_aru_scanner <- function(path, file_type) {
  # Convert to regex ... have to think this through more.
  if(file_type == "wav") {
    ext <- "wav"
  } else if (file_type == "wac") {
    ext <- "wac"
  } else if (file_type == "flac") {
    ext <- "flac"
  } else (file_type == "mp3") {
    ext <- "mp3"
  }
  dfraw <-
    # First list then retrieve file size
    dir_ls(path = path, recurse = TRUE, regexp = ext) %>>%
    "Scanning audio files ..." %>>%
    future_map_dbl(., .f = ~ file_size(.), .progress = TRUE) %>%
    enframe() %>%
    # Convert size to megabytes
    mutate(size_Mb = round(value / 10e5, digits = 2)) %>%
    select(filepath = name, size_Mb) %>%
    mutate(filename = str_replace(basename(filepath), "\\..*", "")) %>%
    # Parse station key and recording date time
    separate(filename, into = c("station_key", "recording_date_time"), #Need a conditional statement here to handle different conditions of file names e.g. SM3s have _0+1_
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
                             .progress = TRUE), ### See file_type options below for functions that can read wac and flac
           length_seconds = future_map(.x = data, .f = ~ round(.x$samples / .x$sample.rate)),
           sample_rate = future_map(.x = data, .f = ~ pluck(.x$sample.rate)),
           n_channels = future_map(.x = data, .f = ~ pluck(.x$channels))) %>%
    select(filepath, filename, size_Mb, station_key, recording_date_time, year, julian, time_index,
           length_seconds:n_channels) %>%
    unnest(c('length_seconds','sample_rate','n_channels')) #Stack lists

  return(dfraw)
}

#For reading different filetypes
read_wac(filepath) #in bioacoustics
wac2flac(filepath,reverse=T) #in seewave
#MP3
r<-readMP3(filepath)
writeWave(r,extensible=T)












