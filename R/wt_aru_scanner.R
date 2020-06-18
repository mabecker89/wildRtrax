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
library(tictoc)

# For parallel processing:
plan(multisession)

wt_aru_scanner <- function(path, file_type) {
  dfraw <-
    # First list then retrieve file size
    dir_ls(path = path, recurse = TRUE, regexp = file_type) %>>%
    "Scanning audio files ..." %>>%
    future_map_dbl(., .f = ~ file_size(.), .progress = TRUE) %>%
    enframe() %>%
    # Convert size to megabytes
    mutate(size_Mb = round(value / 10e5, digits = 2)) %>%
    select(filepath = name, size_Mb) %>%
    mutate(filename = str_replace(basename(filepath), "\\..*", "")) %>%
    # Parse station key and recording date time
    separate(filename, into = c("station_key", "recording_date_time"),
             sep = "_", extra = "merge", remove = FALSE) %>%
    mutate(recording_date_time = ymd_hms(recording_date_time),
           julian = yday(recording_date_time),
           year = year(recording_date_time)) %>%
    mutate(ftype = str_extract(filepath,'.{3}$')) %>%
    arrange(station_key, recording_date_time) %>%
    # Create time index
    group_by(station_key, year, julian) %>%
    mutate(time_index = row_number()) %>%
    ungroup() %>>%
    # Obtain metadata from audio files
    "Audio files scanned. Extracting metadata ..." %>>%
  #WAV METADATA
    mutate(data = future_map(.x = filepath,
                             .f = if (ftype == 'wav') { ~ readWave(.x, from = 0, to = Inf, units = "seconds", header = TRUE) }
                             else if (ftype == 'wac') { ~ read_wac(.x) }
                             else if (ftype == 'mp3') { ~ readMP3(.x) }
                             else if (ftype == 'flac') { ~ wac2flac(.x, reverse=T) }
                             else {print('File type not supported for ', .x)},
                             .progress = TRUE),
           length_seconds = future_map(.x = data, .f = ~ round(length(.x@left) / .x@samp.rate,2)),
           sample_rate = future_map(.x = data, .f = ~ pluck(.x@samp.rate)),
           n_channels = future_map(.x = data, .f = ~ pluck(.x@stereo))) %>%
    select(filepath, filename, size_Mb, station_key, recording_date_time, year, julian, time_index,
           length_seconds:n_channels) %>%
    unnest(c('length_seconds','sample_rate','n_channels')) #Stack lists
  dfraw$n_channels <- ifelse(dfraw$n_channels == TRUE,2,1)
  return(as.data.frame(dfraw))
}












