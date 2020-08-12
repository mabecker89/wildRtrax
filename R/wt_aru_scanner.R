#' Scans directories of audio data and returns the filepath, filename, file size, date, time, station key, sample rate, length /(s)/ and number of channels to be used as filters for wt_aru_assign or other uses
#'
#' @param path
#' @param file_type
#'
#' @import furr fs pipeR tibble stringr lubridate tidyverese bioacoustics soundecology seewave tuneR tictoc tools
#' @return dfraw, summary of scanned volume and time
#' @export
#'
#' @examples z<-wt_aru_scanner('/volumes/budata/abmi/2019/01','\\.wac$|\\.wav$|\\.mp3$')
#'
#'----------------------------# Alternative structure for scanner

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
library(tools)

# For parallel processing:
plan(multisession)

wt_aru_scanner <- function(path, file_type) {
  dfraw <-
    # First list then retrieve file size
    dir_ls(path = path,
           recurse = TRUE,
           regexp = file_type) %>>%
    "Scanning audio files ..." %>>%
    future_map_dbl(., .f = ~ file_size(.), .progress = TRUE) %>%
    enframe() %>%
    # Convert size to megabytes
    mutate(size_Mb = round(value / 10e5, digits = 2)) %>%
    select(filepath = name, size_Mb) %>%
    mutate(filename = str_replace(basename(filepath), "\\..*", "")) %>%
    mutate(ftype = file_ext(filepath)) %>%
    # Parse location and recording date time
    separate(
      filename,
      into = c("location", "recording_date_time"),
      sep = "(?:_0\\+1_|_)",
      extra = "merge",
      remove = FALSE
    ) %>%
    mutate(
      recording_date_time = ymd_hms(recording_date_time),
      julian = yday(recording_date_time),
      year = year(recording_date_time)
    ) %>%
    arrange(location, recording_date_time) %>%
    # Create time index
    group_by(location, year, julian) %>%
    mutate(time_index = row_number()) %>%
    ungroup() %>>%
    # Obtain metadata from audio files
    "Audio files scanned. Extracting metadata ..." %>>%
    mutate(
      data = future_map(.x = filepath, .f = ~ readWave(.x, from = 0, to = Inf, units = "seconds", header = T), .progress = TRUE),
      length_seconds = future_map(.x = data, .f = ~ round(.x$samples/.x$sample.rate,0)),
      sample_rate = future_map(.x = data, .f = ~ pluck(.x$sample.rate)),
      stereo = future_map(.x = data, .f = ~ pluck(.x$channels))
    ) %>%
    select(
      filepath,
      filename,
      size_Mb,
      location,
      recording_date_time,
      year,
      julian,
      time_index,
      ftype,
      length_seconds,
      sample_rate,
      stereo
    ) %>%
    unnest(c('length_seconds', 'sample_rate', 'stereo'))
  return(as.data.frame(dfraw))
}

blpw_r<-wt_aru_scanner('/volumes/budata/enwa/enwa-o-09-03','\\.wav$')
