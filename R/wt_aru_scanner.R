#' Scans directories of audio data and returns the filepath, filename, file size, date, time, station key, sample rate, length /(s)/ and number of channels to be used as filters for wt_aru_assign or other uses
#'
#' @param path
#' @param file_type
#'
#' @import furr fs pipeR tibble stringr lubridate tidyverese tuneR tictoc tools
#' @return dfraw, summary of scanned volume and time
#' @export
#'
#' @examples z<-wt_aru_scanner('/volumes/budata/abmi/2019/01','\\.wac$|\\.wav$')
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
library(purrr)
library(magick)
library(doParallel)
library(ssh)

#Example usage
tw <-
  wt_aru_scanner('/volumes/budata/abmi/2019/01/abmi-0409', '\\.wav$|\\.wac$')

#Look at a wave object
f<-read_audio('/users/alexandremacphail/desktop/testwav/test2/ABMI-0509-SW_0+1_20190319_141314.wac')
#Look at a list read from a wav file
g<-readWave('/users/alexandremacphail/desktop/testwav/test2/ABMI-0509-SE_20190319_142713.wav', header = T)

plan(multisession)
wt_aru_scanner <- function(path, file_type) {
  tic()
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
    "Audio files scanned. Extracting metadata ..." %>>% #MARCUS HALPPPP
    group_by(ftype) %>% #Group by filetype to lighten the futures load if need be (only when storing wave objects)
    mutate(data = future_map_if(.x = filepath, #choose the filepath
                             .p = (ftype == 'wav'), #predicate with type
                             .f = ~ readWave(.x, from = 0, to = Inf, units = "seconds", header = T), #if true, reads a wave file as a list because of header = T. You don't want it to be false otherwise it will read it as an S4 wave object **too big**
                             .else = ~ lst(sample_rate = read_audio(.x, from = 0, to = Inf)@samp.rate, #Create a list that mimics the header = T list that comes out of readWave. Note @ pipe for S4 objects
                                           channels = read_audio(.x, from = 0, to = Inf)@stereo,
                                           samples = length(read_audio(.x, from = 0, to = Inf)@left)),
                             .progress = TRUE),
           length_seconds = future_map_if(.x = data,
                                          .p = ftype == 'wav',
                                          .f = ~ as.factor(round(.x$samples / .x$sample.rate,0)), #Made everything factors to force them into the columns
                                          .else ~ as.factor(.x[[3]]/.x[[1]])), #extract specific items from list not working!!
           sample_rate = future_map_if(.x = data,
                                       .p = ftype == 'wav',
                                       .f = ~ as.factor(.x$sample.rate),
                                       .else ~ as.factor(.x[[1]])),
           n_channels = future_map_if(.x = data,
                                   .p = ftype == 'wav',
                                   .f = ~ as.factor(.x$channels),
                                   .else = ~ as.factor(.x[[2]]))) %>%
    ungroup() %>% #Finalize tibble
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
      n_channels
    ) %>%
    unnest(c("length_seconds", "sample_rate", "n_channels")) %>%
    mutate(length_seconds = as.integer(length_seconds),
           sample_rate = as.integer(sample_rate),
           n_channels = as.integer(n_channels))
    #take just one value from the nested cells
  toc()
  return(as_tibble(dfraw)) #Output as a tibble
}


# For ssh connections: an experiment
session <- ssh_connect("amacphail@nfs.wildtrax.ca")
pwd <-
  ssh_exec_internal(session, command = 'cd /media/BUdata03/ABMI/2019/01/ABMI-0509/ABMI-0509-SW && find . -type f && pwd')
pwd_r <-
  data.frame(t(matrix(
    unlist(strsplit(rawToChar(pwd$stdout), "\n")),
    nrow = length(strsplit(rawToChar(pwd$stdout), "\n")),
    byrow = T
  )))
colnames(pwd_r)[1] <- "filepath"
pwd_r$filepath <-
  gsub("./",
       '/media/BUdata03/ABMI/2019/01/ABMI-0509/ABMI-0509-SW/',
       pwd_r$filepath)

