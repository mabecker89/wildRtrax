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
library(purrr)
library(magick)
library(doParallel)
library(ssh)

# For parallel processing:
session <- ssh_connect("amacphail@nfs.wildtrax.ca")
pwd <- ssh_exec_internal(session, command = 'cd /media/BUdata03/ABMI/2019/01/ABMI-0509/ABMI-0509-SW && find . -type f && pwd')
pwd_r <- data.frame(t(matrix(unlist(strsplit(rawToChar(pwd$stdout),"\n")), nrow=length(strsplit(rawToChar(pwd$stdout),"\n")), byrow=T)))
colnames(pwd_r)[1]<-"filepath"
pwd_r$filepath<-gsub("./",'/media/BUdata03/ABMI/2019/01/ABMI-0509/ABMI-0509-SW/',pwd_r$filepath)

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
    "Audio files scanned. Extracting metadata ..." %>>%
    group_by(ftype) %>%
    mutate(
      data = future_map_if(.x = filepath, .p = (ftype == 'wav'), .f = ~ readWave(.x, from = 0, to = Inf, units = "seconds", header = T), .else = ~ read_audio(.x, from = 0, to = Inf), .progress = TRUE),
      length_seconds = future_map_if(.x = data, .p = (ftype == 'wav'), .f = ~ pluck(round(.x$samples/.x$sample.rate,0)), .else = ~ pluck(round(length(.x@left)/.x@samp.rate,2))),
      sample_rate = future_map_if(.x = data, .p = (ftype == 'wav'), .f = ~ pluck(.x$sample.rate)),
      stereo = future_map_if(.x = data, .p = (ftype == 'wav'), .f = ~ pluck(.x$channels))) %>%
    ungroup() %>%
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
    unnest(c("length_seconds","sample_rate","stereo"))
  return(as_tibble(dfraw))
}

tw<-wt_aru_scanner('/volumes/budata/notproofed/2020/01/RETNO','\\.wav$')


s<-read_audio('/users/alexandremacphail/desktop/testwav/ABMI-0509-SW_0+1_20190319_141314.wac')
t<-readWave('/users/alexandremacphail/desktop/testwav/ABMI-0509-SE_20190319_142713.wav', from=0, to=600, units="seconds", header=T)
con<-file('/users/alexandremacphail/desktop/testwav/ABMI-0509-SW_0+1_20190319_141314.wac',"r")
con2<-readBin(con, integer(), n=1, size=4, endian="little")


