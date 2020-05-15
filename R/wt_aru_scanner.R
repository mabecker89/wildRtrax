#' Scans directories of audio data and returns the filepath, filename, file size, date, time, station key, etc, from the filename string and through file.info
#'
#' @param path0
#' @param pattern
#'
#' @import stringr base stats lubridate tuneR R.utils tidyverse data.table tools
#' @return dfraw
#' @export
#'
#' @examples
#'
#'

library(stringr)
library(base)
library(stats)
library(lubridate)
library(tuneR)
library(R.utils)
library(tidyverse)
library(data.table)
library(tools)

wt_aru_scanner <- function(path0, pattern) {

  print('Scanning audio files... please wait...')
  print('')

  dfraw <- as.data.frame(file.info(list.files(path = path0,
                                    pattern = pattern,
                                    recursive = TRUE,
                                    full.names = TRUE,
                                    include.dirs = FALSE))) #Create a dataframe that is a list of files defined by path; pattern is regex usually "\\.wac$|\\.wav$|\\.mp3$|\\.flac$"

  print('Audio files scanned... extracting metadata...')
  print('')

  pb1<- txtProgressBar(min = 0, max = nrow(dfraw), style=3, title = "Reading audio file lengths...")

  setDT(dfraw,keep.rownames = T) #Move rownames to column

  colnames(dfraw)[1] <- "Filepath" #Rename column to Filepath

  dfraw$size<-round(as.numeric(dfraw$size)/1000000,2) #Convert to megabytes

  dfraw <- dfraw %>%
    dplyr::select(Filepath,size)

  dfraw$Filepath <- as.character(dfraw$Filepath)

  dfraw$Filename <- basename(gsub("\\..*", "", dfraw$Filepath)) #Get filename from the filepath

  dfraw$Station_key <- ifelse((str_detect(dfraw$Filename, "_0+1_")), #Create the station key; concatenate, dataset, site, station.
                        (str_split_fixed(dfraw$Filename, "_", n=3)),
                        (str_split_fixed(dfraw$Filename, "_", n=2)))

  dfraw$recording_date <- as.Date(str_replace(str_sub(str_sub(dfraw$Filename, -15), 1, str_length(str_sub(dfraw$Filename, -15))-7),
                             "(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3")) #Get the date from the filename string

  dfraw$Julian_Date <- yday(dfraw$recording_date) #Get the julian date (easier for data graphing imo)

  dfraw$Time<-format(strptime(str_sub(dfraw$Filename,-6),format="%H%M%S"),"%H:%M:%S") #Get the time substring

  #Get year of the data only works for standard filenames
  dfraw$Year<-str_sub(str_extract(dfraw$Filename,'([^_]+)(?:_[^_]+)$'),1,4)

  dfraw[order(dfraw$Filename), ] #Reorder the dataframe to accept the time index properly

  #Creates an index of all the unique times of the dataframe for each date and time value. So day 1 midnight = 1, day 1 dawn = 4. That way you can pick from the 4th, 5th, recording of the day if you want which would corrrspond to tracking dawn, dusk, etc.
  dfraw$Time_index <- ave(paste(dfraw$Station_key, dfraw$Julian_Date, dfraw$Year, sep=""), paste(dfraw$Station_key, dfraw$Julian_Date, dfraw$Year, sep=""), FUN=seq_along)

  #Read all the audio files and return length in seconds; need to add sample rate here too eventually
  dfraw$length <- 0
  dfraw$samplerate <- 0
  dfraw$channels <- 0

  dfraw$type <- file_ext(dfraw$Filepath) #Get filetype

  #Read all the audio files and return length in seconds; need to add sample rate here too eventually
  for (i in 1:nrow(dfraw)) {
    if(dfraw$size[i]>0) {
      tryCatch(x<-readWave(dfraw$Filepath[i],from=0,to=Inf, units="seconds",header=T),
               error=function(e2) {
                 msg<-conditionMessage(e2)
                 print(paste0(msg, i, sep=' *** '))})
      dfraw$length[i]<-round(x$samples / x$sample.rate)
      dfraw$samplerate[i]<-x$sample.rate
      dfraw$channels[i]<-x$channels
      setTxtProgressBar(pb1,i)}
  }

  #Wrap it up / shut down progress bar
  write.csv(dfraw,'./test_wt_aru_scanner_upsi.csv')
  close(pb1)
  return(dfraw)
}

wt_aru_scanner('/volumes/budata/abmi/2019/01/abmi-0409','\\.wav$')

t<-read.csv('/users/alexandremacphail/wildRtrax/test_wt_aru_scanner_upsi.csv')[,-1]
ggplot(t,aes(Julian_Date)) + geom_bar()





