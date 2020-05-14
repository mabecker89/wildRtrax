#' Scans directories of audio data and returns the filepath, filename, file size, date, time, station key, etc, from the filename string and through file.info
#'
#' @param path0
#' @param pattern
#'
#' @import stringr base stats lubridate tuneR R.utils tidyverse data.table
#' @return
#' @export
#'
#' @examples

library(stringr)
library(base)
library(lubridate)
library(tuneR)
library(R.utils)
library(doParallel)
library(foreach)
library(tidyverse)
library(data.table)

wt_aru_scanner <- function(path0, pattern) {

  dfraw <- as.data.frame(file.info(list.files(path = path0,
                                    pattern = pattern,
                                    recursive = TRUE,
                                    full.names = TRUE,
                                    include.dirs = FALSE))) #Create a dataframe that is a list of files defined by path; pattern is regex usually "\\.wac$|\\.wav$|\\.mp3$|\\.flac$"

  pb1<- txtProgressBar(min = 0, max = nrow(dfraw), style=3, title = "Reading audio file lengths...")

  setDT(dfraw,keep.rownames = T) #Move rownames to column

  colnames(dfraw)[1] <- "Filepath" #Rename column to Filepath

  dfraw$size<-round(as.numeric(dfraw$size)/1000000,2) #Convert to megabytes

  dfraw <- dfraw %>%
    dplyr::select(Filepath,size)

  dfraw$Filepath <- as.character(dfraw$Filepath)

  dfraw$Filename <- basename(gsub("\\..*", "", dfraw$Filepath)) #Get filename from the filepath

  dfraw$Station <- ifelse((str_detect(dfraw$Filename, "_0+1_")), #Create the station key; concatenate, dataset, site, station.
                        (str_split_fixed(dfraw$Filename, "_", n=3)),
                        (str_split_fixed(dfraw$Filename, "_", n=2)))

  dfraw$JD<-yday(as.Date(str_replace(str_sub(str_sub(dfraw$Filename, -15), 1, str_length(str_sub(dfraw$Filename, -15))-7),
                             "(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3"))) #Get the julian date (easier for data graphing imo)

  dfraw$Time <- str_sub(dfraw$Filename, -6) #Get the time substring

  #Get year of the data
  dfraw$Year<-as.numeric(vapply(strsplit(as.character(dfraw$Filepath), "/"), `[`, 6, FUN.VALUE=character(1)))

  dfraw[order(dfraw$Filename), ] #Reorder the dataframe to accept the time index properly

  #Creates an index of all the unique times of the dataframe for each date and time value. So day 1 midnight = 1, day 1 dawn = 4. That way you can pick from the 4th, 5th, recording of the day if you want which would corrrspond to tracking dawn, dusk, etc.
  dfraw$Time_index <- ave(paste(dfraw$Station, dfraw$Julian_Date, dfraw$Year, sep=""), paste(dfraw$Station, dfraw$Julian_Date, dfraw$Year, sep=""), FUN=seq_along)

  #Read all the audio files and return length in seconds; need to add sample rate here too eventually
  dfraw$length <-0

  #Read all the audio files and return length in seconds; need to add sample rate here too eventually
  for (i in 1:nrow(dfraw)) {
    if(dfraw$size[i]>0) {
      tryCatch(x<-readWave(dfraw$Filepath[i],from=0,to=Inf, units="seconds",header=T),
               error=function(e2) {
                 msg<-conditionMessage(e2)
                 print(paste0(msg, i, sep=' *** '))})
      dfraw$length[i]<-round(x$samples / x$sample.rate)
      setTxtProgressBar(pb1,i)}
  }

  #Wrap it up / shut down progress bar
  print(dfraw)
  return(dfraw)
  close(pb)
}
