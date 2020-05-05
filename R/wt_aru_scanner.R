<<<<<<< HEAD
#' Scans directories of audio data and returns the filepath, filename, file size, date, time, station key, etc, from the filename string and through file.info
=======
#' Scans directories of audio data and returns the date, time, station key, etc, from the filename string. Always recursive.
>>>>>>> b792a41d4838fcf7bb0a8dea7eab8dd452e56820
#'
#' @param path0
#' @param pattern
#'
<<<<<<< HEAD
#' @import stringr base stats lubridate
#' @return
=======
#' @import stringr lubridate base stats
#' @return Returns a data frame with file paths and corresponding attributes
>>>>>>> b792a41d4838fcf7bb0a8dea7eab8dd452e56820
#' @export
#'
#' @examples

wt_aru_scanner <- function(path0, pattern) {

<<<<<<< HEAD
  dfraw <- as.data.frame(file.info(list.files(path = path0,
                                    pattern = pattern,
                                    recursive = TRUE,
                                    full.names = TRUE,
                                    include.dirs = FALSE))) #Create a dataframe that is a list of files defined by path; pattern is regex usually "\\.wac$|\\.wav$|\\.mp3$|\\.flac$"
=======
  dfraw<-as.data.frame(file.info(list.files(path=path0, pattern=pattern, recursive=TRUE, full.names=TRUE,include.dirs=FALSE))) #Scan directory and return file metadata
>>>>>>> b792a41d4838fcf7bb0a8dea7eab8dd452e56820

  setDT(dfraw,keep.rownames = T) #Make rownames with path to column

  colnames(dfraw)[1]<-"Filepath"

  dfraw$size<-round(dfraw$size/1000000,2) #Convert size to megabytes

  dfraw<-dfraw %>%
    dplyr::select(Filepath,size) #Get rid of other columns

  setDT(dfraw,keep.rownames = T) #Move rownames to column

  colnames(dfraw)[1]<-"Filepath" #Rename file path column

  dfraw$size<-round(dfraw$size/1000000,2) #Convert to megabytes

  dfraw<-dfraw %>%
    dplyr::select(Filepath,size)

  dfraw$Filename <- basename(gsub("\\..*", "", dfraw$Filepath)) #Get filename from the filepath

  dfraw$Station <- ifelse((str_detect(dfraw$Filename, "_0+1_")), #Create the station key; concatenate, dataset, site, station.
                        (str_split_fixed(dfraw$Filename, "_", n=3)),
                        (str_split_fixed(dfraw$Filename, "_", n=2)))

  dfraw$Julian_Date <- yday(as.Date(str_replace(str_sub(str_sub(dfraw$Filename, -15), 1, str_length(str_sub(dfraw$Filename, -15))-7),
                                                "(\\d{4})(\\d{2})(\\d{2})$", "\\1-\\2-\\3"))) #Get the julian date (easier for data graphing imo)

  dfraw$Time <- str_sub(dfraw$Filename, -6) #Get the time substring

  dfraw[order(dfraw$Filename), ] #Re-order by filename to get the time sequence

  #Get year of the data; helps to sort things if multiple years of a station exists Should pull this from the year of the time prefix instead - fix eventually
  dfraw$Year<-as.numeric(vapply(strsplit(as.character(dfraw$Filepath), "/"), `[`, 6, FUN.VALUE=character(1)))

  #Creates an index of all the unique times of the dataframe for each temporal value. So day 1 midnight = 1, day 1 dawn = 4. That way you can pick from the 4th, 5th, recording of the day if you want which would corrrspond to tracking dawn, dusk, etc.
  dfraw$Time_index <- ave(paste(dfraw$Station, dfraw$Julian_Date, dfraw$Year, sep=""), paste(dfraw$Station, dfraw$Julian_Date, dfraw$Year, sep=""), FUN=seq_along)

  return(dfraw)

}

wt_aru_scanner('/users/alexandremacphail/desktop/testwav/,'
