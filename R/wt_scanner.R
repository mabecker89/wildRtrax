#' Scan directories of raw audio files and return columns populated with information derived from the filename string.
#'
#' @param path0 A character string indicating, the parent directory that is being scanned. Always recursive.
#' @param pattern A regular expression. Only file names which match the regex will be returned.
#' @import base stringr lubridate stats
#' @export
#'
#' @return A dataframe with column values: Filepath, Filename, Station, Julian Date, Time, Year and Time index, the latter being the sequentially ordered recordings by day

# Run the scanner function
wt_scanner<-function(path0,pattern) {
  dfraw<-as.data.frame(list.files(path=path0, pattern=pattern, recursive=TRUE, full.names=TRUE,include.dirs=FALSE))
  colnames(dfraw)[1]<-"Filepath"
  dfraw$Filename<-basename(gsub("\\..*","",dfraw$Filepath))
  dfraw$Station<-ifelse((str_detect(dfraw$Filename, "_0+1_")),
                        (str_split_fixed(dfraw$Filename, "_", n=3)),
                        (str_split_fixed(dfraw$Filename, "_", n=2)))
  dfraw$Julian_Date<-yday(as.Date(str_replace(str_sub(str_sub(dfraw$Filename,-15),1,str_length(str_sub(dfraw$Filename,-15))-7),"(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3")))
  dfraw$Time<-str_sub(dfraw$Filename,-6)
  dfraw[order(dfraw$Filename),]
  dfraw$Year<-as.numeric(vapply(strsplit(as.character(dfraw$Filepath),"/"), `[`, 5, FUN.VALUE=character(1))) #Should pull this from the year of the time prefix instead - fix eventually
  dfraw$Time_index<-ave(paste(dfraw$Station, dfraw$Julian_Date, dfraw$Year, sep=""), paste(dfraw$Station, dfraw$Julian_Date, dfraw$Year, sep=""), FUN=seq_along)
  return(dfraw)
}
