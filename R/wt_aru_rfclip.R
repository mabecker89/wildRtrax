#' Clip audio recordings that contain human speech at the beginning of the recording
#'
#' @param path
#' @param method
#' @param spectro
#' @param clip_out
#' @param verbose
#'
#' @return
#'
#' @importFrom tuneR readWave writeWave stereo
#' @importFrom seewave acoustat addsilw
#' @import tools stringr
#'
#' @export
#'
#' @examples

wt_aru_rfclip <- function(path, method, spectro = FALSE, clip_out = FALSE, verbose = FALSE) {

  substrRight <- function(x, n) {
    substr(x, nchar(x) - n + 1, nchar(x))
  }

  pattern0 <- c("\\.wav$|\\.wac$|\\.sm3dump$|\\.sm4dump$")

  mydir <- list.files(as.character(path), pattern = pattern0, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)

  mydir_df<-as.data.frame(mydir)

  times<-list()
  for (i in mydir) {
    tryCatch(
      t <- readWave(i, from = 0, to = 60, units ="seconds"),
      error = function(e) {
        msg <- conditionMessage(e)
        print(paste0(msg, i, sep=' *** '))}
    )

    if (spectro == TRUE) {
      v <- ggspectro(t, ovlp = 50) + geom_tile(aes(fill=amplitude)) + scale_fill_gradient(low = "#A19E99", high = "#C7461F")
      print(v)
    } # Graph the spectral signatures; this can take a long time

    u <- acoustat(t, plot = FALSE)
    timeanalysis <- as.data.frame(u$time.contour)
    model<-lm(formula = contour ~ time, data = timeanalysis)
    modelq<-lm(formula = contour ~ time + time^2, data = timeanalysis) #quadratic

    if (verbose == TRUE){
      print(summary(modelq))
      print(anova(model, modelq))
    }
    #plot(modelq)
    model2<-segmented(modelq) #Run the segmented model for breaks

    if (verbose == TRUE) {
      plot.segmented(model2)
    }

    Ps <- paste0(round(model2$psi[2], 0), i)  ## where i is whatever your Ps is; get the time breakpoint
    print(Ps)
    times[[paste0(round(model2$psi[2],0), i)]] <- Ps
  }

  # Setup dataframe for the write
  times_df<-do.call(rbind.data.frame,times)
  colnames(times_df)[1]<-'Filepath'
  times_df$sec <- vapply(strsplit(as.character(times_df$Filepath), "/"), `[`, 1, FUN.VALUE = character(1)) #Remove the pesky prefix
  times_df$Filepath <- gsub('^[^/]+', '', times_df$Filepath)
  setnames(times_df, 'sec', 'sec_start')
  times_df$sec_start <- as.numeric(as.numeric(times_df$sec_start) + 5) #Add a buffer for the tone and scrunching; this could be a user input
  times_df$sec_end <- as.numeric(times_df$sec_start) + 600 #Or desired interval of task; adding 1 seconds should hopefully account for tone
  times_df$newfilename <- file_path_sans_ext(basename(times_df$Filepath))
  times_df$date <- as.Date(str_replace(str_sub(str_sub(times_df$newfilename, -15), 1,
                                             str_length(str_sub(times_df$newfilename,-15))-7),
                                     "(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3"))
  times_df$time <- format(strptime(str_sub(times_df$newfilename, -6), format="%H%M%S"), "%H:%M:%S")
  times_df$datetime <- as.POSIXct(paste(times_df$date, times_df$time), format = "%Y-%m-%d %H:%M:%S")
  times_df$newtime <- times_df$datetime + times_df$sec_start
  times_df$newfilename1<-paste0(
    dirname(times_df$Filepath),
    '/',
    gsub('\\_(.*)', '', times_df$newfilename),
    '_',
    gsub(' ', '_', gsub('-|:', '', times_df$newtime)),
    '.wav')

  # Write the clipped wavs to the method length needed
  if (verbose == TRUE) {
    print(times_df)
  }

  if (clip_out == TRUE) {
    for (i in 1:nrow(times_df)) {
      tryCatch(
        x <- readWave(times_df$Filepath[i], from = times_df$sec_start[i], to = times_df$sec_end[i], units = "seconds"), #Will fail if to longer than recording length
        error = function(e2) {
          msg <- conditionMessage(e2)
          print(paste0(msg, i, sep=' *** '))}
      )
      if ((as.numeric(length(x) / x@samp.rate)) < method) {
        if (verbose==TRUE){
          print(paste0(basename(times_df$Filepath[i]), ' *** too short; adding silence for ', round(method - (as.numeric(length(x) / x@samp.rate)), 3), ' seconds'))
        }
        y_1 <- addsilw(x, channel = 1, f = x@samp.rate, at = "end", d = method - (as.numeric(length(x) / x@samp.rate)), output = "Wave") #left add silence
        y_2 <- addsilw(x, channel = 2, f = x@samp.rate, at = "end", d = method - (as.numeric(length(x) / x@samp.rate)), output = "Wave") #right add silence
        y <- stereo(y_1, y_2) # merged to stereo
        writeWave(y, filename = as.character(times_df$newfilename1[i]), extensible = TRUE)
      } else {
        writeWave(x, filename = as.character(times_df$newfilename1[i]), extensible = TRUE)
      }
    }
  }

  # What are we returning?

}


