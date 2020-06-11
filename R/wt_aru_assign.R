#' Subsets data to select audio recordings for transcription. Input is the result of the
#' wt_aru_scanner used to get a full list of the audio data and metadata attributes to select from
#'
#' Requires installation of the QUT audio-analysis packagae as well for geophonic indices. Perhaps include in function directly!
#'
#' @param df #wt_aru_scanner dataframe output
#' @param subsample_templates # choose from a variety of subsampling techniques include the ABMI stratified sampling design or the BU standard dawn chorus.
#' manual overrides and let you select through the remaining fields below
#' @param jd_start #date to start with
#' @param jd_end #date to end with
#' @param time_index #Range of time indices that can be selected e.g. c(1:4)
#' @param sample_size #Number of replicates using the date and time choices per spatial location
#'
#' @import tidyverse stringr soundecology bioacoustics
#' @return df_out plot1
#' @export
#'
#' @examples x<-wt_aru_assign(wt_aru_scanner, blocks="ABMI stratified")
#'
library(bioacoustics)
library(soundecology)

wt_aru_assign <- function(df, blocks = c('ABMI_stratified', 'BU_days', 'manual'), jd_start, jd_end, time_index, sample_size) {
  #Start by creating a "block tower" basically assign each file into each stratified block
  if (blocks == 'ABMI_stratified') {
    #Create the lookup table
    abmi <- data.frame(julian = 90:210) %>%
      crossing(time_index = 1:4) %>%
      mutate(blocks = case_when(
        julian %in% 90:139 & time_index == 1 ~ 9,
        julian %in% 140:159 & time_index == 1 ~ 10,
        julian %in% 160:179 & time_index == 1 ~ 11,
        julian %in% 180:210 & time_index == 1 ~ 12,
        julian %in% 90:104 & time_index == 3 ~ 1,
        julian %in% 105:119 & time_index == 4 ~ 2,
        julian %in% 120:139 & time_index == 3 ~ 3,
        julian %in% 140:149 & time_index == 3 ~ 4,
        julian %in% 150:159 & time_index == 4 ~ 5,
        julian %in% 160:169 & time_index == 3 ~ 6,
        julian %in% 170:179 & time_index == 4 ~ 7,
        julian %in% 180:210 & time_index == 4 ~ 8,
        TRUE ~ NA_real_
      ))
    df<-merge(df,abmi,by=c("julian","time_index"))
    #Subsample
    df <- df[complete.cases(df$blocks), ]
      df_out <- df %>%
        group_by(station_key,blocks) %>%
        na.omit() %>%
        sample_n(1)
      files <- unlist(df_out$filepath)
      print(paste0(round((nrow(df_out)/nrow(t)),3)*100, '% of the dataset is being subsampled with the ABMI stratified design'))
      df_out$filepath<-as.character(df_out$filepath)
     #BIOPHONIC AND ANTHROPOPHONIC FILTERS
      for (i in 1:nrow(df_out)) {
        if(df_out$size_Mb[i]>0) {
          sound<-readWave(df_out$filepath[i],from=0,to=df_out$length_seconds[i], units="seconds")
          results<-ndsi(sound,fft_w=2048,anthro_min=0,anthro_max=2000,bio_min=2000,bio_max=8000)
          if(df_out$n_channels[i]==1){
            df_out$anthrophony[i]<-results$anthrophony_left
            df_out$biophony[i]<-results$biophony_left
          } else {
          df_out$anthrophony[i]<-(results$anthrophony_left+results$anthrophony_right)/2
          df_out$biophony[i]<-(results$biophony_left+results$biophony_right)/2}
        }}
      #GEOPHONIC FILTERS
      for (i in 1:nrow(df_out)) {
        if (!is.na(df_out$filename[i]) & df_out$size_Mb[i]>0) {
          basename<-df_out$filename[i]
          base_output_directory <- '/users/alexandremacphail/desktop/testwav'
          message("******Processing LDFCs for ", df_out$filename[i], ' *******')
          # prepare command
          command <- sprintf('audio2csv "%s" "Towsey.Acoustic.yml" "%s" ', df_out$filepath[i], base_output_directory)
          # finally, execute the command
          system2('/users/alexandremacphail/AP/AnalysisPrograms', command) }}
          files2 <- list.files(base_output_directory, pattern = "*Towsey.Acoustic.Indices.csv",
                             full.names = TRUE, recursive = TRUE)
          indices_summary_files <- lapply(files2, read.csv)
          dfgeo <- do.call(rbind, indices_summary_files)
          rm(indices_summary_files)
          dfgeo[,c("ZeroSignal","ClusterCount","ThreeGramCount","RankOrder","ResultStartSeconds",
                  "SegmentDurationSeconds","ResultMinute")] <- as.numeric(as.character(
                    unlist(dfgeo[,c("ZeroSignal","ClusterCount","ThreeGramCount","RankOrder",
                                   "ResultStartSeconds","SegmentDurationSeconds","ResultMinute")])))
          dfgeo2<-dfgeo %>%
            group_by(FileName) %>%
            dplyr::summarise(AcousticComplexity = mean(AcousticComplexity),
                           SignaltoNoise = mean(Snr),
                           BackgroundNoise = mean(BackgroundNoise),
                           Entropy = mean(EntropyOfAverageSpectrum))
          df_out<-merge(df_out,dfgeo2,by.x="filename",by.y="FileName")
  }
    else if (blocks=='BU_days') {#Do the BU subsampling
      df_out <- df %>%
        group_by(station_key) %>%
        filter(julian %in% c(140:190)) %>%
        filter(time_index==3) %>%
        sample_n(4, replace = FALSE)
      print(paste0(round((nrow(df_out)/nrow(t)),3)*100, '% of the dataset is being subsampled with the BU days design'))
      for (i in 1:nrow(df_out)) {
        if(df_out$size_Mb[i]>0) {
          x<-readWave(df_out$filepath[i],from=0,to=180, units="seconds")
          results<-ndsi(x,fft_w=2048,anthro_min=0,anthro_max=2000,bio_min=2000,bio_max=8000)
          if(df_out$channels[i]==1){
            df_out$anthrophony[i]<-results$anthrophony_left
            df_out$biophony[i]<-results$biophony_left
          } else {
            df_out$anthrophony[i]<-(results$anthrophony_left+results$anthrophony_right)/2
            df_out$biophony[i]<-(results$biophony_left+results$biophony_right)/2}
        }}
    }
    else {#Manually subsampling
      df_out <- df %>%
        group_by(station_key) %>%
        filter(julian %in% c(jd_start:jd_end)) %>%
        filter(time_index %in% time_index) %>%
        dplyr::mutate(new=n()) %>%
        filter(new>sample_size-1) %>%
        sample_n(sample_size,replace = FALSE)
      print(paste0(round((nrow(df_out)/nrow(t)),3)*100, '% of the dataset is being subsampled with your manual design'))
      for (i in 1:nrow(df_out)) {
        if(df_out$size_Mb[i]>0) {
          x<-readWave(df_out$filepath[i],from=0,to=600, units="seconds")
          results<-ndsi(x,fft_w=2048,anthro_min=0,anthro_max=2000,bio_min=2000,bio_max=8000)
          if(df_out$channels[i]==1){
            df_out$anthrophony[i]<-results$anthrophony_left
            df_out$biophony[i]<-results$biophony_left
          } else {
            df_out$anthrophony[i]<-(results$anthrophony_left+results$anthrophony_right)/2
            df_out$biophony[i]<-(results$biophony_left+results$biophony_right)/2}
        }}
    }
  plot1<-ggplot(df_out,aes(x=as.Date(recording_date_time, "%b-%d")),alpha=0.4) +
    geom_point(aes(y=biophony),colour="blue") +
    geom_smooth(aes(y=biophony,colour="blue"),fill="#E3EDFB") +
    geom_point(aes(y=anthrophony),colour="red") +
    geom_smooth(aes(y=anthrophony,colour="red"),fill="#FBE3E3") +
    geom_point(aes(y=AcousticComplexity),colour="blue") +
    geom_smooth(aes(y=AcousticComplexity,colour="blue"),fill="#E3EDFB") +
    geom_point(aes(y=Entropy),colour="forest green") +
    geom_smooth(aes(y=Entropy,colour="forest green"),fill="#E3FBEA") +
    geom_point(aes(y=SignaltoNoise),colour="black") +
    geom_smooth(aes(y=SignaltoNoise,colour="black"),fill="grey") +
    geom_point(aes(y=BackgroundNoise),colour="black") +
    geom_smooth(aes(y=BackgroundNoise,colour="black"),fill="grey") +
    scale_color_identity(name = "Acoustic index family",
                         breaks = c("blue","red","forest green","black"),
                         labels = c("Biophonic", "Anthropophonic", "Geophonic", "Acoustic statistics"),
                         guide="legend") +
    blanktheme +
    ylab("Acoustic index value") +
    xlab("Date") +
    ggtitle(paste0("Acoustic index values for subset of: ",sapply(list(unique(df_out$station_key)),paste,collapse=", ")))
  return(list(df_out,plot1))
}
