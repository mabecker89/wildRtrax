#' Subsets data to select audio recordings for transcription. Input is the result of the
#' wt_aru_scanner used to get a full list of the audio data and metadata attributes to select from
#'
#' @param df #wt_aru_scanner dataframe output
#' @param subsample_templates # choose from a variety of subsampling techniques include the ABMI stratified sampling design or the BU standard dawn chorus.
#' manual overrides and let you select through the remaining fields below
#' @param jd_start #date to start with
#' @param jd_end #date to end with
#' @param time_index #Range of time indices that can be selected e.g. c(1:4)
#' @param sample_size #Number of replicates using the date and time choices per spatial location
#'
#' @import tidyverse stringr soundecology
#' @return df_out plot1
#' @export
#'
#' @examples x<-wt_aru_assign(z, blocks="manual", jd_start=140, jd_end=190, time_index=c(1:2), sample_size=1)

wt_aru_assign <- function(df, blocks = c('ABMI_stratified', 'BU_days', 'manual'), jd_start, jd_end, time_index, sample_size) {

  #Start by creating a "block tower" basically assign each file into each stratified block
  if (blocks == 'ABMI_stratified') {
    df$blocks <- ifelse(df$Julian_Date %in% 90:139 & df$Time_index %in% 1, 9,
                        ifelse(df$Julian_Date %in% 140:159 & df$Time_index %in% 1, 10,
                               ifelse(df$Julian_Date %in% 160:179 & df$Time_index %in% 1, 11,
                                      ifelse(df$Julian_Date %in% 180:210 & df$Time_index %in% 1, 12,
                                             ifelse(df$Julian_Date %in% 90:104 & df$Time_index %in% 3, 1,
                                                    ifelse(df$Julian_Date %in% 105:119 & df$Time_index %in% 4, 2,
                                                           ifelse(df$Julian_Date %in% 120:139 & df$Time_index %in% 3, 3,
                                                                  ifelse(df$Julian_Date %in% 140:149 & df$Time_index %in% 3, 4,
                                                                         ifelse(df$Julian_Date %in% 150:159 & df$Time_index %in% 4, 5,
                                                                                ifelse(df$Julian_Date %in% 160:169 & df$Time_index %in% 3, 6,
                                                                                       ifelse(df$Julian_Date %in% 170:179 & df$Time_index %in% 4, 7,
                                                                                              ifelse(df$Julian_Date %in% 180:210 & df$Time_index %in% 4, 8,
                                                                                                     NA))))))))))))
      #Subsample
    df <- df[complete.cases(df$blocks), ]
      df_out <- df %>%
        group_by(Station_key,blocks) %>%
        na.omit() %>%
        sample_n(1)

      files <- unlist(df_out$Filepath)
      print(paste0(round((nrow(df_out)/nrow(t)),3)*100, '% of the dataset is being subsampled with the ABMI stratified design'))
      df_out$Filepath<-as.character(df_out$Filepath)

     #Run preliminary check of the audio data for anthropophonic data
      for (i in 1:nrow(df_out)) {
        if(df_out$size[i]>0) {
          x<-readWave(df_out$Filepath[i],from=0,to=180, units="seconds")
          results<-ndsi(x,fft_w=2048,anthro_min=0,anthro_max=2000,bio_min=2000,bio_max=8000)
          if(df_out$channels[i]==1){
            df_out$anthrophony[i]<-results$anthrophony_left
            df_out$biophony[i]<-results$biophony_left
          } else {
          df_out$anthrophony[i]<-(results$anthrophony_left+results$anthrophony_right)/2
          df_out$biophony[i]<-(results$biophony_left+results$biophony_right)/2}
        }}

  }
    else if (blocks=='BU_days') {#Do the BU subsampling
      df_out <- df %>%
        group_by(Station_key) %>%
        filter(Julian_Date %in% c(140:190)) %>%
        filter(Time_index==3) %>%
        sample_n(4, replace = FALSE)
      print(paste0(round((nrow(df_out)/nrow(t)),3)*100, '% of the dataset is being subsampled with the BU days design'))

      for (i in 1:nrow(df_out)) {
        if(df_out$size[i]>0) {
          x<-readWave(df_out$Filepath[i],from=0,to=180, units="seconds")
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
        group_by(Station_key) %>%
        filter(Julian_Date %in% c(jd_start:jd_end)) %>%
        filter(Time_index %in% time_index) %>%
        dplyr::mutate(new=n()) %>%
        filter(new>sample_size-1) %>%
        sample_n(sample_size,replace = FALSE)

      print(paste0(round((nrow(df_out)/nrow(t)),3)*100, '% of the dataset is being subsampled with your manual design'))

      for (i in 1:nrow(df_out)) {
        if(df_out$size[i]>0) {
          x<-readWave(df_out$Filepath[i],from=0,to=180, units="seconds")
          results<-ndsi(x,fft_w=2048,anthro_min=0,anthro_max=2000,bio_min=2000,bio_max=8000)
          if(df_out$channels[i]==1){
            df_out$anthrophony[i]<-results$anthrophony_left
            df_out$biophony[i]<-results$biophony_left
          } else {
            df_out$anthrophony[i]<-(results$anthrophony_left+results$anthrophony_right)/2
            df_out$biophony[i]<-(results$biophony_left+results$biophony_right)/2}
        }}
    }
  plot1<-ggplot(as.data.frame(x[[1]]),aes(x=Julian_Date)) +
    geom_point(aes(y=biophony),colour="forest green") +
    geom_smooth(aes(y=biophony),colour="forest green") +
    geom_hline(yintercept = 0.3, size=1, color="forest green", linetype="dashed") +
    geom_text(aes(x=min(Julian_Date),y=0.3),label="Biophonic threshold", vjust=-1, hjust=-1) +
    geom_point(aes(y=anthrophony),colour="red") +
    geom_smooth(aes(y=anthrophony),colour="red") +
    geom_hline(yintercept = 0.75, size=1, color="red", linetype="dashed") +
    geom_text(aes(x=min(Julian_Date),y=0.75),label="Anthropophonic threshold", vjust=-1, hjust=-1) +
    blanktheme +
    ylab("Acoustic index value")

    return(list(df_out,plot1))
}



