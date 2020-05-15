#' Subsets data to select audio recordings for transcription. Input is the result of the
#' wt_aru_scanner used to get a full list of the audio data and metadata attributes to select from
#'
#' @param df #wt_aru_scanner dataframe output
#' @param subsample_templates # choose from a variety of subsampling techniques include the ABMI stratified sampling design or the BU standard dawn chorus.
#' manual overrides and let you select through the remaining fields below
#' @param jd_start #date to start with
#' @param jd_end #date to end with
#' @param time_index
#' @param sample_size
#'
#' @import tidyverse stringr soundecology
#' @return df_out
#' @export
#'
#' @examples

library(soundecology)

wt_aru_assign <- function(df, blocks = c('ABMI_stratified', 'BU_days', 'manual'), jd_start, jd_end, time_index, sample_size){

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
      print(paste0(round((nrow(df_out)/nrow(t)),3)*100, '% of the dataset is being subsampled with ABMI stratified design'))
      df_out$Filepath<-as.character(df_out$Filepath)
      
     #Run preliminary check of the audio data
      for (i in 1:nrow(df_out)) {
        if(df_out$size[i]>0) {
          x<-readWave(df_out$Filepath[i],from=0,to=Inf, units="seconds")
          results<-acoustic_complexity(x,min_freq=0,max_freq=8000)
          summary(results)
      }}
      # Section for acoustic indices for weather is broken
      # # Set the directory containing the files
      # setwd('/users/alexandremacphail/AP2')
      #
      # # The directory to store the results
      # base_output_directory <- "/users/alexandremacphail/desktop/ap2/wildRtrax/results"
      #
      # # Get a list of audio files inside the directory
      # # (Get-ChildItem is just like ls, or dir)
      # files <- unlist(df_out$Filepath)
      #
      # # iterate through each file
      # for(file in files) {
      #   message("Processing ", file)
      #
      #   # get just the name of the file
      #   file_name <- basename(file)
      #
      #   # make a folder for results
      #   output_directory <- normalizePath(file.path(base_output_directory, file_name))
      #   dir.create(output_directory, recursive = TRUE)
      #
      #   # prepare command
      #   command <- sprintf('audio2csv "%s" "Towsey.Acoustic.yml" "%s" ', '/users/alexandremacphail/desktop/RS-0031-NW_20190627_020000-old1.wav', '/users/alexandremacphail/desktop/ap2/wildRtrax/results')
      #
      #   # finally, execute the command
      #   system2('/users/alexandremacphail/AP2/AnalysisPrograms.exe', command)
      #}

  }
    else if (blocks=='BU_days') {
      df_out <- df %>%
        group_by(Station_key) %>%
        filter(Julian_Date %in% c(140:190)) %>%
        filter(Time_index==3) %>%
        sample_n(4, replace = FALSE)

      print(paste0(round((nrow(df_out)/nrow(t)),3)*100, '% of the dataset is being subsampled with BU days design'))

    }
    else {
      df_out <- df %>%
        group_by(Station_key) %>%
        filter(Julian_Date %in% c(jd_start:jd_end)) %>%
        filter(Time_index %in% time_index) %>%
        dplyr::mutate(new=n()) %>%
        filter(new>sample_size-1) %>%
        sample_n(sample_size,replace = FALSE)

      print(paste0(round((nrow(df_out)/nrow(t)),3)*100, '% of the dataset is being subsampled with your manual design'))
    }
    return(df_out)
}

wt_aru_assign(t %>% filter(!type=='wac'),blocks="ABMI_stratified")

