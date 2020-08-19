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
plan(multisession)
registerDoParallel(cl <- makeCluster(4))

wt_aru_assign <-
  function(df, blocks = c('ABMI_stratified', 'BU_days', 'manual'), jd_start, jd_end, time_index, sample_size) {
    #Start by creating a "block tower" basically assign each file into each stratified block. If it doesn't fit the block, value will be NA and subsequently filtered out
    if (blocks == 'ABMI_stratified') {
      #Create the lookup table
      abmi <- data.frame(julian = 90:210) %>%
        crossing(time_index = 1:4) %>%
        mutate(
          blocks = case_when(
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
          )
        )
      df <- merge(df, abmi, by = c("julian", "time_index"))
      #Subsample
      df <- df[complete.cases(df$blocks),]
      df_out <- df %>%
        group_by(location, blocks) %>%
        na.omit() %>%
        sample_n(4, replace = T)
      df_out <- df
      files <- unlist(df_out$filepath)
      print(
        paste0(
          round((nrow(df_out) / nrow(df)), 3) * 100,
          '% of the dataset is being subsampled with the ABMI stratified design'
        )
      )
      df_out$filepath <- as.character(df_out$filepath)
      #BIOPHONIC AND ANTHROPOPHONIC FILTERS
      # for (i in 1:nrow(df_out)) {
      #   if (df_out$size_Mb[i] > 0) {
      #     sound <- tryCatch(
      #       if (str_extract(df_out$filepath[i], '.{3}$') == 'wac') {
      #         read_wac(df_out$filepath[i])
      #       } else if (str_extract(df_out$filepath[i], '.{3}$') == 'wav') {
      #         readWave(
      #           df_out$filepath[i],
      #           from = 0,
      #           to = df_out$length_seconds[i],
      #           units = "seconds"
      #         )
      #       } else {
      #         print('Could not read or source not valid')
      #       },
      #       error = function(e) {
      #         msg <- conditionMessage(e)
      #         print(paste0(msg, df_out$filepath[i], sep = ' '))
      #       }
      #     )
      #     results <-
      #       ndsi(
      #         sound,
      #         fft_w = 512,
      #         anthro_min = 0,
      #         anthro_max = 2000
      #       )
      #     if (df_out$stereo[i] == 1) {
      #       df_out$anthrophony[i] <- results$anthrophony_left
      #       df_out$biophony[i] <- results$biophony_left
      #     } else {
      #       df_out$anthrophony[i] <-
      #         (results$anthrophony_left + results$anthrophony_right) / 2
      #       df_out$biophony[i] <-
      #         (results$biophony_left + results$biophony_right) / 2
      #     }
      #   }
      # }
      #GEOPHONIC FILTERS
      foreach (i = 1:nrow(df_out)) %dopar% {
        if (!is.na(df_out$filename[i]) & df_out$size_Mb[i] > 0) {
          basename <- df_out$filename[i]
          base_output_directory <- '/users/alexandremacphail/blpw'
          message("******Processing LDFCs for ", df_out$filename[i], ' *******')
          # prepare command
          command <- sprintf('audio2csv "%s" "Towsey.Acoustic.yml" "%s" ', df_out$filepath[i], base_output_directory)
          # finally, execute the command
          system2('/users/alexandremacphail/AP/AnalysisPrograms', command)
        }
      }
      files2 <-
        list.files(
          '/users/alexandremacphail/blpw/Towsey.Acoustic',
          pattern = "*Towsey.Acoustic.Indices.csv",
          full.names = TRUE,
          recursive = TRUE
        )
      indices_summary_files <- lapply(files2, read.csv)
      dfgeo <- do.call(rbind, indices_summary_files)
      rm(indices_summary_files)
      dfgeo2 <- dfgeo %>%
        group_by(FileName) %>%
        summarise(
          AcousticComplexity = mean(AcousticComplexity),
          SignaltoNoise = mean(Snr),
          BackgroundNoise = mean(BackgroundNoise),
          Temporal_Entropy = mean(TemporalEntropy),
          Entropy_average = mean(EntropyOfAverageSpectrum),
          NDSI = mean(Ndsi)
        )
      df_out <- merge(df_out, dfgeo2, by.x = "filename", by.y = "FileName")
    }
    else if (blocks == 'BU_days') {
      #Do the BU subsampling
      df_out <- df %>%
        filter(julian %in% c(64:115)) %>%
        filter(time_index == c(1:27)) #%>%
      #sample_n(12, replace = FALSE)
      df_out$filepath <- as.character(df_out$filepath)
      print(
        paste0(
          round((nrow(df_out) / nrow(df)), 3) * 100,
          '% of the dataset is being subsampled with the BU days stratified design'
        )
      )
      #BIOPHONIC AND ANTHROPOPHONIC FILTERS
      for (i in 1:nrow(df_out)) {
        if (df_out$size_Mb[i] > 0) {
          sound <-
            readWave(
              df_out$filepath[i],
              from = 0,
              to = df_out$length_seconds[i],
              units = "seconds"
            )
          results <-
            ndsi(
              sound,
              fft_w = 2048,
              anthro_min = 0,
              anthro_max = 500,
              bio_min = 500,
              bio_max = 1500
            )
          if (df_out$stereo[i] == 1) {
            df_out$anthrophony[i] <- results$anthrophony_left
            df_out$biophony[i] <- results$biophony_left
          } else {
            df_out$anthrophony[i] <-
              (results$anthrophony_left + results$anthrophony_right) / 2
            df_out$biophony[i] <-
              (results$biophony_left + results$biophony_right) / 2
          }
        }
      }
      #GEOPHONIC FILTERS
      for (i in 1:nrow(df_out)) {
        if (!is.na(df_out$filename[i]) & df_out$size_Mb[i] > 0) {
          basename <- df_out$filename[i]
          base_output_directory <-
            '/users/alexandremacphail/desktop/testwav'
          message("******Processing LDFCs for ",
                  df_out$filename[i],
                  ' *******')
          # prepare command
          command2 <-
            sprintf(
              'audio2csv "%s" "Towsey.Acoustic.yml" "%s" ',
              df_out$filepath,
              base_output_directory
            )
          # finally, execute the command
          system2('/users/alexandremacphail/AP/AnalysisPrograms',
                  command)
        }
      }
      files2 <-
        list.files(
          base_output_directory,
          pattern = "*Towsey.Acoustic.Indices.csv",
          full.names = TRUE,
          recursive = TRUE
        )
      indices_summary_files <- lapply(files2, read.csv)
      dfgeo <- do.call(rbind, indices_summary_files)
      rm(indices_summary_files)
      dfgeo[, c(
        "ZeroSignal",
        "ClusterCount",
        "ThreeGramCount",
        "RankOrder",
        "ResultStartSeconds",
        "SegmentDurationSeconds",
        "ResultMinute"
      )] <-
        as.numeric(as.character(unlist(dfgeo[, c(
          "ZeroSignal",
          "ClusterCount",
          "ThreeGramCount",
          "RankOrder",
          "ResultStartSeconds",
          "SegmentDurationSeconds",
          "ResultMinute"
        )])))
      dfgeo2 <- dfgeo %>%
        group_by(FileName) %>%
        dplyr::summarise(
          AcousticComplexity = mean(AcousticComplexity),
          SignaltoNoise = mean(Snr),
          BackgroundNoise = mean(BackgroundNoise),
          Entropy = mean(EntropyOfAverageSpectrum)
        )
      df_out <-
        merge(df_out, dfgeo2, by.x = "filename", by.y = "FileName")
    }
    else if (blocks == 'manual') {
      #Manually subsampling
      df_out <- df %>%
        group_by(location) %>%
        filter(julian %in% c(jd_start:jd_end)) %>%
        filter(time_index %in% time_index) %>%
        dplyr::mutate(new = n()) %>%
        filter(new > sample_size - 1) %>%
        sample_n(sample_size, replace = FALSE)
      print(paste0(
        round((nrow(df_out) / nrow(df)), 3) * 100,
        '% of the dataset is being subsampled with your manual design'
      ))
      #BIOPHONIC AND ANTHROPOPHONIC FILTERS
      for (i in 1:nrow(df_out)) {
        if (df_out$size_Mb[i] > 0) {
          sound <-
            readWave(
              df_out$filepath[i],
              from = 0,
              to = df_out$length_seconds[i],
              units = "seconds"
            )
          results <-
            ndsi(
              sound,
              fft_w = 2048,
              anthro_min = 0,
              anthro_max = 2000,
              bio_min = 2000,
              bio_max = 8000
            )
          if (df_out$stereo[i] == 1) {
            df_out$anthrophony[i] <- results$anthrophony_left
            df_out$biophony[i] <- results$biophony_left
          } else {
            df_out$anthrophony[i] <-
              (results$anthrophony_left + results$anthrophony_right) / 2
            df_out$biophony[i] <-
              (results$biophony_left + results$biophony_right) / 2
          }
        }
      }
      #GEOPHONIC FILTERS
      for (i in 1:nrow(df_out)) {
        if (!is.na(df_out$filename[i]) & df_out$size_Mb[i] > 0) {
          basename <- df_out$filename[i]
          base_output_directory <-
            '/users/alexandremacphail/testcars'
          message("******Processing LDFCs for ",
                  df_out$filename[i],
                  ' *******')
          # prepare command
          command2 <-
            sprintf(
              'audio2csv "%s" "Towsey.Acoustic.yml" "%s" ',
              df_out$filepath,
              base_output_directory
            )
          # finally, execute the command
          system2('/users/alexandremacphail/AP/AnalysisPrograms',
                  command2)
        }
      }
      files2 <-
        list.files(
          base_output_directory,
          pattern = "*Towsey.Acoustic.Indices.csv",
          full.names = TRUE,
          recursive = TRUE
        )
      indices_summary_files <- lapply(files2, read.csv)
      dfgeo <- do.call(rbind, indices_summary_files)
      rm(indices_summary_files)
      dfgeo[, c(
        "ZeroSignal",
        "ClusterCount",
        "ThreeGramCount",
        "RankOrder",
        "ResultStartSeconds",
        "SegmentDurationSeconds",
        "ResultMinute"
      )] <-
        as.numeric(as.character(unlist(dfgeo[, c(
          "ZeroSignal",
          "ClusterCount",
          "ThreeGramCount",
          "RankOrder",
          "ResultStartSeconds",
          "SegmentDurationSeconds",
          "ResultMinute"
        )])))
      dfgeo2 <- dfgeo %>%
        group_by(FileName) %>%
        dplyr::summarise(
          AcousticComplexity = mean(AcousticComplexity),
          SignaltoNoise = mean(Snr),
          BackgroundNoise = mean(BackgroundNoise),
          Entropy = mean(EntropyOfAverageSpectrum)
        )
      df_out <-
        merge(df_out, dfgeo2, by.x = "filename", by.y = "FileName")
    }
    else {
      df_out <- df
      files <- unlist(df_out$filepath)
      print(paste0(
        round((nrow(df_out) / nrow(df)), 3) * 100,
        '% of the dataset is being subsampled with the full design'
      ))
      df_out$filepath <- as.character(df_out$filepath)
      #PARALLELIZE
      # df_out <- df_out %>%
      #   group_by(julian) %>%
      #   mutate(
      #     sound = future_map(.x = filepath, .f = ~ readWave(.x, from = 0, to = length_seconds, units = "seconds", header = T), .progress = T),
      #     results = future_map(.x = sound, .f = ~ ndsi(.x, fft_w = 2048, anthro_min = 0, anthro_max = 2000, bio_min = 2000, bio_max = 10000), .progress = T),
      #     anthrophopny = future_map(.x = results, .p = stereo = 2, .f = ~ pluck((.x$anthrophony_left + .x$anthrophony_right) / 2), .else = ~ pluck((.x$anthrophony_left)), .progress = F),
      #     biophony = futute_map(.x = results, .p = stereo = 2, .f = ~ pluck((.x$biophony_left + .x$biophony_right) / 2), .else = ~ pluck((.x$biophony_left)), .progress = F)
      #   ) %>%
      #   select(filename, filepath, size_Mb, location, recording_date_time, year, julian, time_index, ftype, length_seconds, sample_rate, stereo, anthrophony, biophony) %>%
      #   ungroup()
      #GEOPHONIC FILTERS
      foreach (i = 1:nrow(df_out)) %dopar% {
        if (!is.na(df_out$filename[i]) & df_out$size_Mb[i] > 0) {
          basename <- df_out$filename[i]
          base_output_directory <- '/users/alexandremacphail/blpw'
          message("******Processing LDFCs for ", df_out$filename[i], ' *******')
          # prepare command
          command <- sprintf('audio2csv "%s" "Towsey.Acoustic.yml" "%s" ', df_out$filepath[i], base_output_directory)
          # finally, execute the command
          system2('/users/alexandremacphail/AP/AnalysisPrograms', command)
        }
      }
      files2 <-
        list.files(
          '/users/alexandremacphail/blpw/Towsey.Acoustic',
          pattern = "*Towsey.Acoustic.Indices.csv",
          full.names = TRUE,
          recursive = TRUE
        )
      indices_summary_files <- lapply(files2, read.csv)
      dfgeo <- do.call(rbind, indices_summary_files)
      rm(indices_summary_files)
      dfgeo2 <- dfgeo %>%
        group_by(FileName) %>%
        summarise(
          Anthrophony = mean(anthrophony),
          Biophony = mean(biophony),
          AcousticComplexity = mean(AcousticComplexity),
          SignaltoNoise = mean(Snr),
          BackgroundNoise = mean(BackgroundNoise),
          Temporal_Entropy = mean(TemporalEntropy),
          Entropy_average = mean(EntropyOfAverageSpectrum),
          NDSI = mean(Ndsi)
        )
      df_out <- merge(df_out, dfgeo2, by.x = "filename", by.y = "FileName")
    }
    plot1 <-
      ggplot(df_out, aes(x = as.Date(recording_date_time, "%b-%d")), alpha = 0.4) +
      geom_point(aes(y = biophony), colour = "blue") +
      geom_smooth(aes(y = biophony, colour = "blue"), fill = "#E3EDFB") +
      geom_point(aes(y = anthrophony), colour = "red") +
      geom_smooth(aes(y = anthrophony, colour = "red"), fill = "#FBE3E3") +
      geom_point(aes(y = NDSI), colour = "blue") +
      geom_smooth(aes(y = NDSI, colour = "blue"), fill = "#E3EDFB") +
      geom_point(aes(y = AcousticComplexity), colour = "blue") +
      geom_smooth(aes(y = AcousticComplexity, colour = "blue"), fill = "#E3EDFB") +
      geom_point(aes(y = Temporal_Entropy), colour = "forest green") +
      geom_smooth(aes(y = Temporal_Entropy, colour = "forest green"), fill = "#E3FBEA") +
      geom_point(aes(y = Entropy_average), colour = "forest green") +
      geom_smooth(aes(y = Entropy_average, colour = "forest green"), fill = "#E3FBEA") +
      geom_point(aes(y = SignaltoNoise), colour = "black") +
      geom_smooth(aes(y = SignaltoNoise, colour = "black"), fill = "grey") +
      geom_point(aes(y = BackgroundNoise), colour = "black") +
      geom_smooth(aes(y = BackgroundNoise, colour = "black"), fill = "grey") +
      scale_color_identity(
        name = "Acoustic index family",
        breaks = c("blue", "forest green", "black"), #red
        labels = c(
          "Biophonic",
          "Geophonic",
          "Acoustic statistics"
          #Anthropophonic
        ),
        guide = "legend"
      ) +
      blanktheme +
      ylab("Acoustic index value") +
      xlab("Date") +
      ggtitle(paste0(
        "Acoustic index values for subset of: ",
        sapply(list(unique(df_out$location)), paste, collapse = ", ")
      ))
    pics <- list.files("/users/alexandremacphail/blpw/Towsey.Acoustic","\\__2Maps.png$", recursive = T, full.names = T)
    images <- image_read(pics)
    images <- image_append(images)
    r <- image_write(images, "MERGED_BLPW2.png")
    r
    dev.off()
    return(list(df_out, plot1))
  }


plot2 <-
  ggplot(blpw_s[[1]], aes(x = as.Date(recording_date_time, "%b-%d")), alpha = 0.4) +
  # geom_point(aes(y = biophony), colour = "blue") +
  # geom_smooth(aes(y = biophony, colour = "blue"), fill = "#E3EDFB") +
  # geom_point(aes(y = anthrophony), colour = "red") +
  # geom_smooth(aes(y = anthrophony, colour = "red"), fill = "#FBE3E3") +
  geom_point(aes(y = NDSI), colour = "blue") +
  geom_smooth(aes(y = NDSI, colour = "blue"), fill = "#E3EDFB") +
  geom_point(aes(y = AcousticComplexity), colour = "blue") +
  geom_smooth(aes(y = AcousticComplexity, colour = "blue"), fill = "#E3EDFB") +
  geom_point(aes(y = Temporal_Entropy), colour = "forest green") +
  geom_smooth(aes(y = Temporal_Entropy, colour = "forest green"), fill = "#E3FBEA") +
  geom_point(aes(y = Entropy_average), colour = "forest green") +
  geom_smooth(aes(y = Entropy_average, colour = "forest green"), fill = "#E3FBEA") +
  # geom_point(aes(y = SignaltoNoise), colour = "black") +
  # geom_smooth(aes(y = SignaltoNoise, colour = "black"), fill = "grey") +
  # geom_point(aes(y = BackgroundNoise), colour = "black") +
  # geom_smooth(aes(y = BackgroundNoise, colour = "black"), fill = "grey") +
  scale_color_identity(
    name = "Acoustic index family",
    breaks = c("blue", "forest green"), #red
    labels = c(
      "Biophonic",
      "Geophonic"
    ),
    guide = "legend"
  ) +
  blanktheme +
  ylab("Acoustic index value") +
  xlab("Date")
l_g<-ggplotly(plot2,dynamicTicks=T)
l_g
plot2




blpw_try<-blpw_r %>%
  filter(time_index==3)

tic()
blpw_s <- wt_aru_assign(blpw_r, blocks = '')
toc()




#BIOPHONIC AND ANTHROPOPHONIC FILTERS
# for (i in 1:nrow(df_out)) {
#   if (df_out$size_Mb[i] > 0) {
#     sound <-
#       readWave(
#         df_out$filepath[i],
#         from = 0,
#         to = df_out$length_seconds[i],
#         units = "seconds"
#       )
#     results <-
#       ndsi(
#         sound,
#         fft_w = 2048,
#         anthro_min = 0,
#         anthro_max = 500,
#         bio_min = 500,
#         bio_max = 10000
#       )
#     if (df_out$stereo[i] == 1) {
#       df_out$anthrophony[i] <- results$anthrophony_left
#       df_out$biophony[i] <- results$biophony_left
#     } else {
#       df_out$anthrophony[i] <-
#         (results$anthrophony_left + results$anthrophony_right) / 2
#       df_out$biophony[i] <-
#         (results$biophony_left + results$biophony_right) / 2
#     }
#   }


tl <- read.csv('/users/alexandremacphail/AP5/Towsey.Acoustic/IVNP-SC-0T_20180606_040000__Towsey.Acoustic.ACI.csv')
tl <- tl %>%
  remove_rownames() %>%
  column_to_rownames(var = "Index")
names(tl) = gsub("c", "", x = names(tl))
names(tl) = as.numeric(names(tl))
tl <- as.data.frame(t(tl))
tl <- tl %>%
  transmute_all(mean) %>%
  distinct()
