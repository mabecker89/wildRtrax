#' Subsets data to select audio recordings for transcription
#'
#' @param df
#' @param blocks
#' @param jd_start
#' @param jd_end
#' @param time_index
#' @param sample_size
#'
#' @import dplyr
#' @return
#' @export
#'
#' @examples

wt_aru_assign <- function(df, blocks = c('ABMI', 'BU_days', 'manual'), jd_start, jd_end, time_index, sample_size){

  if(missing(jd_start)|missing(jd_end)|missing(time_index)|missing(sample_size)) {
    stop("Not gonna work bud!")

  } else {
    if (blocks == 'ABMI') {
      df$blocks <- ifelse(((str_detect(df$Filename, "ABMI")) & (df$JD %in% 90:139) & (df$Time_index %in% 1)), 9,
                        ifelse(((str_detect(df$Filename, "ABMI")) & (df$JD %in% 140:159) & (df$Time_index %in% 1)), 10,
                               ifelse(((str_detect(df$Filename, "ABMI")) & (df$JD %in% 160:179) & (df$Time_index %in% 1)), 11,
                                      ifelse(((str_detect(df$Filename, "ABMI")) & (df$JD %in% 180:210) & (df$Time_index %in% 1)), 12,
                                             ifelse(((str_detect(df$Filename, "ABMI")) & (df$JD %in% 90:104) & (df$Time_index %in% 3)), 1,
                                                    ifelse(((str_detect(df$Filename, "ABMI")) & (df$JD %in% 105:119) & (df$Time_index %in% 4)), 2,
                                                           ifelse(((str_detect(df$Filename, "ABMI")) & (df$JD %in% 120:139) & (df$Time_index %in% 3)), 3,
                                                                  ifelse(((str_detect(df$Filename, "ABMI")) & (df$JD %in% 140:149) & (df$Time_index %in% 3)), 4,
                                                                         ifelse(((str_detect(df$Filename, "ABMI")) & (df$JD %in% 150:159) & (df$Time_index %in% 4)), 5,
                                                                                ifelse(((str_detect(df$Filename, "ABMI")) & (df$JD %in% 160:169) & (df$Time_index %in% 3)), 6,
                                                                                       ifelse(((str_detect(df$Filename, "ABMI")) & (df$JD %in% 170:179) & (df$Time_index %in% 4)), 7,
                                                                                              ifelse(((str_detect(df$Filename, "ABMI")) & (df$JD %in% 180:210) & (df$Time_index %in% 4)), 8,
                                                                                                     NA))))))))))))
      df <- df[complete.cases(df$blocks), ]

      df_out <- df %>%
        group_by(Station,blocks) %>%
        na.omit() %>%
        sample_n(1)

      df_out_validate <- df_out %>%
        group_by(Station) %>%
        dplyr::summarize(count=n())

      df_out_validate[order(df_out_validate$count), ]

      print(df_out_validate)

      valvizJD <- ggplot(df_out, aes(x = Station, y = JD)) + #colour = blocks
        geom_count(aes(colour = Time)) +
        theme_bw() +
        theme(panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        coord_flip() +
        ggsave(paste0("Distrubtion_assign_JD.pdf"))

      print(valvizJD)

      valvizTime <- ggplot(df_out, aes(x=Station,y=Time)) + #colour = blocks
        geom_count(aes(color=JD)) +
        theme_bw() +
        theme(panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        scale_y_discrete(breaks = waiver()) +
        coord_flip() +
        ggsave(paste0("Distrubtion_assign_Time.pdf"))
      print(valvizTime)
    }
    else if (blocks=='BU_days') {

      df_out <- df %>%
        group_by(Station) %>%
        filter(JD %in% c(140:190)) %>%
        filter(Time_index==3) %>%
        sample_n(4, replace = FALSE)

      df_out_validate <- df_out %>%
        group_by(Station) %>%
        dplyr::summarize(count=n())

      df_out_validate[order(df_out_validate$count), ]

      print(df_out_validate)

      valvizJD<-ggplot(df_out, aes(x=Station,y=JD)) + #colour = blocks
        geom_count(aes(colour=Time)) +
        theme_bw() +
        theme(panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        coord_flip() +
        ggsave(paste0("Distrubtion_assign_JD.pdf"))

      print(valvizJD)
      valvizTime<-ggplot(df_out, aes(x=Station,y=Time)) + #colour = blocks
        geom_count(aes(color=JD)) +
        theme_bw() +
        theme(panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        scale_y_discrete(breaks = waiver()) +
        coord_flip() +
        ggsave(paste0("Distrubtion_assign_Time.pdf"))
      print(valvizTime)
    }
    else {
      df_out <- df %>%
        group_by(Station) %>%
        filter(JD %in% c(jd_start:jd_end)) %>%
        filter(Time_index %in% time_index) %>%
        dplyr::mutate(new=n()) %>%
        filter(new>sample_size-1) %>%
        sample_n(sample_size,replace = FALSE)

      df_out_validate <- df_out %>%
        group_by(Station) %>%
        dplyr::summarize(count=n())

      df_out_validate[order(df_out_validate$count), ]

      print(df_out_validate)

      valvizJD <- ggplot(df_out, aes(x = Station, y=JD)) + #colour = blocks
        geom_count(aes(colour=Time)) +
        theme_bw() +
        theme(panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        coord_flip() +
        ggsave(paste0("Distrubtion_assign_JD.pdf"))
      print(valvizJD)
      valvizTime<-ggplot(df_out, aes(x = Station, y = Time)) + #colour = blocks
        geom_count(aes(color=JD)) +
        theme_bw() +
        theme(panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        scale_y_discrete(breaks = waiver()) +
        coord_flip() +
        ggsave(paste0("Distrubtion_assign_Time.pdf"))

      print(valvizTime)

    }
    return(df_out)
  }
}
