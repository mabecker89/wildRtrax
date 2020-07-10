#' Uses subsampled dataframe and create the csvs used to assign tasks to users. A task in WildTrax is a unique combination of audio recording, user and method.
#'
#' @param df Dataframe from the scanner or assign tools
#' @param rename Rename the final csv on your local machine; keeps the original dataframe name by default
#' @param users Can be a list or range of values. User 1, User 2, etc. Don't forget: user must be already added to the project in WildTrax before they can be assigned to a task
#' @param project Name of the project. Must match what is on WildTrax
#' @param prop List or range of ordered probabilities to assign to the users; will clean this up in the future so it isn't ordinal
#' @param method Method used to transcribe the task
#'
#' @import stringr base
#' @return
#' @export
#'
#' @examples
#'
#'
library(tibble)


batch_upload_dfs <-
  function(df, rename = FALSE, users, project, prop, method) {

    #Step 1 Create the recordings dataframe. This is required by the WildTrax system as minimum metadata for
    df1 <- as.data.frame(df[, c(1)])
    colnames(df1)[1] <- 'File'
    df1$File <- ifelse(
      str_detect(df1$File, '(Contracts)'),
      sub("/volumes/BUdata/", "/media/BUdata02/", df1$File),
      sub("/volumes/BUdata/", "/media/BUdata01/", df1$File)
    )
    df1$Dataset <- sub('\\-(.*)', '', basename(df1$File))
    df1$Site <- gsub('\\-', '', str_match(basename(df1$File), '-(.*?)-'))
    df1$Station <-
      sub('.*-', '', str_match(basename(df1$File), '.+?(?=_)'[1]))
    df1$Profile <- ''
    if (rename = FALSE) {
      write.csv(df1, paste0(as.character(df1$Dataset[1]), '-1.csv'), row.names =
                  FALSE)
    } else {
      write.csv(df1, paste0(as.character(rename), '-1.csv'), row.names = FALSE)
    }


    #Create the tasks dataframe

    df2 <- as.data.frame(df[, c(3)])
    colnames(df2)[1] <- 'Filename'
    df2$Dataset <- sub('\\-(.*)', '', df2$Filename)
    df2$Site <- gsub('\\-', '', str_match(basename(df2$File), '-(.*?)-'))
    df2$Station <- sub('.*-', '', str_match(df2$File, '.+?(?=_)'[1]))
    df2$Date <- ifelse(
      str_detect(df2$Filename, '(_0\\+1_)'),
      str_match(df2$Filename, '^(.*?\\_.*?)\\_.*'),
      str_match(df2$Filename, '(?<=\\_).*$')
    )
    df2$Project <-
      as.character(paste0(project)) #Change project name accordingly
    people <- users # Change transcribers accordingly
    df2$User <-
      sample(people, nrow(df2), prob = prop, replace = TRUE) #Change proportion accordingly
    df2$Method <- as.character(method)
    if (rename = FALSE) {
      write.csv(df2, paste0(as.character(df1$Dataset[1]), '-2.csv'), row.names =
                  FALSE)
    } else {
      write.csv(df2, paste0(as.character(rename), '-2.csv'), row.names = FALSE)
    }
}



users0<-c("")
prop0<-c(0.2,0.25,0.2,0.05,0.1,0.1,0.1)
avg_dissimilarity<-c(0.92,0.23,0.33,0.41,0.72)

z2<-z
z2<-z2 %>%
  group_by(user) %>%
  summarise(count=n()) %>%
  mutate(perc=count/sum(count)) %>%
  mutate(perc_ad=perc*avg_dissimilarity)

z2$user<-sample(users, nrow(z2), prob=prop, replace=T)
ggplot(melt(z2),aes(x=user,y=perc_ad)) +
  geom_bar(stat="identity") +
  geom_bar(aes(y=perc),stat="identity") +
  blanktheme

batch_upload_dfs(z,"upsi2019",users=users0,prop=prop0,method="5M 1SPM")

z1 <- as.data.frame(z[, c(1)])
colnames(z1)[1] <- 'File'
z1$File <- ifelse(
  str_detect(z1$File, '(Contracts)'),
  sub("/volumes/BUdata/", "/media/BUdata02/", z1$File),
  sub("/volumes/BUdata/", "/media/BUdata01/", z1$File)
)
z1$Dataset <- sub('\\-(.*)', '', basename(z1$File))
z1$Site <- gsub('\\-', '', str_match(basename(z1$File), '-(.*?)-'))
z1$Station <-
  sub('.*-', '', str_match(basename(z1$File), '.+?(?=_)'[1]))
z1$Profile <- ''
write.csv(z1, paste0(as.character("UPSI2019"), '-1.csv'), row.names = FALSE)

z2 <- as.data.frame(z[, c(3)])
colnames(z2)[1] <- 'Filename'
z2$Dataset <- sub('\\-(.*)', '', z2$Filename)
z2$Site <- gsub('\\-', '', str_match(basename(z2$File), '-(.*?)-'))
z2$Station <- sub('.*-', '', str_match(z2$File, '.+?(?=_)'[1]))
z2$Date <- ifelse(
  str_detect(z2$Filename, '(_0\\+1_)'),
  str_match(z2$Filename, '^(.*?\\_.*?)\\_.*'),
  str_match(z2$Filename, '(?<=\\_).*$')
)
z2$Project <- 'Upper Peace Songbird Inventory AEP Upper Peace Region 2019' #Change project name accordingly
people <- users # Change transcribers accordingly
z2$User <-
  sample(people, nrow(z2), prob = prop, replace = TRUE) #Change proportion accordingly
z2$Method <- as.character("5M 1SPM")
write.csv(z2, paste0(as.character("UPSI2019"), '-2.csv'), row.names = FALSE)


