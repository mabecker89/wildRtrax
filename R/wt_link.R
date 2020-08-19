#' Creates symlinks of subsetted raw audio to the appropraite final destination on the server. Operation is quiet - check the final destination folder for more information
#'
#' @param df #wt_aru_assign dataframe output or list of file paths
#' @import base
#'
#' @examples x<-wt_link(df)
#'

t<-read.csv('/users/alexandremacphail/desktop/gbm_fill_test.csv')
t$filepath<-as.character(t$filepath)


wt_link<-function(df) {
  for (i in 1:nrow(df)) {
    message('*** Creating symlink to ', paste0('/Volumes/BUdata/wildtrax_loads','/', basename(df$filepath[i])), ' ***')
    # prepare command
    command <- sprintf('sudo ln -s "%s" "%s"', df$filepath[i], paste0('/Volumes/BUdata/wildtrax_loads','/', basename(df$filepath[i])))
    # finally, execute the command
    link<-tryCatch(system2(command),
                   error=function(e){
                     msg<-conditionMessage(e)
                     print(paste0(msg, ' for ', df$filepath[i]))
                   })
  }}

wt_link(t)
