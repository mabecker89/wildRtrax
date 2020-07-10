#' Creates symlinks of subsetted raw audio to the appropraite final destination on the server. Operation is quiet - check the final destination folder for more information
#'
#' @param df #wt_aru_assign dataframe output or list of file paths
#' @import base
#'
#' @examples x<-wt_link(df)
#'

wt_link<-function(df) {
  for (i in 1:nrow(df)) {
    base_output_directory[i] <- paste0('/Volumes/BUdata/wildtrax_loads','/',basename(df$filepath[i]))
    message('*** Creating symlink to ', base_output_directory[i], ' ***')
    # prepare command
    command <- sprintf(' sudo ln -s "%s" "%s" ', df$filepath[i], base_output_directory[i])
    # finally, execute the command
    link<-tryCatch(system2(command),
                   error=function(e){
                     msg<-conditionMessage(e)
                     print(paste0(msg, ' for ', df$filepath[i]))
                   })
  }}

wt_link()
