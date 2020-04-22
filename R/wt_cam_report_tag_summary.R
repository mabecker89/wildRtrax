#' Retrieve tagged image data from camera project
#'
#' @param conn WildTrax database connection (PostgreSQLConnection object)
#' @param project_id Integer; the project ID
#' @param native_only Logical; set to TRUE to retrieve tagged data on native species
#' @importFrom glue glue_sql
#' @importFrom DBI dbSendQuery dbFetch
#' @export
#' @examples
#' \donttest{
#' connection <- wt_con(username = "", password = ")
#' id <- project_id # set project id as integer
#' camera_summary <- wt_cam_report_tag_summary(conn = connection, project_id = id, native_only = TRUE)
#' }
#' @return This function returns a dataframe of tagged camera image data from the project specified in project_id

wt_cam_report_tag_summary <- function(conn, project_id, native_only = TRUE) {

  # Compose SQL query
  if(native_only == TRUE) {
    query <- glue_sql("SELECT * FROM camera.get_report_tag_summary(array[{project}]) WHERE common_name IN ({native_species*})",
                      project = project_id,
                      native_species = native_sp,
                      .con = conn)
  } else {
    query <- glue_sql("SELECT * FROM camera.get_report_tag_summary(array[{project}])",
                      project = project_id,
                      .con = conn)
  }

  # Send query to WildTrax
  send_query <- dbSendQuery(conn = conn, statement = query)

  # Fetch result
  dbFetch(send_query)

}



