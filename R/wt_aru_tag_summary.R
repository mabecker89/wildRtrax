#' Retrieve tagged image data from camera project
#'
#' @param conn WildTrax database connection (PostgreSQLConnection object)
#' @param proj_full_nm Character; The ARU project full name
#' @importFrom glue glue_sql
#' @importFrom DBI dbSendQuery dbFetch
#' @export
#' @examples
#' \donttest{
#' connection <- wt_con(username = "", password = ")
#' full_name <- proj_full_nm # set project full name as character
#' aru_project_summary <- wt_aru_tag_summary(conn = connection, proj_full_nm = full_name)
#' }
#' @return This function returns a dataframe of tagged ARU recording data from the project specified in proj_full_nm
#'

wt_aru_tag_summary <- function(conn, proj_full_nm) {

  # Compose SQL query
    query <- glue_sql(
      "SELECT
       project_full_nm as project,
       project_year as year,
       data_set_nm || '-' || site_name || '-' || station_name as station_key,
       site_public_latitude, site_public_longitude,
       station_latitude, station_longitude, station_buffer_size,
       recording_file_name, recording_file_type, recording_audio_length, to_char(recording_date,'YYYY-MM-DD') as recording_date, to_char(recording_date, 'HH24:mi:ss') as recording_time, to_char(recording_date,'J') as JD, recording_spectrogram_profile, recording_source_file_path, recording_channels,
       task_id, task_mean_decibels, task_max_decibels, task_rms_decibels, task_rain_id, task_wind_id, task_other_noise_id, task_industrial_noise_id, status_task_type, audio_quality_type, task_comments,
       user_name,
       method_type, method_min_length,
       tag_id, si_species_code, si_comments, si_unique_name, abundance_type, confidence_type, vocalization_type,
       aru.get_tag_start_time(tag_id) as tag_start_time,
       aru.get_tag_length(tag_id) as tag_length
       FROM aru.tag
        JOIN aru.species_individual ON tag_task_id = si_task_id AND tag_species_individual_id = si_id
        JOIN aru.task ON si_task_id = task_id
        JOIN aru.recording ON task_recording_id = recording_id
        JOIN common.station ON recording_station_id = station_id
        JOIN common.site ON station_site_id = site_id
        JOIN common.data_set ON site_data_set_id = data_set_id
        JOIN aru.lu_status_task on task_status_id = status_task_id
        JOIN aru.lu_audio_quality on task_audio_quality_id = audio_quality_id
        JOIN aru.lu_method ON task_method_id = method_id
        JOIN aru.lu_abundance ON si_abundance_id = abundance_id
        JOIN aru.lu_confidence ON si_confidence_id = confidence_id
        JOIN common.app_user ON user_id = task_transcriber_user_id
        JOIN aru.lu_vocalization ON tag_vocalization_id = vocalization_id
        JOIN common.project ON task_project_id = project_id
      WHERE project_full_nm IN ({project_full_name*});",
      project_full_name = proj_full_nm,
      .con = conn)

  # Send query to WildTrax
  send_query <- dbSendQuery(conn = conn, statement = query)

  # Fetch result
  x <- dbFetch(send_query)

  if(nrow(x) == 0) {
    warning("Zero results found. Have you correctly specified the project full name?")
  }

  # Clear query
  dbClearResult(send_query)

  return(x)

}




