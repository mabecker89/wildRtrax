#' Retrieve camera deployment metadata
#'
#' @param con PostgreSQLConnection object to Wildtrax
#' @param year Character string to filter by year
#' @param project Character string of full project name to filter by project
#' @import dbplyr
#' @import dplyr
#' @export
#' @examples
#' @return This function returns a \code{data.frame} of the metadata associated with the camera deployments requested.
#' @export
#' @author Marcus Becker
#' @author Alex MacPhail

# Collect camera deployment metadata
wt_collect_cam_meta <- function(con, year, project) {

  # Define database objects
  deployments <- wt_collect_table(con, schema = "camera", table = "deployment")
  stations <- wt_collect_table(con, schema = "common", table = "station")
  sites <- wt_collect_table(con, schema = "common", table = "site")
  projects <- wt_collect_table(con, schema = "common", "project")

  # Create query
  deployments %>%
    left_join(stations, by = c("deployment_station_id" = "station_id")) %>%
    left_join(sites, by = c("station_site_id" = "site_id")) %>%
    left_join(projects, by = c("deployment_project_owner_id" = "project_id")) %>%
    filter(deployment_year == year,
           project_full_nm == project) %>%
    select(deployment_alias_name, deployment_year, project_full_nm, site_public_latitude, site_public_longitude)

}
