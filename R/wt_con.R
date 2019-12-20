#' Establish Connection to Wildtrax PostgreSQL Database
#'
#' @param username A character string indicating the username for access the Wildtrax database
#' @param password A character string indicating the password associated with the supplied username
#' @import DBI
#' @import RPostgreSQL
#' @export
#' @examples
#' @return This function returns an S4 object of class PostgreSQLConnection
#'
#' @export
#' @author Marcus Becker
#' @author Alex MacPhail

# Establish connection to Wildtrax database
wt_con <- function(username, password){
    con <- DBI::dbConnect(
    drv = dbDriver("PostgreSQL"),
    dbname = "wildtrax",
    host = "prod.wildtrax.ca",
    port = "5432",
    user = username,
    password = password
  )
  con
}
