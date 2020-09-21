#' Establish Connection to WildTrax PostgreSQL Database
#'
#' @param username A character string indicating the username for accessing the WildTrax database
#' @param password A character string indicating the password associated with the supplied username
#' @import DBI RPostgreSQL
#' @export
#'
#' @return This function returns an S4 object of class PostgreSQLConnection

# Establish connection to Wildtrax database
wt_connect <- function(username, password) {
  conn <- DBI::dbConnect(
    drv = dbDriver("PostgreSQL"),
    dbname = "wildtrax",
    host = "prod.wildtrax.ca",
    port = "5432",
    user = username,
    password = password
  )

  return(conn)

}

