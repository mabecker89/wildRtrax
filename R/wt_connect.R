#' Establish Connection to WildTrax PostgreSQL Database
#'
#' @param username A character string indicating the username for accessing the WildTrax database
#' @param password A character string indicating the password associated with the supplied username
#' @import DBI RPostgreSQL svDialogs
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
    user = dlgInput("Enter your username: ", Sys.info()["user"])$res,
    password = dlgInput("Enter your password: ", Sys.info()["user"])$res
  )
  return(conn)
}
