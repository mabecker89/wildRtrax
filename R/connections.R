# Establish connection to Wildtrax

wt_con <- function(db, user, password){
  con <- DBI::dbConnect(
    drv = dbDriver("PostgreSQL"),
    dbname = db,
    host = "prod.wildtrax.ca",
    port = "5432",
    user = user,
    password = password
  )
  con
}
