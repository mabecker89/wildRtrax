#' Establish connection to WildTrax via an API
#'
#' @param username A character string indicating the username for accessing the WildTrax database
#' @param password A character string indicating the password associated with the supplied username
#' @import httr jsonlite
#' @export
#'
#' @return This function returns the json object of the GET request


library(httr)
library(jsonlite)

wt_api_get <- function (site) {
  res = GET(site)
  return (res)
}

x <- wt_api_get("https://www.wildtrax.ca")
y <- fromJSON(rawToChar(x$content))

