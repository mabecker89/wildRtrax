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

wt_api_get <- function (res) {
  res = GET("https://www-api.wildtrax.ca")
  return (res)
}


# rawToChar(res$content)
# data = fromJSON(rawToChar(res$content))
# names(data)
# data$people
