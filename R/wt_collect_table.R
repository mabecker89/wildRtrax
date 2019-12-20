#' Collect a table from the Wildtrax PostgreSQL database
#'
#' @param con PostgreSQLConnection object to Wildtrax
#' @param schema Character string indicating the schema that the table is in
#' @param table Character string indicating the table
#' @import dbplyr
#' @import dplyr
#' @export
#' @examples
#' @return This function returns a \code{data.frame} of the table requested.
#' @export
#' @author Marcus Becker
#' @author Alex MacPhail

# Collect a table from a specified schema in Wildtrax
wt_collect_table <- function(con, schema, table) {
  # Define database object
  db_object <- dplyr::tbl(con, dbplyr::in_schema(schema, table))
  # Collect data from database
  collect(db_object)
}
