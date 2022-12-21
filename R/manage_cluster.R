
#' Start cluster
#' @export

connect_cluster <- function() {
  con <- DBI::dbConnect(odbc::odbc(), "Databricks")
  con
}

#' Stop cluster
#' @param con connection
#' @export

disconnect_cluster <- function(con) {
  DBI::dbDisconnect(con)
}
