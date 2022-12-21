
#' Start cluster
#' @param libname
#' @param pkgname
#' @param spark_home
#' @export

connect_cluster <- function() {
  con <- DBI::dbConnect(odbc::odbc(), "Databricks")
  con
}

#' Stop cluster
#' @param libpath
#' @export

disconnect_cluster <- function(con) {
  DBI::dbDisconnect(con)
  packageStartupMessage("Databricks disconnected!")
}
