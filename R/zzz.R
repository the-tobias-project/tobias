.onAttach <- function(libname, pkgname, spark_home) {
  con <- DBI::dbConnect(odbc::odbc(), "Databricks")
  assign("con", con, envir = .GlobalEnv)
  packageStartupMessage("Databricks connected!")
}

.onDetach <- function(libpath) {
  DBI::dbDisconnect(con)
  packageStartupMessage("Databricks disconnected!")
}
