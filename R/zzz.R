library(DBI)
library(odbc)

.onAttach <- function(libname, pkgname, spark_home) {
  con <- DBI::dbConnect(odbc::odbc(),
                   dsn = "<data_source_name>",
                   UID = "<userID>",
                   PWD = "<password>")
  assign("con", con, envir = .GlobalEnv)
  packageStartupMessage("Databricks connected!")
}

.onDetach <- function(libpath) {
  DBI::dbDisconnect(con)
  packageStartupMessage("Databricks disconnected!")
}
