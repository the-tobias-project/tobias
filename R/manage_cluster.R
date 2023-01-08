
#' Start cluster
#' @export

connect_cluster <- function(type=c("odbc", "jdbc")) {
  if(type == "odbc") {
  con <- DBI::dbConnect(odbc::odbc(), "Databricks")
  con
  } else {
    driver <- RJDBC::JDBC(driverClass = "com.databricks.client.jdbc.Driver",
                   classPath = Sys.getenv("DATABRICKS_CLASSPATH")
    con <- DBI::dbConnect(driver,Sys.getenv("JDBC_PATH"))
  }
  con
}

#' Stop cluster
#' @param con connection
#' @export

disconnect_cluster <- function(con) {
  DBI::dbDisconnect(con)
}
