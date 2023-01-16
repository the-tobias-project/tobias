
#' Start cluster
#' @export

connect_cluster <- function(type=c("odbc", "jdbc", "sparklyr")) {
  if(type == "odbc") {
  con <- DBI::dbConnect(odbc::odbc(), "Databricks")
  con
  } else if(type=="jdbc") {
    driver <- RJDBC::JDBC(driverClass = "com.databricks.client.jdbc.Driver",
                   classPath = Sys.getenv("DATABRICKS_CLASSPATH"))
    con <- DBI::dbConnect(driver, Sys.getenv("JDBC_PATH"))
  } else if(type == "sparklyr"){
    config <- spark_config()
    config$`sparklyr.shell.driver-class-path` <- Sys.getenv("JDBC_PATH")
    sc <- spark_connect(master = "local", config = config)
  }
  con$"type" <- type
  con
}

#' Stop cluster
#' @param con connection
#' @export

disconnect_cluster <- function(con) {
  if(con$type %in% ("odbc", "jdbc")) {
    DBI::dbDisconnect(con)
  } else {
    spark_disconnect(con)
  }
}
