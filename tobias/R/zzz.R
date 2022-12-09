
.onAttach <- function(libname, pkgname, spark_home) {
  sc <- sparklyr::spark_connect(method="databricks", spark_home=Sys.getenv("SPARK_HOME"))
  assign("sc", sc, envir = .GlobalEnv)
  packageStartupMessage("Spark loaded!")
}
