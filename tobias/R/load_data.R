
#' read_data
#'
#'@description
#'
#' @param path
#' @export

read_data <- function(path) {

    print("Connecting to database...")
    sparklyr::tbl_change_db(sc, "tobias")
    print("Reading data...")
    clinvar <- sparklyr::spark_read_table(sc, "shinyapp_input")
    print("Transforming data...")
    clinvar <- dplyr::select(clinvar, -c(hash))
    clinvar <- sparklyr::collect(clinvar)
    clinvar
}

