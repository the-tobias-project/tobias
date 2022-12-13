
#' read_data
#'
#'@description
#'
#' @param path
#' @export

# TODO: ADD VERSIONING HERE DEPENDING ON USER INPUT

read_data <- function(path, version = "latest") {

    print("Connecting to database...")
    sparklyr::tbl_change_db(sc, "tobias")
    print("Reading data...")
    clinvar <- sparklyr::spark_read_table(sc, "shinyapp_input")
    #clinvar <- sparklyr::spark_read_table(sc, "original_table")
    print("Transforming data...")
    clinvar <- dplyr::select(clinvar, -c(hash, latest))
    clinvar <- sparklyr::collect(clinvar)
    head(clinvar) # remove for final version
    clinvar <- as.data.frame(clinvar)
    clinvar$CLNSIG <- factor(clinvar$CLNSIG)
    clinvar
}

