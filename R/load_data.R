
#' read_data
#'
#'@description
#'
#' @param path
#' @export

# TODO: ADD VERSIONING HERE DEPENDING ON USER INPUT

library(magrittr)

read_data <- function(path, version = "latest") {

    print("Connecting to database...")
    print("Reading data...")
    clinvar <- dplyr::tbl(con, dbplyr::in_schema("tobias", "original_table"))
    #clinvar <- sparklyr::spark_read_table(sc, "original_table")
    print("Transforming data...")
    #clinvar <- dplyr::select(clinvar, -c(hash, latest))
    #clinvar <- sparklyr::collect(clinvar)
    head(clinvar) # remove for final version
    clinvar <- as.data.frame(clinvar)
    clinvar$CLNSIG <- factor(clinvar$CLNSIG)
    #clinvar
    #clinvar <- clinvar %>% dplyr::mutate(CLNSIG=as.factor(CLNSIG))
    clinvar
}

