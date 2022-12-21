
#' read_data
#'
#'@description
#'
#' @param path
#' @export

# TODO: ADD VERSIONING HERE DEPENDING ON USER INPUT

library(magrittr)

read_data <- function(con, db, tab) {

    print("Connecting to database...")
    print("Reading data...")
    #con  <- connect_cluster()
    clinvar <- dplyr::tbl(con, dbplyr::in_schema(db, tab))
    todrop <- c("hash", "latest")[c("hash", "latest") %in% colnames(clinvar)]
    if(length(todrop) > 0) {
      clinvar <- dplyr::select(clinvar, -todrop)
    }
    #clinvar <- dplyr::select(clinvar, -c(hash, latest))
    #disconnect_cluster(con)
    #clinvar <- clinvar %>% dplyr::mutate(CLNSIG=as.factor(CLNSIG))
    clinvar
}

