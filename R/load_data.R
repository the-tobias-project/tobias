
#' read_data
#'
#'@description
#'
#' @param path
#' @export

# TODO: ADD VERSIONING HERE DEPENDING ON USER INPUT

library(magrittr)

read_data <- function(con, db, tab, clinvar_version) {

    print("Connecting to database...")
    print("Reading data...")
    #con  <- connect_cluster()
    clinvar <- dplyr::tbl(con, dbplyr::in_schema(db, tab))
    clinvar <- clinvar %>% dplyr::filter(clinvar_version == clinvar_version)
    todrop <- c("timestamp",
                "gnomad_version",
                "clinvar_version")[c("timestamp",
                                     "gnomad_version",
                                     "clinvar_version") %in% colnames(clinvar)]
    if(length(todrop) > 0) {
      clinvar <- dplyr::select(clinvar, -todrop)
    }
    #clinvar <- dplyr::select(clinvar, -c(hash, latest))
    #disconnect_cluster(con)
    #clinvar <- clinvar %>% dplyr::mutate(CLNSIG=as.factor(CLNSIG))
    clinvar
}

