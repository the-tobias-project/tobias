
#' read_data
#'
#'@description
#'
#' @param path
#' @export

read_data <- function(path) {
    #'/Users/snehit/dev/ancestrybias/clinvar.exac.variants.gene.submission.diseases.alleles.tab'
    clinvar <- read.delim(path, sep = "\t", header = TRUE)
    dim(clinvar)

    # with(clinvar, tapply(CLNSIG, GENEINFO, FUN = function(x)
    # length(unique(x)))) with(clinvar, table(GENEINFO, CLNREVSTAT))

    clinvar$CLNSIG <- as.factor(clinvar$CLNSIG)
    levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG) == "0"] <- "VUS"
    levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG) == "1"] <- "Not Provided"
    levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG) == "2"] <- "B or LB"  # Benign
    levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG) == "3"] <- "B or LB"  # Likely Benign
    levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG) == "4"] <- "P or LP"  # Likely Pathogenic
    levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG) == "5"] <- "P or LP"  # Pathogenic
    levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG) == "6"] <- "Drug Response"
    levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG) == "7"] <- "Histocompatibility"
    levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG) == "255"] <- "Other"
    levels(clinvar$CLNSIG)

    clinvar <- clinvar[which(clinvar$CLNSIG == "B or LB" | clinvar$CLNSIG == "VUS" |
        clinvar$CLNSIG == "P or LP"), ]
    clinvar$CLNSIG <- factor(clinvar$CLNSIG)
    dim(clinvar)

    # clinvar <- clinvar[which(clinvar$GENEINFO=='BRCA2:675' |
    # clinvar$GENEINFO=='BRCA1:672'), ]

    # confirm no 0 entries
    xtabs(~CLNSIG, data = clinvar)

    clinvar
}

