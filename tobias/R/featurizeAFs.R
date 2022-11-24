
#' set_AFs
#'
#'@description
#'
#' @param dataset
#' @param poplabels
#' @export

set_AFs <- function(dataset, poplabels) {
    # c('adj', 'nfe', 'afr', 'amr', 'sas', 'eas', 'fin', 'oth')
    for (pop in poplabels) {
        allele <- paste("ac_", pop, sep = "")
        dataset[, allele] <- as.numeric(as.character(dataset[, allele]))
        total <- paste("an_", pop, sep = "")
        dataset[, total] <- as.numeric(as.character(dataset[, total]))
        freq <- paste("af_", pop, sep = "")

        # MODIFIED dataset[,freq] <- clinvar[,allele]/pmax(dataset[,total],1)
        # changed to 'dataset' since 'clinvar' was not previously defined
        dataset[, freq] <- dataset[, allele]/pmax(dataset[, total], 1)
        # End of MODIFIED
    }
    dataset
}

#' set_residual_AFs
#'
#'@description
#'
#' @param dataset
#' @param poplabels
#' @export

set_residual_AFs <- function(dataset, poplabels) {
    for (pop in poplabels) {

        # Definitions that were never added in the scope of this function ADDED
        allele <- paste("ac_", pop, sep = "")
        dataset[, allele] <- as.numeric(as.character(dataset[, allele]))
        total <- paste("an_", pop, sep = "")
        dataset[, total] <- as.numeric(as.character(dataset[, total]))
        population <- paste("af_", pop, sep = "")
        dataset[, population] <- dataset[, allele]/pmax(dataset[, total], 1)
        # End of ADDED


        dfreq <- paste("d_", pop, sep = "")
        dataset[, dfreq] <- dataset[, population] - dataset[, "af_adj"]
        range(dataset[, dfreq])
        mean(dataset[, dfreq])
    }
    dataset
}

#' hist_AFs
#'
#'@description
#'
#' @param dataset
#' @param poplabels
#' @param colorName
#' @export
# Can we split this function into two functions?  One will call the for loop
# for all populations in the API Another will create individual histograms,
# also used by the UI

hist_AFs <- function(dataset, poplabels, colorName) {
    i = 1
    for (pop in poplabels) {
        popfreq <- paste("af_", pop, sep = "")
        opar = par(ps = 20, font = 7)
        hist(dataset[, popfreq], breaks = 100, cex = 10, main = "", col = colorName,
            xlim = c(0, 1), ylim = c(0, 70000), xlab = paste("AF(", pop, ")", sep = ""),
            ylab = "# variants")
        opar
    }
}

#' hist_residual_AFs
#'
#'@description
#'
#' @param dataset
#' @param poplabels
#' @param colorName
#' @export

hist_residual_AFs <- function(dataset, poplabels, colorName) {
    for (pop in poplabels) {
        dfreq <- paste("d_", pop, sep = "")
        opar = par(ps = 20, font = 7)
        hist(dataset[, dfreq], breaks = 100, cex = 10, main = "", col = colorName,
            xlim = c(-1, 1), ylim = c(0, 70000), xlab = paste("AF(", pop, ") - AF(global)",
                sep = ""), ylab = "# variants")
        opar
    }
}

#' scatter_plot
#'
#'@description
#'
#' @param dataset
#' @param feature1
#' @param feature2
#' @param colorByClass
#' @param colorcode
#' @param title
#' @param xlabel
#' @param ylabel
#' @importFrom  scales alpha
#' @export

scatter_plot <- function(dataset, feature1, feature2, colorByClass, colorcode, title,
    xlabel, ylabel) {
    # try( if (length(colorcode) != levels(dataset[,colorByClass]))
    # return('colorByClass has different number of levels than colors in
    # colorcode') )

    plot(dataset[, feature1], dataset[, feature2], col = alpha(colorcode[dataset[,
        colorByClass]], 1), pch = 10, cex = 0.3, main = title, xlab = xlabel, ylab = ylabel)

    # MODIFIED changed to 'dataset' since 'clinvar' was not previously defined
    # legend('topleft',col=alpha(colorcode,1.0), pch = 10, cex=.5,
    # legend=levels(clinvar[,colorByClass]), bty='n')
    legend("topleft", col = alpha(colorcode, 1), pch = 10, cex = 0.5, legend = levels(dataset[,
        colorByClass]), bty = "n")
    # End of MODIFIED


    # abline(a=0, b=20,lty=2,col='gray60') abline(a=0,
    # b=0.05,lty=2,col='gray60')
}
