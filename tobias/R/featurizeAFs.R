
require(scales)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
require(effects)



#' setAFs
#'
#'@description
#'
#' @param dataset
#' @param poplabels
#' @export

setAFs <- function(dataset, poplabels) {
    # c('adj', 'nfe', 'afr', 'amr', 'sas', 'eas', 'fin', 'oth')
    for (pop in poplabels) {
        assign("allele", paste("ac_", pop, sep = ""))
        dataset[, allele] <- as.numeric(as.character(dataset[, allele]))
        assign("total", paste("an_", pop, sep = ""))
        dataset[, total] <- as.numeric(as.character(dataset[, total]))
        assign("freq", paste("af_", pop, sep = ""))

        # MODIFIED dataset[,freq] <- clinvar[,allele]/pmax(dataset[,total],1)
        # changed to 'dataset' since 'clinvar' was not previously defined
        dataset[, freq] <- dataset[, allele]/pmax(dataset[, total], 1)
        # End of MODIFIED
    }
    dataset
}

#' setResidualAFs
#'
#'@description
#'
#' @param dataset
#' @param poplabels
#' @export

setResidualAFs <- function(dataset, poplabels) {
    for (pop in poplabels) {

        # Definitions that were never added in the scope of this function ADDED
        assign("allele", paste("ac_", pop, sep = ""))
        dataset[, allele] <- as.numeric(as.character(dataset[, allele]))
        assign("total", paste("an_", pop, sep = ""))
        dataset[, total] <- as.numeric(as.character(dataset[, total]))
        assign("population", paste("af_", pop, sep = ""))
        dataset[, population] <- dataset[, allele]/pmax(dataset[, total], 1)
        # End of ADDED


        assign("dfreq", paste("d_", pop, sep = ""))
        dataset[, dfreq] <- dataset[, population] - dataset[, "af_adj"]
        range(dataset[, dfreq])
        mean(dataset[, dfreq])
    }
    dataset
}

#' histAFs
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

histAFs <- function(dataset, poplabels, colorName) {
    i = 1
    for (pop in poplabels) {
        assign("popfreq", paste("af_", pop, sep = ""))
        opar = par(ps = 20, font = 7)
        hist(dataset[, popfreq], breaks = 100, cex = 10, main = "", col = colorName,
            xlim = c(0, 1), ylim = c(0, 70000), xlab = paste("AF(", pop, ")", sep = ""),
            ylab = "# variants")
        opar
    }
}

#' histResidualAFs
#'
#'@description
#'
#' @param dataset
#' @param poplabels
#' @param colorName
#' @export

histResidualAFs <- function(dataset, poplabels, colorName) {
    for (pop in poplabels) {
        assign("dfreq", paste("d_", pop, sep = ""))
        opar = par(ps = 20, font = 7)
        hist(dataset[, dfreq], breaks = 100, cex = 10, main = "", col = colorName,
            xlim = c(-1, 1), ylim = c(0, 70000), xlab = paste("AF(", pop, ") - AF(global)",
                sep = ""), ylab = "# variants")
        opar
    }
}

#' scatterPlot
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
#' @export

scatterPlot <- function(dataset, feature1, feature2, colorByClass, colorcode, title,
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
