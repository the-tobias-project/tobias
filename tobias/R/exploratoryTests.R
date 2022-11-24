


#' ratio_func
#'
#'@description
#'
#' @param x


ratio_func = function(x){
  x/10
}

#' diff_func
#'
#'@description
#'
#' @param x

diff_func = function(x) {
  x+0.01
}

#' cutoff_func
#'
#'@description
#'
#' @param x
#' @export

cutoff_func = function(x) {
  0.01
}

#' scatter_grob
#'
#'@description
#'
#' @param dataset
#' @param pop1
#' @param pop2
#' @param min1
#' @param min2
#' @importFrom ggplot2 ggplot geom_point geom_line theme_bw theme scale_y_log10 scale_x_log10 aes aes_string
#' @importFrom scales trans_breaks trans_format math_format

#' @export

scatter_grob <- function(dataset, pop1, pop2, min1, min2) {
  partitions = data.frame(xvar = c(1:10 %o% 10^(-7:-1))) #seq(min1, 0.0499, 0.001))
  partitions$pred = diff_func(partitions$xvar) #new.dat$x1 + 0.1 #0.01

  scatter <- ggplot(data=dataset) +
    geom_point(aes_string(x=pop1, y=pop2, colour="ACMG"), cex=0.4, alpha=0.4) + #col=colorcode[clinvar$ACMG],
    geom_line(aes(x = xvar, y = pred), data = partitions, col = "black", linetype=2) +
    geom_line(aes(x = pred, y = xvar), data = partitions, col = "black", linetype=2) +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), limits=c(min1,1.2)) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), limits=c(min2,1.2)) +
    theme_bw() + theme(legend.position = "none") # order matter here. theme_bw() at the end overrides hide-legend setting

  scatter
}


#' hist_grob
#'
#'@description
#'
#' @param dataset
#' @param pop1
#' @param pop2
#' @param min1
#' @importFrom ggplot2 ggplot geom_histogram scale_fill_manual theme_bw theme element_blank element_text scale_y_log10 scale_x_log10 aes_string
#' @importFrom scales trans_breaks trans_format math_format comma
#' @export

hist_grob <- function(dataset, pop1, pop2, min1, colorcode) {
  histOfVariants <- ggplot(data=dataset) +
    geom_histogram(aes_string(pop1, color="ACMG"), alpha=0.4, position = 'identity', binwidth = 0.1, pad="TRUE") + scale_fill_manual(values=colorcode) +
    theme_bw() + theme(legend.position="none", axis.title.x=element_blank(), text = element_text(size=10)) +
    scale_y_log10(labels = comma, limits=c(1,10000), breaks=c(1,10,100,1000,10000)) +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)), limits=c(min1,1.2))

  histOfVariants
}

#' get_counts
#'
#'@description
#'
#' @param dataset
#' @export

get_counts <- function(dataset) {
  enrichmentTable <- matrix(nrow=1, ncol=3)
  enrichmentTable[1,1] = nrow(dataset[which(dataset[, "ACMG"]=="B or LB"),])
  enrichmentTable[1,2] = nrow(dataset[which(dataset[, "ACMG"]=="VUS"),])
  enrichmentTable[1,3] = nrow(dataset[which(dataset[, "ACMG"]=="P or LP"),])
  return(enrichmentTable)
}

#' test_enrichment
#'
#'@description
#'
#' @param dataset
#' @param pop1
#' @param pop2
#' @param colorcode
#' @param title
#' @param xlabel
#' @param ylabel
#' @importFrom  grid legendGrob gpar textGrob grid.draw unit
#' @importFrom gridExtra tableGrob ttheme_default tableGrob
#' @importFrom gtable gtable gtable_add_grob
#' @importFrom ggplot2 ggplotGrob coord_flip
#' @export

test_enrichment <- function(dataset, pop1, pop2, colorcode, title, xlabel, ylabel) {

  min1 <- min(dataset[,pop1][which(dataset[,pop1]!=0)])/1.5
  min2 <- min(dataset[,pop2][which(dataset[,pop2]!=0)])/1.5
  dataset[,pop1][which(dataset[,pop1]==0)]<- min1
  dataset[,pop2][which(dataset[,pop2]==0)]<- min2

  dataset_pop1 <- dataset[dataset[,pop1] > diff_func(dataset[,pop2]),]
  hist_top <- hist_grob(dataset_pop1, pop1, pop2, min1, colorcode)

  scatter <- scatter_grob(dataset, pop1, pop2, min1, min2)

  dataset_pop2 <- dataset[dataset[,pop2] > diff_func(dataset[,pop1]),]
  hist_right <- hist_grob(dataset_pop2, pop2, pop1, min2) + coord_flip()

  counts_pop1 <- get_counts(dataset_pop1)
  counts_pop2 <- get_counts(dataset_pop2)
  countTable <- rbind(counts_pop1, counts_pop2)

  ## Extra attempts to format the top right grob here
  legend <- legendGrob(c("B or LB", "VUS", "P or LP"), pch=15, gp= gpar(col=colorcode, fontsize=10), ncol=3, hgap=0.2) #alpha=0.4
  title <- textGrob(label="Total counts by category", gp = gpar(fontsize = 10, fontface = 'italic')) #, x = unit(0.1, "inches"), y = unit(0.1, "inches"), just = "center", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE, default.units = "npc", name = NULL, gp = grid::gpar(), vp = NULL)
  table <- tableGrob(rows = c(pop1, pop2), cols = c("B or LB", "VUS", "P or LP"), d=countTable, theme = ttheme_default(base_size=10)) #
  annot <- gtable(widths=unit(5,"cm"), heights=unit(c(0.3,0.3,0.8), c("inches")), respect = TRUE)
  annot <- gtable_add_grob(annot, legend, 1,1)
  annot <- gtable_add_grob(annot, title, 2,1)
  annot <- gtable_add_grob(annot, table, 3,1)
  #grid.draw(annot)
  ##

  one <- ggplotGrob(hist_top)
  two <- ggplotGrob(scatter)
  three <- ggplotGrob(hist_right)
  four <- annot

  #http://stackoverflow.com/questions/20552226/make-one-panel-blank-in-ggplot2
  all <- gtable(unit(c(10,5), c("cm")), unit(c(5,10), c("cm")))
  all <- gtable_add_grob(all, one, 1, 1)
  all <- gtable_add_grob(all, two, 2, 1)
  all <- gtable_add_grob(all, three, 2, 2)
  all <- gtable_add_grob(all, four, 1, 2)
  #quartz()
  grid.draw(all)

}
