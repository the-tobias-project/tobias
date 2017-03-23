#!/usr/bin/Rscript

#---------------------------------------------------------------
# Project: Ancestry Bias in ClinVar (w.r.t ExAC population data)
# Author: Snehit Prabhu <snehit@stanford.edu>
#---------------------------------------------------------------

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
require(effects)

clinvar <- read.delim("/Users/snehit/dev/ancestrybias/clinvar.exac.variants.gene.submission.diseases.alleles.tab", sep="\t", header=TRUE)
#clinvar <- read.delim("/Users/snehit/dev/ancestrybias/clinvar.exac.variants.gene.submission.diseases.alleles.acmg56.tab", sep="\t", header=TRUE)
dim(clinvar)
# with(clinvar, tapply(CLNSIG, GENEINFO, FUN = function(x) length(unique(x))))
# with(clinvar, table(GENEINFO, CLNREVSTAT))

clinvar$CLNSIG <- as.factor(clinvar$CLNSIG)
levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG)=="0"] <- "VUS"
levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG)=="1"] <- "Not Provided"
levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG)=="2"] <- "B or LB" # Benign
levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG)=="3"] <- "B or LB" # Likely Benign
levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG)=="4"] <- "P or LP" # Likely Pathogenic
levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG)=="5"] <- "P or LP" # Pathogenic
levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG)=="6"] <- "Drug Response"
levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG)=="7"] <- "Histocompatibility"
levels(clinvar$CLNSIG)[levels(clinvar$CLNSIG)=="255"] <- "Other"
levels(clinvar$CLNSIG)

clinvar <- clinvar[which(clinvar$CLNSIG=='B or LB' | clinvar$CLNSIG=='VUS' | clinvar$CLNSIG=='P or LP'), ]
clinvar$CLNSIG <-factor(clinvar$CLNSIG)
dim(clinvar)

#clinvar <- clinvar[which(clinvar$GENEINFO=="BRCA2:675" | clinvar$GENEINFO=="BRCA1:672"), ]

#confirm no 0 entries
xtabs(~ CLNSIG, data = clinvar)

# Overall
clinvar$ac_adj <- as.numeric(as.character(clinvar$ac_adj))
clinvar$an_adj <- as.numeric(as.character(clinvar$an_adj))
clinvar$af_adj <- clinvar$ac_adj/pmax(clinvar$an_adj,1)
range(clinvar$af_adj)

#Non-finn Europeans
clinvar$ac_nfe <- as.numeric(as.character(clinvar$ac_nfe))
clinvar$an_nfe <- as.numeric(as.character(clinvar$an_nfe))
clinvar$af_nfe <- clinvar$ac_nfe/pmax(clinvar$an_nfe,1)
range(clinvar$af_nfe)

#African
clinvar$ac_afr <- as.numeric(clinvar$ac_afr)
clinvar$an_afr <- as.numeric(clinvar$an_afr)
clinvar$af_afr <- clinvar$ac_afr/pmax(clinvar$an_afr,1)
range(clinvar$af_afr)

#American
clinvar$ac_amr <- as.numeric(clinvar$ac_amr)
clinvar$an_amr <- as.numeric(clinvar$an_amr)
clinvar$af_amr <- clinvar$ac_amr/pmax(clinvar$an_amr,1)
range(clinvar$af_amr)

#East Asian
clinvar$ac_eas <- as.numeric(clinvar$ac_eas)
clinvar$an_eas <- as.numeric(clinvar$an_eas)
clinvar$af_eas <- clinvar$ac_eas/pmax(clinvar$an_eas,1)
range(clinvar$af_eas)

#South Asian
clinvar$ac_sas <- as.numeric(clinvar$ac_sas)
clinvar$an_sas <- as.numeric(clinvar$an_sas)
clinvar$af_sas <- clinvar$ac_sas/pmax(clinvar$an_sas,1)
range(clinvar$af_sas)

#Finn
clinvar$ac_fin <- as.numeric(clinvar$ac_fin)
clinvar$an_fin <- as.numeric(clinvar$an_fin)
clinvar$af_fin <- clinvar$ac_fin/pmax(clinvar$an_fin,1)
range(clinvar$af_fin)

#Other
clinvar$ac_oth <- as.numeric(clinvar$ac_oth)
clinvar$an_oth <- as.numeric(clinvar$an_oth)
clinvar$af_oth <- clinvar$ac_oth/pmax(clinvar$an_oth,1)
range(clinvar$af_oth)


#Get predictor variables (AF diffs) after all kinds of normalization
clinvar$nfe_adj <-clinvar$af_nfe - clinvar$af_adj # (clinvar$af_nfe - clinvar$af_afr) + (clinvar$af_nfe - clinvar$af_amr) + (clinvar$af_nfe - clinvar$af_eas) + (clinvar$af_nfe - clinvar$af_sas) + (clinvar$af_nfe - clinvar$af_fin) + (clinvar$af_nfe - clinvar$af_oth)
range(clinvar$nfe_adj)
mean(clinvar$nfe_adj)
opar=par(ps=20)
hist(clinvar$nfe_adj, breaks=100, cex=10, main="", col="grey", xlim=c(-1,1), ylim=c(0,50000), xlab="AF(nfe) - AF(global)")
opar
# clinvar$nfe_adj <- (clinvar$nfe_adj - mean(clinvar$nfe_adj)) / sd(clinvar$nfe_adj)

clinvar$afr_adj <- clinvar$af_afr - clinvar$af_adj# (clinvar$af_afr - clinvar$af_nfe) + (clinvar$af_afr - clinvar$af_amr) + (clinvar$af_afr - clinvar$af_eas) + (clinvar$af_afr - clinvar$af_sas) + (clinvar$af_afr - clinvar$af_fin) + (clinvar$af_afr - clinvar$af_oth)
range(clinvar$afr_adj)
mean(clinvar$afr_adj)
opar=par(ps=20)
hist(clinvar$afr_adj, breaks=100, main="", col="purple", xlim=c(-1,1), ylim=c(0,50000), xlab="AF(afr) - AF(global)")
opar
# clinvar$afr_adj <- (clinvar$afr_adj - mean(clinvar$afr_adj)) / sd(clinvar$afr_adj)

clinvar$amr_adj <- clinvar$af_amr - clinvar$af_adj # (clinvar$af_amr - clinvar$af_nfe) + (clinvar$af_amr - clinvar$af_afr) + (clinvar$af_amr - clinvar$af_eas) + (clinvar$af_amr - clinvar$af_sas) + (clinvar$af_amr - clinvar$af_fin) + (clinvar$af_amr - clinvar$af_oth)
range(clinvar$amr_adj)
mean(clinvar$amr_adj)
opar=par(ps=20)
hist(clinvar$amr_adj, breaks=100, main="", col="darkgreen", xlim=c(-1,1), ylim=c(0,50000), xlab="AF(amr) - AF(global)")
opar
# clinvar$amr_adj <- (clinvar$amr_adj - mean(clinvar$amr_adj)) / sd(clinvar$amr_adj)

clinvar$sas_adj <- clinvar$af_sas - clinvar$af_adj # (clinvar$af_sas - clinvar$af_nfe) + (clinvar$af_sas - clinvar$af_afr) + (clinvar$af_sas - clinvar$af_amr) + (clinvar$af_sas - clinvar$af_eas) + (clinvar$af_sas - clinvar$af_fin) + (clinvar$af_sas - clinvar$af_oth)
range(clinvar$sas_adj)
mean(clinvar$sas_adj)
opar=par(ps=20)
hist(clinvar$sas_adj, breaks=100, main="", col="orange", xlim=c(-1,1), ylim=c(0,50000), xlab="AF(sas) - AF(global)")
opar
# clinvar$sas_adj <- (clinvar$sas_adj - mean(clinvar$sas_adj)) / sd(clinvar$sas_adj)

colorcode<-c("blue","lightgreen","red")
clinvar$ACMG <- relevel(clinvar$CLNSIG, ref = "VUS")

#scatter plots
library(scales)
par(mfrow=c(1,4))
plot(clinvar$af_nfe,clinvar$af_afr, col=alpha(colorcode[clinvar$ACMG],1.0), pch=10, cex=.3, main="African vs European")
legend("topleft",col=alpha(colorcode,1.0), pch = 10, cex=.5, legend=levels(clinvar$ACMG), bty="n")
abline(a=0, b=20,lty=2,col="gray60")
abline(a=0, b=0.05,lty=2,col="gray60")
plot(clinvar$af_nfe,clinvar$af_amr, col=colorcode[clinvar$ACMG], pch=10, cex=.3, main="NativeAmr vs European")
legend("topleft",col=colorcode, pch = 10, cex=.5, legend=levels(clinvar$ACMG), bty="n")
abline(a=0, b=20,lty=2,col="gray60")
abline(a=0, b=0.05,lty=2,col="gray60")
plot(clinvar$af_nfe,clinvar$af_sas, col=colorcode[clinvar$ACMG], pch=10, cex=.3, main="SouthAsian vs European")
legend("topleft",col=colorcode, pch = 10, cex=.5, legend=levels(clinvar$ACMG), bty="n")
abline(a=0, b=20,lty=2,col="gray60")
abline(a=0, b=0.05,lty=2,col="gray60")
plot(clinvar$af_nfe,clinvar$af_eas, col=colorcode[clinvar$ACMG], pch=10, cex=.3, main="EastAsian vs European")
legend("topleft",col=colorcode, pch = 10, cex=.5, legend=levels(clinvar$ACMG), bty="n")
abline(a=0, b=20,lty=2,col="gray60")
abline(a=0, b=0.05,lty=2,col="gray60")

library(ggplot2)
library(ggExtra)
library(gridExtra)
library(grid)
library(gtable)
min1 <- min(clinvar$af_nfe[which(clinvar$af_nfe!=0)])/1.5
min2 <- min(clinvar$af_sas[which(clinvar$af_sas!=0)])/1.5
clinvar$af_nfe[which(clinvar$af_nfe==0)]<- min1
clinvar$af_sas[which(clinvar$af_sas==0)]<- min2

ratio.func = function(x){
  x/10
}

diff.func = function(x) {
  x+0.01
}

cutoff.func = function(x) {
  0.01
}

new.dat = data.frame(xvar = c(1:10 %o% 10^(-7:-1))) #seq(min1, 0.0499, 0.001))
new.dat$pred = diff.func(new.dat$xvar) #new.dat$x1 + 0.1 #0.01
clinvar_new <- clinvar[clinvar$af_sas > diff.func(clinvar$af_nfe),]
clinvar_new2 <- clinvar[clinvar$af_nfe > diff.func(clinvar$af_sas),]

hist_top <- ggplot(data=clinvar_new) +
  geom_histogram(aes(af_nfe, color=ACMG), alpha=0.4, position = 'identity', binwidth = 0.1, pad="TRUE") + scale_fill_manual(values=colorcode) +
  theme_bw() + theme(legend.position="none", axis.title.x=element_blank(), text = element_text(size=10)) +
  scale_y_log10(labels = scales::comma, limits=c(1,10000), breaks=c(1,10,100,1000,10000)) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)), limits=c(min1,1.2))

scatter <- ggplot(data=clinvar) +
  geom_point(aes(x=af_nfe, y=af_sas, colour=ACMG), cex=0.4, alpha=0.4) + #col=colorcode[clinvar$ACMG],
  geom_line(aes(x = xvar, y = pred), data = new.dat, col = "black", linetype=2) +
  geom_line(aes(x = pred, y = xvar), data = new.dat, col = "black", linetype=2) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)), limits=c(min1,1.2)) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)), limits=c(min2,1.2)) +
  theme_bw() + theme(legend.position = "none") # order matter here. theme_bw() at the end overrides hide-legend setting

hist_right <- ggplot(data=clinvar_new2) +
  geom_histogram(aes(af_sas, colour=ACMG), alpha=0.4, position = 'identity', binwidth=0.1, pad=TRUE) +
  theme_bw() + theme(legend.position="none", axis.title.y=element_blank(), text = element_text(size=10)) +
  #coord_trans(y="log10") +
  scale_y_log10(labels = scales::comma, limits=c(1,10000), breaks=c(1,10,100,1000,10000,10000)) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format(10^.x)), limits=c(min2,1.2)) +
  coord_flip()

## Extra attempts to format the top right grob here
legend <- legendGrob(c("VUS", "B or LB", "P or LP"), pch=15, gp=gpar(col=colorcode, fontsize=10), ncol=3, hgap=0.2) #alpha=0.4
title <- textGrob(label="Total counts by category", gp = gpar(fontsize = 10, fontface = 'italic')) #, x = unit(0.1, "inches"), y = unit(0.1, "inches"), just = "center", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE, default.units = "npc", name = NULL, gp = gpar(), vp = NULL)
table <- tableGrob(rows = c("EUR", "SAS"), cols = c("VUS", "B or LB", "P or LP"), d=matrix(nrow=2, ncol=3, 1:6), theme = ttheme_default(base_size=10)) #
annot <- gtable(widths=unit(5,"cm"), heights=unit(c(0.3,0.3,0.8), c("inches")), respect = TRUE)
annot <- gtable_add_grob(annot, legend, 1,1)
annot <- gtable_add_grob(annot, title, 2,1)
annot <- gtable_add_grob(annot, table, 3,1)
grid.draw(annot)
##

one <- ggplotGrob(hist_top)
two <- ggplotGrob(scatter)
four <- annot
three <- ggplotGrob(hist_right)

#http://stackoverflow.com/questions/20552226/make-one-panel-blank-in-ggplot2
all <- gtable(unit(c(10,5), c("cm")), unit(c(5,10), c("cm")))
all <- gtable_add_grob(all, one, 1, 1)
all <- gtable_add_grob(all, two, 2, 1)
all <- gtable_add_grob(all, three, 2, 2)
all <- gtable_add_grob(all, four, 1, 2)
quartz()
grid.draw(all)


#### THIS IS WHERE I STOPPED ####

#1. Test the effects
# clinvar$nfe_adj_sq <- clinvar$nfe_adj^2
test_global_f <- multinom(ACMG ~ af_adj, data = clinvar) # ACMG = global_allele_freq
test_global_afr_f <- multinom(ACMG ~ af_adj + afr_adj, data = clinvar) # ACMG = global_allele_freq + (afr_allele_freq - global_allele_frequency)
test_ancestry_f <- multinom(ACMG ~ af_adj + nfe_adj + afr_adj + amr_adj + sas_adj, data = clinvar)

z_global_f <- summary(test_global_f)$coefficients/summary(test_global_f)$standard.errors
z_global_afr_f <- summary(test_global_afr_f)$coefficients/summary(test_global_afr_f)$standard.errors
z_ancestry_f <- summary(test_ancestry_f)$coefficients/summary(test_ancestry_f)$standard.errors

p_global_f <- (1 - pnorm(abs(z_global_f), 0, 1))*2
p_global_afr_f <- (1 - pnorm(abs(z_global_afr_f), 0, 1))*2
p_ancestry_f <- (1 - pnorm(abs(z_ancestry_f), 0, 1))*2

adj_fs <- data.frame(af_adj = c(0:100)/100)
nfe_diffs <- data.frame(nfe_adj = 2*c(-50:50)/100)
afr_diffs <- data.frame(afr_adj = 2*c(-50:50)/100)
amr_diffs <- data.frame(amr_adj = 2*c(-50:50)/100)
sas_diffs <- data.frame(sas_adj = 2*c(-50:50)/100)
afr_data <- cbind(adj_fs, afr_diffs)
ancestry_data <- cbind(adj_fs, nfe_diffs, afr_diffs, amr_diffs, sas_diffs)

global_preds <- cbind(adj_fs, predict(test_global_f, newdata = adj_fs, type = "probs", se = TRUE))
global_afr_preds <- cbind(afr_data, predict(test_global_afr_f, newdata = afr_data, type = "probs", se = TRUE))
ancestry_preds <- cbind(ancestry_data, predict(test_ancestry_f, newdata = ancestry_data, type = "probs", se = TRUE))

global_preds_melt <- melt(global_preds, id.vars = c("af_adj"), value.name = "probability")
global_afr_preds_melt <- melt(global_afr_preds, id.vars = c("af_adj", "afr_adj"), value.name = "probability")
ancestry_preds_melt <- melt(ancestry_preds, id.vars = c("af_adj", "nfe_adj", "afr_adj", "amr_adj", "sas_adj"), value.name = "probability")


## Plotting starts here ###

#Type 1: Probability plots
ggplot(global_preds_melt, aes(x = adj_fs, y = probability, colour = variable)) + geom_line() # + facet_grid(variable ~ ., scales="fixed")
ggplot(ancestry_preds_melt, aes(x = adj_fs, y = probability, colour = variable)) + geom_line() # + facet_grid(variable ~ ., scales="fixed")
ggplot(ancestry_preds_melt, aes(x = nfe_diffs, y = probability, colour = variable)) + geom_line() # + facet_grid(variable ~ ., scales="fixed")
ggplot(ancestry_preds_melt, aes(x = afr_diffs, y = probability, colour = variable)) + geom_line() # + facet_grid(variable ~ ., scales="fixed")
ggplot(ancestry_preds_melt, aes(x = amr_diffs, y = probability, colour = variable)) + geom_line() # + facet_grid(variable ~ ., scales="fixed")
ggplot(ancestry_preds_melt, aes(x = sas_diffs, y = probability, colour = variable)) + geom_line() # + facet_grid(variable ~ ., scales="fixed")

#Type 2: AF_diff based probability plots by professionals, with std. err.
test_global_eff <- Effect("af_adj", test_global_f, xlevels = adj_fs)
plot(test_global_eff, ylim=c(0,1), rug=FALSE, xlab = "AF(global)", ylab = "Probability", main="Global AFs", par.settings = list(fontsize = list(text = 20, points = 20)))

test_global_eff <- Effect("af_adj", test_global_afr_f, xlevels = adj_fs)
plot(test_global_eff, ylim=c(0,1), rug=FALSE, xlab = "AF(global)", ylab = "Probability", main="Global AFs", par.settings = list(fontsize = list(text = 20, points = 20)))
test_afr_eff <- Effect("afr_adj", test_global_afr_f, xlevels = afr_diffs)
plot(test_afr_eff, ylim=c(0,1), rug=FALSE, xlab = "AF(global) - AF(afr)", ylab = "Probability", main="Global + African", par.settings = list(fontsize = list(text = 20, points = 20)))


test_ancestry_eff <- Effect("af_adj", test_ancestry_f, xlevels = adj_fs)
plot(test_ancestry_eff, ylim=c(0,1), rug=FALSE, color="blue", xlab = "AF(global)", ylab = "Probability", main="Ancestry controlled", par.settings = list(fontsize = list(text = 20, points = 20)))
test_nfe_eff <- Effect("nfe_adj", test_ancestry_f, xlevels = nfe_diffs)
plot(test_nfe_eff, ylim=c(0,1), rug=FALSE, color="brown", xlab = "AF(global)-AF(nfe)", ylab = "Probability", main="Ancestry controlled", par.settings = list(fontsize = list(text = 20, points = 20)))
test_afr_eff <- Effect("afr_adj", test_ancestry_f, xlevels = afr_diffs)
plot(test_afr_eff, ylim=c(0,1), rug=FALSE, color="purple", xlab = "AF(global)-AF(afr)", ylab = "Probability", main="Ancestry controlled", par.settings = list(fontsize = list(text = 20, points = 20)))
test_amr_eff <- Effect("amr_adj", test_ancestry_f, xlevels = amr_diffs)
plot(test_amr_eff, ylim=c(0,1), rug=FALSE, color="darkgreen", xlab = "AF(global)-AF(amr)", ylab = "Probability", main="Ancestry controlled", par.settings = list(fontsize = list(text = 20, points = 20)))
test_sas_eff <- Effect("sas_adj", test_ancestry_f, xlevels = sas_diffs)
plot(test_sas_eff, ylim=c(0,1), rug=FALSE, color="orange", xlab = "AF(global)-AF(sas)", ylab = "Probability", main="Ancestry controlled", par.settings = list(fontsize = list(text = 20, points = 20)))


### STOP HERE ####


library(ROCR)
# Type 4: Hard misclassiication error
fitted_global_f <- predict(test_global_f, newdata = clinvar, type = "class", se = TRUE)
fitted_global_afr_f <- predict(test_global_afr_f, newdata = clinvar, type = "class", se = TRUE)
fitted_ancestry_f <- predict(test_ancestry_f, newdata = clinvar, type = "class", se = TRUE)
# pr <- prediction(fitted_nfe, clinvar$CLNSIG)
misClasificError_global_f <- mean(fitted_global_f != clinvar$ACMG)
misClasificError_global_afr_f <- mean(fitted_global_afr_f != clinvar$ACMG)
misClasificError_ancestry_f <- mean(fitted_ancestry_f != clinvar$ACMG)
print(paste('Accuracy(AF)',1- misClasificError_global_f,'   ','Accuracy(AF+AFR)',1- misClasificError_global_afr_f, 'Accuracy(AF+race)',1- misClasificError_ancestry_f))

# Type 5: Soft misclassification error
real_class <- model.matrix(~ 0 + ACMG, clinvar)
softError_baseline <- sum((1-real_class)*proportions)/total; #average misclassification per variants
fitted_global_f <- predict(test_global_f, newdata = clinvar, type = "probs", se = TRUE)
fitted_global_afr_f <- predict(test_global_afr_f, newdata = clinvar, type = "probs", se = TRUE)
fitted_ancestry_f <- predict(test_ancestry_f, newdata = clinvar, type = "probs", se = TRUE)
colnames(real_class) <- colnames(fitted_global_f)
softError_global_f <- sum((1-real_class)*fitted_global_f)/total #average misclassification per variant if you use AF diff encoding
softError_global_afr_f <- sum((1-real_class)*fitted_global_afr_f)/total
softError_ancestry_f <- sum((1-real_class)*fitted_ancestry_f)/total
print(paste(softError_baseline,softError_global_f,softError_global_afr_f, softError_ancestry_f))

#Besides analytical significance (assumptions violated), use the following for model validation

#1. Cross validation for signification of the predictor
require(caret)
set.seed(3456)

for(run in 1:10) {
  # First, train
  trainIndex <- createDataPartition(clinvar$CLNSIG, p = .8, list=FALSE, times=1)

  test_global_f <- multinom(ACMG ~ af_adj, data = clinvar[trainIndex,])
  test_global_afr_f <- multinom(ACMG ~ af_adj + afr_adj, data = clinvar[trainIndex,])
  test_ancestry_f <- multinom(ACMG ~ af_adj + nfe_adj + afr_adj + amr_adj + sas_adj, data = clinvar[trainIndex,])

  z_global_f <- summary(test_global_f)$coefficients/summary(test_global_f)$standard.errors
  z_global_afr_f <- summary(test_global_afr_f)$coefficients/summary(test_global_afr_f)$standard.errors
  z_ancestry_f <- summary(test_ancestry_f)$coefficients/summary(test_ancestry_f)$standard.errors

  p_global_f <- (1 - pnorm(abs(z_global_f), 0, 1))*2
  p_global_afr_f <- (1 - pnorm(abs(z_global_afr_f), 0, 1))*2
  p_ancestry_f <- (1 - pnorm(abs(z_ancestry_f), 0, 1))*2

  adj_fs <- data.frame(af_adj = c(0:100)/100)
  nfe_diffs <- data.frame(nfe_adj = 2*c(-50:50)/100)
  afr_diffs <- data.frame(afr_adj = 2*c(-50:50)/100)
  amr_diffs <- data.frame(amr_adj = 2*c(-50:50)/100)
  sas_diffs <- data.frame(sas_adj = 2*c(-50:50)/100)
  afr_data <- cbind(adj_fs, afr_diffs)
  ancestry_data <- cbind(adj_fs, nfe_diffs, afr_diffs, amr_diffs, sas_diffs)

  global_preds <- cbind(adj_fs, predict(test_global_f, newdata = adj_fs, type = "probs", se = TRUE))
  global_afr_preds <- cbind(afr_data, predict(test_global_afr_f, newdata = afr_data, type = "probs", se = TRUE))
  ancestry_preds <- cbind(ancestry_data, predict(test_ancestry_f, newdata = ancestry_data, type = "probs", se = TRUE))

  # The test
  fitted_global_f <- predict(test_global_f, newdata = clinvar[-trainIndex,], type = "class", se = TRUE)
  fitted_global_afr_f <- predict(test_global_afr_f, newdata = clinvar[-trainIndex,], type = "class", se = TRUE)
  fitted_ancestry_f <- predict(test_ancestry_f, newdata = clinvar[-trainIndex,], type = "class", se = TRUE)

  # pr <- prediction(fitted_nfe, clinvar$CLNSIG)
  misClasificError_global_f <- mean(fitted_global_f != clinvar[-trainIndex,]$ACMG)
  misClasificError_global_afr_f <- mean(fitted_global_afr_f != clinvar[-trainIndex,]$ACMG)
  misClasificError_ancestry_f <- mean(fitted_ancestry_f != clinvar[-trainIndex,]$ACMG)
  print(paste('Accuracy(AF)',1- misClasificError_global_f,'   ','Accuracy(AF+AFR)',1- misClasificError_global_afr_f, 'Accuracy(AF+race)',1- misClasificError_ancestry_f))

  #perform[run,] = c()

}



#2. Permutation testing for significance of predictor
# First, establish a baseline. Filter out the histogram peak at AF_diff = 0. Only predict at hugely divergent variants.
clinvar_new <- clinvar # clinvar[which(abs(clinvar$nfe_adj)>0.05 | abs(clinvar$afr_adj)>0.05 | abs(clinvar$amr_adj)>0.05  | abs(clinvar$sas_adj)>0.05), ]

total <- sum(summary(clinvar$ACMG))
proportions <- summary(clinvar$ACMG)/total
proportions
proportions <- matrix(data=proportions, nrow=dim(clinvar)[1], ncol=length(levels(clinvar$ACMG)), byrow = "TRUE")
randomExp <- sum(proportions^2) #p^2 + q^2 + r^2 is prob of randomly picking correct class

real_class <- model.matrix( ~ 0 + ACMG, clinvar)
colnames(real_class) <- colnames(fitted_global_f)
softErrorIter_baseline <- sum((1-real_class)*proportions)/total; #average misclassification per variants

# We do this, since we can't rely on normal assumption of indep variable (poorly spread peaky at 0 historgram), making the Wald p-value unreliable
candidate_nfe = 0
candidate_afr = 0
candidate_amr = 0
candidate_sas = 0
for(run in 1:10000) {
  clinvar_new <- clinvar
  clinvar_new$ACMG <- clinvar_new$ACMG[sample(nrow(clinvar))] # clinvar[which(abs(clinvar$nfe_adj)>0.05 | abs(clinvar$afr_adj)>0.05 | abs(clinvar$amr_adj)>0.05  | abs(clinvar$sas_adj)>0.05), ]
  clinvar_new$ACMG <- relevel(clinvar_new$ACMG, ref = "VUS")
  summary(clinvar_new$ACMG)

  test_nfe_f_new <- multinom(ACMG ~ af_nfe, data = clinvar_new)
  test_nfe_both_new <- multinom(ACMG ~ af_nfe + nfe_adj, data = clinvar_new)
  fitted_nfe_f_new <- predict(test_nfe_f_new, newdata = clinvar_new, type = "probs", se = TRUE)
  fitted_nfe_both_new <- predict(test_nfe_both_new, newdata = clinvar_new, type = "probs", se = TRUE)
  #test_nfe_eff_new <- Effect("nfe_adj", test_nfe_new, xlevels = nfe_diffs)

  test_afr_f_new <- multinom(ACMG ~ af_afr, data = clinvar_new)
  test_afr_both_new <- multinom(ACMG ~ af_afr + afr_adj, data = clinvar_new)
  fitted_afr_f_new <- predict(test_afr_f_new, newdata = clinvar_new, type = "probs", se = TRUE)
  fitted_afr_both_new <- predict(test_afr_both_new, newdata = clinvar_new, type = "probs", se = TRUE)
  #test_afr_eff_new <- Effect("afr_adj", test_afr_new, xlevels = afr_diffs)

  test_amr_f_new <- multinom(ACMG ~ af_amr, data = clinvar_new)
  test_amr_both_new <- multinom(ACMG ~ af_amr + amr_adj, data = clinvar_new)
  fitted_amr_f_new <- predict(test_amr_f_new, newdata = clinvar_new, type = "probs", se = TRUE)
  fitted_amr_both_new <- predict(test_amr_both_new, newdata = clinvar_new, type = "probs", se = TRUE)
  #test_amr_eff_new <- Effect("amr_adj", test_amr_new, xlevels = amr_diffs)

  test_sas_f_new <- multinom(ACMG ~ af_sas, data = clinvar_new)
  test_sas_both_new <- multinom(ACMG ~ af_sas + sas_adj, data = clinvar_new)
  fitted_sas_f_new <- predict(test_sas_f_new, newdata = clinvar_new, type = "probs", se = TRUE)
  fitted_sas_both_new <- predict(test_sas_both_new, newdata = clinvar_new, type = "probs", se = TRUE)
  #test_sas_eff_new <- Effect("sas_adj", test_sas_new, xlevels = sas_diffs)

  #average misclassification per variant if you use AF diff encoding
  softError_nfe_f_new <- sum((1-real_class)*fitted_nfe_f_new)/total
  softError_nfe_both_new <- sum((1-real_class)*fitted_nfe_both_new)/total

  softError_afr_f_new <- sum((1-real_class)*fitted_afr_f_new)/total
  softErrorIter_amr <- sum((1-real_class)*fitted_amr_new)/total
  softErrorIter_sas <- sum((1-real_class)*fitted_sas_new)/total

  # print(paste(softErrorIter_baseline,softErrorIter_nfe,softErrorIter_baseline-softErrorIter_nfe))
  if(softErrorIter_baseline-softError_nfe_f_new > softError_baseline-softError_nfe_f)
    candidate_nfe = candidate_nfe+1

  if(softErrorIter_baseline-softErrorIter_afr > softError_baseline-softError_afr)
    candidate_afr = candidate_afr+1

  if(softErrorIter_baseline-softErrorIter_amr > softError_baseline-softError_amr)
    candidate_amr = candidate_amr+1

  if(softErrorIter_baseline-softErrorIter_sas > softError_baseline-softError_sas)
    candidate_sas = candidate_sas+1

  print(c(run, candidate_nfe, candidate_afr, candidate_amr, candidate_sas))
}
#print(candidate/10)

#Plot null expectations
temp <- test_nfe_eff
temp$prob <- proportions[1:(length(test_nfe_eff$prob)/3),]
temp$lower.prob <- proportions[1:(length(test_nfe_eff$prob)/3),]-0.01
temp$upper.prob <- proportions[1:(length(test_nfe_eff$prob)/3),]+0.01
plot(temp, ylim=c(0,1), lty=2, xlab = "AF(pop) - AF(global)", ylab = "Probability", rug="FALSE", main="Null Hypothesis", par.settings = list(fontsize = list(text = 20, points = 20)))

# Try out hard assignment statistics - waste
a <- fitted_nfe != clinvar_new$ACMG
a <- a*1 # convert TRUE/FALSE to 1/0
sum(a*clinvar_new$af_nfe)
b <- clinvar_new$ACMG != "B or LB"
b <- b*1
sum(b*clinvar_new$af_nfe)


temp = clinvar_new$ACMG[sample(nrow(clinvar_new))]
head(clinvar_new$ACMG)
