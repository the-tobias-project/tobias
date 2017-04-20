#!/usr/bin/Rscript

#---------------------------------------------------------------
# Project: Ancestry Bias in ClinVar (w.r.t ExAC population data)
# Author: Snehit Prabhu <snehit@stanford.edu>
# Contributor: Arturo Lopez Pineda <arturolp@stanford.edu>
#---------------------------------------------------------------


#--------
# A. contains logic to FIT the regression models

# Part 1. Create 3 models
fit_models <- function(clinvar, pop){
  assign("pop_adj", paste("d", pop, sep="_"))
  test_global_f <- multinom(ACMG ~ af_adj, data = clinvar, trace=FALSE)
  test_global_pop_f <- multinom(ACMG ~ af_adj + get(pop_adj), data = clinvar, trace=FALSE)
  test_ancestry_f <- multinom(ACMG ~ af_adj + d_nfe + d_fin + d_afr + d_amr + d_sas + d_oth + d_eas, data = clinvar, trace=FALSE)

  return(list(test_global_f=test_global_f, test_global_pop_f=test_global_pop_f, test_ancestry_f=test_ancestry_f))
}

#--------
# B. Contains logic to PLOT the regression models

plotEffects <- function(pop, popColor, test_ancestry_f){
  pop_diffs <- data.frame(2*c(-50:50)/100)
  inputLabel <- paste("d", pop, sep="_")
  test_pop_eff <- Effect(inputLabel, test_ancestry_f, xlevels = pop_diffs)
  myXlab = paste("AF(global)-AF(", pop, ")", sep="")
  plot(test_pop_eff, ylim=c(0,1), rug=FALSE, color=popColor, xlab = myXlab, ylab = "Probability", main="Ancestry controlled", par.settings = list(fontsize = list(text = 20, points = 20)))
}



