#!/usr/bin/Rscript

#---------------------------------------------------------------
# Project: Ancestry Bias in ClinVar (w.r.t ExAC population data)
# Author: Snehit Prabhu <snehit@stanford.edu>
#---------------------------------------------------------------
setwd("/Users/snehit/dev/tobias/")

source("R/loadData.R")
clinvar <- readData("inputs/clinvar.exac.variants.gene.submission.diseases.alleles.tab")

source("R/featurizeAFs.R")
populations <- c("nfe", "afr", "amr", "sas", "eas", "fin", "oth")
clinvar <- setAFs(clinvar, c("adj", populations))
clinvar <- setResidualAFs(clinvar, populations)
pdf("outputs/af_histograms.pdf",width=16,height=8)

par(mfrow = c(2,length(populations)) )
histAFs(clinvar, populations, "gray")
histResidualAFs(clinvar, populations, "gray")
#histograms not paneling based on number of entries in "populations"
dev.off()

# Relevel dataset around VUS. Remove 0 values
colorcode<-c("blue","lightgreen","red")
clinvar$ACMG <- relevel(clinvar$CLNSIG, ref = "VUS")

#Scatter plots
pdf("outputs/af_scatters.pdf",width=16,height=16)
par(mfrow=c(2,ceiling(length(populations)/2)))
scatterPlot(clinvar, "af_adj", "af_nfe", "ACMG", colorcode, "All vs. Europeans", "AF(global)", "AF(Non-Finnish Europeans)")
scatterPlot(clinvar, "af_adj", "af_afr", "ACMG", colorcode, "All vs. Africans", "AF(global)", "AF(Africans)")
scatterPlot(clinvar, "af_adj", "af_amr", "ACMG", colorcode, "All vs. Latinos", "AF(global)", "AF(Latino)")
scatterPlot(clinvar, "af_adj", "af_sas", "ACMG", colorcode, "All vs. South Asians", "AF(global)", "AF(South Asians)")
scatterPlot(clinvar, "af_adj", "af_eas", "ACMG", colorcode, "All vs. East Asians", "AF(global)", "AF(East Asians)")
scatterPlot(clinvar, "af_adj", "af_fin", "ACMG", colorcode, "All vs. Finns", "AF(global)", "AF(Finns)")
scatterPlot(clinvar, "af_adj", "af_oth", "ACMG", colorcode, "All vs. Other", "AF(global)", "AF(Other populations)")
dev.off()

source("R/exploratoryTests.R")
pdf("outputs/test_nfe_enrichment.pdf", width=16, height=16)
testEnrichment(clinvar, "af_adj", "af_nfe", colorcode, "All vs. Europeans", "AF(global)", "AF(Non-Finnish Europeans)")
dev.off()

# Need to pass function name, need to run chi^sq test, need to plot as pdf
# Need geom_ribbon shading for transparency between lines
# From top to bottom needs to be a comparison of two populations?
