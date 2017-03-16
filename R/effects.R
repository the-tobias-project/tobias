#!/usr/bin/Rscript

#---------------------------------------------------------------
# Project: Ancestry Bias in ClinVar (w.r.t ExAC population data)
# Author: Snehit Prabhu <snehit@stanford.edu>
#---------------------------------------------------------------

#test the effects

# clinvar$nfe_adj_sq <- clinvar$nfe_adj^2
test_global_f <- multinom(ACMG ~ af_adj, data = clinvar)
test_global_afr_f <- multinom(ACMG ~ af_adj + afr_adj, data = clinvar)
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

