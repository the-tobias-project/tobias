#!/usr/bin/Rscript

#---------------------------------------------------------------
# Project: Ancestry Bias in ClinVar (w.r.t ExAC population data)
# Author: Snehit Prabhu <snehit@stanford.edu>
#---------------------------------------------------------------

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
plot(test_afr_eff, ylim=c(0,1), rug=FALSE, xlab = "AF(global)", ylab = "Probability", main="Global AFs", par.settings = list(fontsize = list(text = 20, points = 20)))


test_ancestry_eff <- Effect("af_adj", test_ancestry_f, xlevels = adj_fs)
plot(test_ancestry_eff, ylim=c(0,1), rug=FALSE, color="blue", xlab = "AF(global)", ylab = "Probability", main="Ancestry controlled", par.settings = list(fontsize = list(text = 20, points = 20)))
test_nfe_eff <- Effect("nfe_adj", test_ancestry_f, xlevels = nfe_diffs)
plot(test_nfe_eff, ylim=c(0,1), rug=FALSE, color="brown", xlab = "AF(global)", ylab = "Probability", main="Ancestry controlled", par.settings = list(fontsize = list(text = 20, points = 20)))
test_afr_eff <- Effect("afr_adj", test_ancestry_f, xlevels = afr_diffs)
plot(test_afr_eff, ylim=c(0,1), rug=FALSE, color="purple", xlab = "AF(global)", ylab = "Probability", main="Ancestry controlled", par.settings = list(fontsize = list(text = 20, points = 20)))
test_amr_eff <- Effect("amr_adj", test_ancestry_f, xlevels = amr_diffs)
plot(test_amr_eff, ylim=c(0,1), rug=FALSE, color="darkgreen", xlab = "AF(global)", ylab = "Probability", main="Ancestry controlled", par.settings = list(fontsize = list(text = 20, points = 20)))
test_sas_eff <- Effect("sas_adj", test_ancestry_f, xlevels = sas_diffs)
plot(test_sas_eff, ylim=c(0,1), rug=FALSE, color="orange", xlab = "AF(global)", ylab = "Probability", main="Ancestry controlled", par.settings = list(fontsize = list(text = 20, points = 20)))

