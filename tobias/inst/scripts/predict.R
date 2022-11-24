#!/usr/bin/Rscript

#---------------------------------------------------------------
# Project: Ancestry Bias in ClinVar (w.r.t ExAC population data)
# Author: Snehit Prabhu <snehit@stanford.edu>
#---------------------------------------------------------------

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
