#!/usr/bin/Rscript

#---------------------------------------------------------------
# Project: Ancestry Bias in ClinVar (w.r.t ExAC population data)
# Author: Snehit Prabhu <snehit@stanford.edu>
# Contributor: Arturo Lopez Pineda <arturolp@stanford.edu>
#---------------------------------------------------------------


#--------
# A. contains logic to FIT the regression models

# Part 1. Create 3 models
fit_global_model <- function(clinvar){
  test_global_f <- multinom(ACMG ~ af_adj, data = clinvar, trace=FALSE)
  return(test_global_f)
}


fit_global_pop <- function(clinvar, pop){
  xnam <- paste("d", pop, sep="_")
  fmla <- as.formula(paste("ACMG ~ af_adj +", paste(xnam, collapse= "+")))
  test_global_pop_f <- multinom(fmla, data = clinvar, trace=FALSE)
  return(test_global_pop_f)
}


#--------
# B. Contains logic to PLOT the regression models

# 1. Plot for Global Effects
plotGlobalEffects <- function(test_global_f){
  adj_fs <- data.frame(af_adj = c(0:100)/100) # Lines between 0 and 1
  test_global_eff <- Effect("af_adj", test_global_f, xlevels = adj_fs)
  plot(test_global_eff, xlim=c(0,1), ylim=c(0,1), rug=FALSE, xlab = "AF(global)", ylab = "Probability", main="Global AFs", par.settings = list(fontsize = list(text = 20, points = 20)))
}


# 2. Plot for Global + Population Effects
plotPopEffects <- function(pop, popLabel, popColor, test_global_pop_f){
  pop_diffs <- data.frame(pop_adj = 2*c(-50:50)/100)
  d_pop <- paste("d", pop, sep="_")
  test_pop_eff <- Effect(d_pop, test_global_pop_f, xlevels = pop_diffs)
  myXLab = paste("AF(global) - AF(", pop , ")", sep="")
  mainLabel = paste("Global + ", popLabel, sep="")
  plot(test_pop_eff, xlim=c(-1,1), ylim=c(0,1), rug=FALSE, color=popColor, xlab = myXLab, ylab = "Probability", main=mainLabel, par.settings = list(fontsize = list(text = 20, points = 20)))
}


# 3. Plot for Global + Ancestry Effects
plotAncestryEffects <- function(pop, other_pops, popLabel, popColor, test_ancestry_f){
  attach(mtcars)
  par(mfrow=c(3,1))
  pop_diffs <- data.frame(pop_adj = 2*c(-50:50)/100)
  d_pop <- paste("d", pop, sep="_")
  test_pop_eff <- Effect(d_pop, test_ancestry_f, xlevels = pop_diffs)
  myXLab = paste("AF(global) - AF(", pop , ")", sep="")
  mainLabel = paste("Global + ", popLabel, sep="")
  plot(test_pop_eff, xlim=c(-1,1), ylim=c(0,1), rug=FALSE, color=popColor, xlab = myXLab, ylab = "Probability", main = mainLabel, par.settings = list(fontsize = list(text = 20, points = 20)))
}

#--------
# C. Contains logic to TEST the regression models
# (permutation tests, statistical significance, prediction power, etc.)

# 1. Z-score
getZscore <- function(model){
  ztable = summary(model)$coefficients/summary(model)$standard.errors
  return(ztable)
}

# 2. P-value (from z-score)
getPvalue <- function(zscore){
  p_value <- (1 - pnorm(abs(zscore), 0, 1))*2
  return(p_value)
}

# 3. Hard missclassification error
getHardError <- function(model, clinvar){
  fitted_model <- predict(model, newdata = clinvar, type = "class", se = TRUE)
  hardError <- mean(fitted_model != clinvar$ACMG)
  return(hardError)
}

# 4. Soft missclassification error
getSoftError <- function(model, clinvar){
  softError = c(0,0)
  real_class <- model.matrix(~ 0 + ACMG, clinvar)
  fitted_model <- predict(model, newdata = clinvar, type = "probs", se = TRUE)
  total <- sum(summary(clinvar$ACMG))
  proportions <- summary(clinvar$ACMG)/total

  #Soft Error Baseline
  softError[1] <- sum((1-real_class)*proportions)/total;#average misclassification per variants
  #Soft Error
  softError[2] <- sum((1-real_class)*fitted_model)/total #average misclassification per variant if you use AF diff encoding

  return(softError)
}

#5. Cross validation
calculateCrossValidation <- function(percentage, pops, clinvar){
  require(caret)
  set.seed(3456)

  accuracy = 0
  reps = 10
  percentage = percentage/100 #to transform from 10% to 0.1

  for(run in 1:reps) {
    # First, train
    trainIndex <- createDataPartition(clinvar$CLNSIG, p = percentage, list=FALSE, times=1)

    if(is.null(pops)){
      model <- fit_global_model(clinvar[trainIndex,])
    }
    else{
      model = fit_global_pop(clinvar[trainIndex,], pops)
    }

    # The test
    fitted_global_f <- predict(model, newdata = clinvar[-trainIndex,], type = "class", se = TRUE)
    misClasificError <- mean(model != clinvar[-trainIndex,]$ACMG)

    accuracy = accuracy + misClasificError

  }

  accuracy = accuracy / reps

  return(accuracy)

}


#6. Permutation testing for significance of predictor
calculatePermutationTesting <- function(numberOfPermutations, pops, clinvar){

  total <- sum(summary(clinvar$ACMG))
  proportions <- summary(clinvar$ACMG)/total
  proportions <- matrix(data=proportions, nrow=dim(clinvar)[1], ncol=length(levels(clinvar$ACMG)), byrow = "TRUE")
  randomExp <- sum(proportions^2) #p^2 + q^2 + r^2 is prob of randomly picking correct class

  real_class <- model.matrix( ~ 0 + ACMG, clinvar)
  colnames(real_class) <- colnames(fitted_global_f)
  softErrorIter_baseline <- sum((1-real_class)*proportions)/total; #average misclassification per variants

  # We do this, since we can't rely on normal assumption of indep variable (poorly spread peaky at 0 historgram), making the Wald p-value unreliable
  candidate = 0
  for(run in 1:numberOfPermutations) {
    clinvar_new$ACMG <- clinvar$ACMG[sample(nrow(clinvar))] # clinvar[which(abs(clinvar$nfe_adj)>0.05 | abs(clinvar$afr_adj)>0.05 | abs(clinvar$amr_adj)>0.05  | abs(clinvar$sas_adj)>0.05), ]
    clinvar_new$ACMG <- relevel(clinvar_new$ACMG, ref = "VUS")

    #********* AF_NFE to general
    test_model <- multinom(ACMG ~ af_nfe, data = clinvar_new)
    fitted_model <- predict(test_model, newdata = clinvar_new, type = "probs", se = TRUE)

    test_afr_f_new <- multinom(ACMG ~ af_afr, data = clinvar_new)
    fitted_afr_f_new <- predict(test_afr_f_new, newdata = clinvar_new, type = "probs", se = TRUE)

    #average misclassification per variant if you use AF diff encoding
    softError_nfe_f_new <- sum((1-real_class)*fitted_nfe_f_new)/total
    softError_nfe_both_new <- sum((1-real_class)*fitted_nfe_both_new)/total

    softError_afr_f_new <- sum((1-real_class)*fitted_afr_f_new)/total
    softErrorIter_amr <- sum((1-real_class)*fitted_amr_new)/total

    # print(paste(softErrorIter_baseline,softErrorIter_nfe,softErrorIter_baseline-softErrorIter_nfe))
    if(softErrorIter_baseline-softError_nfe_f_new > softError_baseline-softError_nfe_f)
      candidate_nfe = candidate_nfe+1

    print(c(run, candidate_nfe, candidate_afr, candidate_amr, candidate_sas))
  }
  #print(candidate/10)

}
