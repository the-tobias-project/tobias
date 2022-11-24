
#' fit_global_pop
#'
#'@description
#' contains logic to FIT the regression models
#' @param clinvar
#' @param mypops
#' @export

fit_global_pop <- function(clinvar, mypops) {
    if (is.null(mypops)) {
        model <- multinom(ACMG ~ af_adj, data = clinvar, trace = FALSE)
    } else {
        xnam <- paste("d", mypops, sep = "_")
        pops.formula <- paste(xnam, collapse = "+")
        fmla <- as.formula(paste("ACMG ~ af_adj +", pops.formula))
        model <- multinom(fmla, data = clinvar, trace = FALSE)
    }
    model
}

#' plotGlobalEffects
#'
#'@description
#' Contains logic to PLOT the regression models. Plot for Global Effects
#' @param test_global_f
#' @export

plotGlobalEffects <- function(test_global_f) {
    adj_fs <- data.frame(af_adj = c(0:100)/100)  # Lines between 0 and 1
    test_global_eff <- Effect("af_adj", test_global_f, xlevels = adj_fs)
    plot(test_global_eff, xlim = c(0, 1), ylim = c(0, 1), rug = FALSE, xlab = "AF(global)",
        ylab = "Probability", main = "Global AFs", par.settings = list(fontsize = list(text = 20,
            points = 20)))
}


#' plotPopEffects
#'
#'@description
#' Contains logic to PLOT the regression models. Plot for Global + Population Effects
#' @param mypops
#' @param popLabel
#' @param popColor
#' @param model
#' @export

plotPopEffects <- function(mypops, popLabel, popColor, model) {


    if (is.null(mypops)) {
        adj_fs <- data.frame(af_adj = c(0:100)/100)  # Lines between 0 and 1
        test_model <- Effect("af_adj", model, xlevels = adj_fs)
        myXLab = "AF(global)"
        mainLabel = "Global AFs"
        plot(test_model, xlim = c(0, 1), ylim = c(0, 1), rug = FALSE, color = popColor,
            xlab = myXLab, ylab = "Probability", main = mainLabel, par.settings = list(fontsize = list(text = 20,
                points = 20)))
    } else {
        adj_fs <- data.frame(af_adj = 2 * c(-50:50)/100)  # Lines between -1 and 1
        d_pop <- paste("d", mypops, sep = "_")
        test_model <- Effect(d_pop, model, xlevels = adj_fs)
        myXLab = paste("AF(global) - AF(", paste(mypops, collapse = "+"), ")", sep = "")
        mainLabel = paste("Global + ", popLabel, sep = "")
        plot(test_model, xlim = c(-1, 1), ylim = c(0, 1), rug = FALSE, color = popColor,
            xlab = myXLab, ylab = "Probability", main = mainLabel, par.settings = list(fontsize = list(text = 20,
                points = 20)))
    }
}

#' plotAncestryEffects
#'
#'@description
#' Contains logic to PLOT the regression models. Plot for Global + Ancestry Effects
#' @param mypops
#' @param popLabel
#' @param popColor
#' @param model
#' @export

plotAncestryEffects <- function(mypops, popLabel, popColor, model) {
    pop_diffs <- data.frame(pop_adj = 2 * c(-50:50)/100)
    d_pop <- paste("d", mypops, sep = "_")
    test_pop_eff <- Effect(d_pop, model, xlevels = pop_diffs)
    myXLab = paste("AF(global) - AF(", popLabel, ")", sep = "")
    mainLabel = paste("Global + ", popLabel, sep = "")
    plot(test_pop_eff, xlim = c(-1, 1), ylim = c(0, 1), rug = FALSE, color = popColor,
        xlab = myXLab, ylab = "Probability", main = mainLabel, par.settings = list(fontsize = list(text = 20,
            points = 20)))
}


#' getZscore
#'
#'@description
#' Contains logic to TEST the regression models (permutation tests, statistical significance, prediction power, etc.)
#' @param model
#' @export

getZscore <- function(model) {
    ztable = summary(model)$coefficients/summary(model)$standard.errors
    ztable
}

#' getPvalue
#'
#'@description
#' P-value (from z-score)
#' @param zscore
#' @export

getPvalue <- function(zscore) {
    p_value <- (1 - pnorm(abs(zscore), 0, 1)) * 2
    p_value
}

#' getHardError
#'
#'@description
#' Hard missclassification error
#' @param model
#' @param clinvar
#' @export

getHardError <- function(model, clinvar) {
    fitted_model <- predict(model, newdata = clinvar, type = "class", se = TRUE)
    hardError <- mean(fitted_model != clinvar$ACMG)
    hardError
}


#' getSoftError
#'
#'@description
#' Soft missclassification error
#' @param model
#' @param clinvar
#' @export

getSoftError <- function(model, clinvar) {
    softError = c(0, 0)
    real_class <- model.matrix(~0 + ACMG, clinvar)
    fitted_model <- predict(model, newdata = clinvar, type = "probs", se = TRUE)
    total <- sum(summary(clinvar$ACMG))
    proportions <- summary(clinvar$ACMG)/total

    # Soft Error Baseline
    softError[1] <- sum((1 - real_class) * proportions)/total  #average misclassification per variants
    # Soft Error
    softError[2] <- sum((1 - real_class) * fitted_model)/total  #average misclassification per variant if you use AF diff encoding

    softError
}


#' calculateCrossValidation
#'
#'@description
#' Cross validation
#' @param percentage
#' @param pops
#' @param clinvar
#' @export

calculateCrossValidation <- function(percentage, pops, clinvar) {
    require(caret)
    set.seed(3456)

    accuracy = 0
    reps = 10
    percentage = percentage/100  #to transform from 10% to 0.1

    for (run in 1:reps) {
        # First, select the partitions
        trainIndex <- createDataPartition(clinvar$CLNSIG, p = percentage, list = FALSE,
            times = 1)

        # Train the model
        model = fit_global_pop(clinvar[trainIndex, ], pops)

        # The test
        fitted_model <- predict(model, newdata = clinvar[-trainIndex, ], type = "class",
            se = TRUE)
        misClasificError <- mean(fitted_model != clinvar[-trainIndex, ]$ACMG)

        accuracy = accuracy + misClasificError

    }

    accuracy = accuracy/reps

    accuracy

}

#' calculatePermutationTesting
#'
#'@description
#' Permutation testing for significance of predictor
#' @param baseline.pops
#' @param model.pops
#' @param numberOfPermutations
#' @param clinvar
#' @export

calculatePermutationTesting <- function(baseline.pops, model.pops, numberOfPermutations,
    clinvar) {

    randomModelMoreSignificant = 0

    # 0. Calculate the model proportions Train the baseline
    baseline = fit_global_pop(clinvar, baseline.pops)
    softError = getSoftError(model, clinvar)
    softError_apriori = softError[1]
    softError_model = softError[2]


    for (run in 1:numberOfPermutations) {
        clinvar_new <- clinvar

        # 1. Shuffle the ACMG labels
        clinvar_new$ACMG <- clinvar_new$ACMG[sample(nrow(clinvar))]
        clinvar_new$ACMG <- relevel(clinvar_new$ACMG, ref = "VUS")
        real_class <- model.matrix(~0 + ACMG, clinvar_new)

        # 2. Train a new multinomial model based on a shuffled dataset
        model <- fit_global_pop(clinvar_new, model.pops)
        softErrorIter = getSoftError(model, clinvar)
        softErrorIter_apriori = softErrorIter[1]
        softErrorIter_model = softErrorIter[2]

        # 3. Count if it is more extreme than the original data
        if (softError_apriori - softError_model < softErrorIter_apriori - softErrorIter_model) {
            randomModelMoreSignificant = randomModelMoreSignificant + 1
        }
    }

    randomModelMoreSignificant/numberOfPermutations


}
