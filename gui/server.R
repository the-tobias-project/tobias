#---------------------------------------------------------------
# Project: Ancestry Bias in ClinVar (w.r.t ExAC population data)
# Author: Arturo Lopez Pineda <arturolp@stanford.edu>
# With code from: Snehit Prabhu <snehit@stanford.edu>
# Date: April 20, 2017
#---------------------------------------------------------------

library(shinydashboard)

#---------------------------
#Load once when app is launched
#---------------------------

# Load Scripts
source("../R/loadData.R")
source("../R/featurizeAFs.R")
source("../R/exploratoryTests.R")
source("../R/effects.R")

# Settings
pops <- data.frame(rbind(
  c("afr", "African", "purple"),
  c("amr", "American", "darkgreen"),
  c("eas", "East Asian", "orange"),
  c("sas", "South Asian", "gold"),
  c("fin", "Finn", "pink"),
  c("nfe", "Non-Finn European", "brown"),
  c("oth", "Other", "tomato")
))
colnames(pops) <- c("symbol", "label", "color")


popsMultiChoice <- list(
  "African" = "afr",
  "American" = "amr",
  "East Asian" = "eas",
  "South Asian" = "sas",
  "Finn" ="fin",
  "Non-Finn European" = "nfe",
  "Other" = "oth"
)


# Load Data
clinvar <- readData("../inputs/clinvar.exac.variants.gene.submission.diseases.alleles.tab")
clinvar$ACMG <- relevel(clinvar$CLNSIG, ref = "VUS")
populations <- as.vector(pops$symbol)
dataset <- setAFs(clinvar, c("adj", populations))
dataset <- setResidualAFs(dataset, c("adj", populations))


# Support Functions
getLabel <- function(id_label){
  ids = which(pops$symbol %in% id_label)
  return(as.character(pops[ids, "label"]))
}

getColor <- function(id_label){
  ids = which(pops$symbol %in% id_label)
  return(as.character(pops[ids, "color"]))
}


#---------------------------
# Define server logic
#---------------------------

shinyServer(function(input, output) {

  # 1A. Histogram Population Allele Frequency
  #---------------------------
  output$histogramPAF <- renderPlot({
    histAFs(dataset, input$pop_hist, getColor(input$pop_hist))
  })

  # 1B. Histogram Global Allele Frequency
  #---------------------------
  output$histogramGAF <- renderPlot({
    histResidualAFs(dataset, input$pop_hist, getColor(input$pop_hist))
  })

  # 2. Scatter Plot
  #---------------------------
  output$scatterAF <- renderPlot({

    af_label <- paste("af_", input$pop_scatter, sep="")
    colorcode<-c("blue","lightgreen","red")
    title_label <- paste("All vs. ", getLabel(input$pop_scatter), sep = "")
    y_label <- paste("AF(", getLabel(input$pop_scatter) ,")", sep="")

    scatterPlot(dataset, "af_adj", af_label, "ACMG", colorcode, title_label, "AF(global)", y_label)

  })


  # 3. Enrichment Plot
  #---------------------------
  output$enrichment <- renderPlot({

    af_label <- paste("af_", input$pop_enrichement, sep="")
    colorcode<-c("blue","lightgreen","red")
    title_label <- paste("All vs. ", getLabel(input$pop_enrichement), sep = "")
    y_label <- paste("AF(", getLabel(input$pop_enrichement) ,")", sep="")

    testEnrichment(dataset, "af_adj", af_label, colorcode, title_label, "AF(global)", y_label)

  })


  # 4.0 Render UI for Effects Tab
  #---------------------------
  output$choose_pop_effect <- renderUI({
    pop <- input$pop_effects
    ch <- popsMultiChoice[which(popsMultiChoice!=pop)]
    checkboxGroupInput("multi_pop_effect", "Other population", choices = ch, selected = ch[1])
  })

  output$multi_pop_effect <- renderUI({
    mypops = c(input$pop_effects, input$multi_pop_effect)
      boxTitle = paste(c("Test of Global", getLabel(mypops)), collapse=" + ")
    box(title = boxTitle, status = "primary", solidHeader = TRUE, collapsible = TRUE)
  })

  # 4.A Global Effects
  #---------------------------
  model.global <- reactive({
    model.global = fit_global_model(dataset)
  })

  output$globalEffects <- renderPlot({
    plotGlobalEffects(model.global())
  })

  output$globalLabel <- renderText({
    model <- model.global()
    #zscore.global = getZscore(model)
    #pvalue.global = getPvalue(zscore.global)
    hardError = getHardError(model, dataset)
    softError = getSoftError(model, dataset)
    paste("Classification error (hard) = ", format(hardError, digits=3), ", ",
          "Classification error (soft) = ", format(softError[2], digits=3), ", baseline = ", format(softError[1], digits=3),
          collapse="")
  })

  calculateCVglobal <- eventReactive(input$cvButtonGlobal, {
    accuracy = calculateCrossValidation(input$cvInputGlobal, NULL, dataset)
    paste("Accuracy = ", format(accuracy, digits=3), sep="")
  })

  output$globalLabelCV <- renderText({
    calculateCVglobal()
  })

  calculatePTglobal <- eventReactive(input$ptButtonGlobal, {
    paste("Hello World!")
  })

  output$globalLabelPT <- renderText({
    calculatePTglobal()
  })


  # 4.B Global and Population Effects
  #---------------------------

  model.global.pop <- reactive({
    model.global.pop <- fit_global_pop(dataset, input$pop_effects)
  })

  output$globalPopEffects <- renderPlot({
    model <- model.global.pop()
    plotPopEffects(input$pop_effects, getLabel(input$pop_effects), getColor(input$pop_effects), model)
  })

  output$globalPopLabel <- renderText({
    model <- model.global.pop()
    #zscore.global = getZscore(model)
    #pvalue.global = getPvalue(zscore.global)
    hardError = getHardError(model, dataset)
    softError = getSoftError(model, dataset)
    paste("Classification error (hard) = ", format(hardError, digits=3), ", ",
          "Classification error (soft) = ", format(softError[2], digits=3), ", baseline = ", format(softError[1], digits=3),
          collapse="")
  })


  calculateCVglobalPop <- eventReactive(input$cvButtonGlobalPop, {
    accuracy = calculateCrossValidation(input$cvInputGlobalPop, input$pop_effects, dataset)
    paste("Accuracy = ", format(accuracy, digits=3), sep="")
  })

  output$globalPopLabelCV <- renderText({
    calculateCVglobalPop()
  })


  # 4.C Global and Multiple Populations Effects
  #---------------------------
  model.multi <- reactive({
    mypops = c(input$pop_effects, input$multi_pop_effect)
    model.multi <- fit_global_pop(dataset, mypops)
  })

  output$globalMultiPopEffects <- renderPlot({
    mypops = c(input$pop_effects, input$multi_pop_effect)
    model <- model.multi()
    multiLabel = paste(getLabel(mypops), collapse=" + ")
    plotAncestryEffects(input$pop_effects, input$multi_pop_effect, multiLabel, getColor(input$pop_effects), model)
  })

  output$globalMultiLabel <- renderText({
    model <- model.multi()
    #zscore.global = getZscore(model)
    #pvalue.global = getPvalue(zscore.global)
    hardError = getHardError(model, dataset)
    softError = getSoftError(model, dataset)
    paste("Classification error (hard) = ", format(hardError, digits=3), ", ",
          "Classification error (soft) = ", format(softError[2], digits=3), ", baseline = ", format(softError[1], digits=3),
          collapse="")
  })

  calculateCVglobalMultiPop <- eventReactive(input$cvButtonGlobalMultiPop, {
    mypops = c(input$pop_effects, input$multi_pop_effect)
    accuracy = calculateCrossValidation(input$cvInputGlobalMultiPop, mypops, dataset)
    paste("Accuracy = ", format(accuracy, digits=3), sep="")
  })

  output$globalMultiPopLabelCV <- renderText({
    calculateCVglobalMultiPop()
  })



})


