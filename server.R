#---------------------------------------------------------------
# Project: Ancestry Bias in ClinVar (w.r.t ExAC population data)
# Author: Arturo Lopez Pineda <arturolp@stanford.edu>
# With code from: Snehit Prabhu <snehit@stanford.edu>
# Date: April 20, 2017
#---------------------------------------------------------------

library(shinydashboard)
library(knitr)
library(rmarkdown)

#---------------------------
#Load once when app is launched
#---------------------------

# Load Scripts
source("R/loadData.R")
source("R/featurizeAFs.R")
source("R/exploratoryTests.R")
source("R/effects.R")

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


# Load Data
clinvar <- readData("inputs/clinvar.exac.variants.gene.submission.diseases.alleles.tab")
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
  if(is.null(id_label)){
    return("black")
  } else{
    ids = which(pops$symbol %in% id_label)
    return(as.character(pops[ids, "color"]))
  }
}


#---------------------------
# Define server logic
#---------------------------

shinyServer(function(input, output) {


  # 1A. Histogram Population Allele Frequency
  #---------------------------
  output$histogramPAF <- renderPlot({
    histAFs(dataset, input$pop_explore, getColor(input$pop_explore))
  })

  # 1B. Histogram Global Allele Frequency
  #---------------------------
  output$histogramGAF <- renderPlot({
    histResidualAFs(dataset, input$pop_explore, getColor(input$pop_explore))
  })

  # 2. Scatter Plot
  #---------------------------
  output$scatterAF <- renderPlot({

    af_label <- paste("af_", input$pop_explore, sep="")
    colorcode<-c("blue","lightgreen","red")
    title_label <- paste("All vs. ", getLabel(input$pop_explore), sep = "")
    y_label <- paste("AF(", getLabel(input$pop_explore) ,")", sep="")

    scatterPlot(dataset, "af_adj", af_label, "ACMG", colorcode, title_label, "AF(global)", y_label)

  })


  # 3. Enrichment Plot
  #---------------------------
  output$enrichment <- renderPlot({

    af_label <- paste("af_", input$pop_explore, sep="")
    colorcode<-c("blue","lightgreen","red")
    title_label <- paste("All vs. ", getLabel(input$pop_explore), sep = "")
    y_label <- paste("AF(", getLabel(input$pop_explore) ,")", sep="")

    testEnrichment(dataset, "af_adj", af_label, colorcode, title_label, "AF(global)", y_label)

  })


  # 4. Global and Multiple Populations Effects
  #---------------------------
  model.multi.baseline <- reactive({
    mypops = input$pop_predict_baseline
    model.multi <- fit_global_pop(dataset, mypops)
  })

  model.multi.alternative <- reactive({
    mypops = input$pop_predict_model
    model.multi <- fit_global_pop(dataset, mypops)
  })

  output$effectsBaseline <- renderPlot({
    mypops = input$pop_predict_baseline
    model <- model.multi.baseline()
    multiLabel = paste(getLabel(mypops), collapse=" + ")
    myColors = getColor(mypops)[1]
    plotPopEffects(mypops, multiLabel, myColors, model)
  })

  output$effectsModel <- renderPlot({
    mypops = input$pop_predict_model
    model <- model.multi.alternative()
    multiLabel = paste(getLabel(mypops), collapse=" + ")
    myColors = getColor(mypops)[1]
    plotPopEffects(mypops, multiLabel, myColors, model)
  })

  output$labelBaseline <- renderUI({
    model <- model.multi.baseline()
    hardError = getHardError(model, dataset)
    softError = getSoftError(model, dataset)
    hard <-  paste("Classification error (hard) = ", format(hardError, digits=3))
    soft <- paste("Classification error (soft) = ", format(softError[2], digits=3))
    baseline <- paste("Classification error (a priori) = ", format(softError[1], digits=3))
    HTML(paste(hard, soft, baseline, sep = "<br />"))
  })


  output$labelModel <- renderUI({
    model <- model.multi.alternative()
    hardError = getHardError(model, dataset)
    softError = getSoftError(model, dataset)
    hard <-  paste("Classification error (hard) = ", format(hardError, digits=3))
    soft <- paste("Classification error (soft) = ", format(softError[2], digits=3))
    baseline <- paste("Classification error (a priori) = ", format(softError[1], digits=3))
    HTML(paste(hard, soft, baseline, sep = "<br />"))
  })

  #-----
  # Calculate Cross Validation
  #-----
  calculateCVbaseline <- eventReactive(input$cvButtonBaseline, {
    mypops = input$pop_predict_baseline
    accuracy = calculateCrossValidation(input$cvInputBaseline, mypops, dataset)*100
    paste("Misclassification Error = ", format(accuracy, digits=3), " %", sep="")
  })

  output$cvResultBaseline <- renderText({
    calculateCVbaseline()
  })

  calculateCVmodel <- eventReactive(input$cvButtonModel, {
    mypops = input$pop_predict_model
    accuracy = calculateCrossValidation(input$cvInputModel, mypops, dataset)*100
    paste("Misclassification Error = ", format(accuracy, digits=3), " %", sep="")
  })

  output$cvResultModel <- renderText({
    calculateCVmodel()
  })

  #-----
  # Calculate Permutation Testing
  #-----
  calculatePT <- eventReactive(input$buttonPT, {
    baseline = input$pop_predict_baseline
    model = input$pop_predict_model
    numberOfPermutations = input$sliderPT
    pvalue = calculatePermutationTesting(baseline, model, numberOfPermutations, dataset)
    paste("P-Value = ", format(pvalue, digits=3), sep="")
  })

  output$ptResult <- renderText({
    calculatePT()
  })



  # 5. Generate Reports
  #---------------------------


  # output$reportPredict <- downloadHandler(
  #   filename = "myreportpredict.pdf",
  #   content = function(file) {
  #     out = knit2pdf('input.Rnw', clean = TRUE)
  #     file.rename(out, file)
  #   },
  #
  #   contentType = 'application/pdf'
  # )


  output$reportPredict <- downloadHandler(
    filename = "my-report-predict.pdf",

    content = function(file) {

      # Set up parameters
      params <- list(pop1 = getLabel(input$pop_predict_baseline),
                     pop2 = getLabel(input$pop_predict_model)
                    )

      # Knit the document
      rmarkdown::render("report.Rmd", output_format="pdf_document",
                        params = params,
                        output_file = file,
                        envir = new.env())
    }

  )



})


