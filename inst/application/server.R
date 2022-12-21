#---------------------------------------------------------------
# Project: Ancestry Bias in ClinVar (w.r.t ExAC population data)
# Author: Arturo Lopez Pineda <arturolp@stanford.edu>
# With code from: Snehit Prabhu <snehit@stanford.edu>
# Date: April 20, 2017
#---------------------------------------------------------------

library(shinydashboard)
library(tobias)

con  <- connect_cluster()
#---------------------------
#Load once when app is launched
#---------------------------

# Load Scripts
#source("R/loadData.R")
#source("R/featurizeAFs.R")
#source("R/exploratoryTests.R")
#source("R/effects.R")

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


# Load Data. This is temporal. Change of code logic using dplyr in progress.
clinvar <- read_data(con, "tobias", "original_table") %>% collect()
clinvar <- as.data.frame(clinvar)
clinvar$CLNSIG <- as.factor(clinvar$CLNSIG)

clinvar$ACMG <- relevel(clinvar$CLNSIG, ref = "VUS")
populations <- as.vector(pops$symbol)
dataset <- set_AFs(clinvar, c("adj", populations))
dataset <- set_residual_AFs(dataset, c("adj", populations))


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
    hist_AFs(dataset, input$pop_explore, getColor(input$pop_explore))
  })

  # 1B. Histogram Global Allele Frequency
  #---------------------------
  output$histogramGAF <- renderPlot({
    hist_residual_AFs(dataset, input$pop_explore, getColor(input$pop_explore))
  })

  # 2. Scatter Plot
  #---------------------------
  output$scatterAF <- renderPlot({

    af_label <- paste("af_", input$pop_explore, sep="")
    colorcode <- c("blue","lightgreen","red")
    title_label <- paste("All vs. ", getLabel(input$pop_explore), sep = "")
    y_label <- paste("AF(", getLabel(input$pop_explore) ,")", sep="")

    scatter_plot(dataset, "af_adj", af_label, "ACMG", colorcode, title_label, "AF(global)", y_label)

  })


  # 3. Enrichment Plot
  #---------------------------
  output$enrichment <- renderPlot({

    af_label <- paste("af_", input$pop_test, sep="")
    colorcode <- c("blue","lightgreen","red")
    title_label <- paste("All vs. ", getLabel(input$pop_test), sep = "")
    y_label <- paste("AF(", getLabel(input$pop_test) ,")", sep="")

    test_enrichment(dataset, "af_adj", af_label, colorcode, title_label, "AF(global)", y_label)

  })


  # 4. Global and Multiple Populations Effects
  #---------------------------
  model.multi.baseline <- reactive({
    mypops = input$pop_predict_baseline
    model.multi <- fit_global_pop(dataset, mypops)
  })

  model.multi.model <- reactive({
    mypops = input$pop_predict_model
    model.multi <- fit_global_pop(dataset, mypops)
  })

  output$effectsBaseline <- renderPlot({
    mypops = input$pop_predict_baseline
    model <- model.multi.baseline()
    multiLabel = paste(getLabel(mypops), collapse=" + ")
    myColors = getColor(mypops)[1]
    plot_pop_effects(mypops, multiLabel, myColors, model)
  })

  output$effectsModel <- renderPlot({
    mypops = input$pop_predict_model
    model <- model.multi.model()
    multiLabel = paste(getLabel(mypops), collapse=" + ")
    myColors = getColor(mypops)[1]
    plot_pop_effects(mypops, multiLabel, myColors, model)
  })

  output$labelBaseline <- renderUI({
    model <- model.multi.baseline()
    hardError = get_hard_error(model, dataset)
    softError = get_soft_error(model, dataset)
    hard <-  paste("Classification error (hard) = ", format(hardError, digits=3))
    soft <- paste("Classification error (soft) = ", format(softError[2], digits=3))
    baseline <- paste("Classification error (a priori) = ", format(softError[1], digits=3))
    HTML(paste(hard, soft, baseline, sep = "<br />"))
  })


  output$labelModel <- renderUI({
    model <- model.multi.model()
    hardError = get_hard_error(model, dataset)
    softError = get_soft_error(model, dataset)
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
    accuracy = calculate_cross_validation(input$cvInputBaseline, mypops, dataset)*100
    paste("Misclassification Error = ", format(accuracy, digits=3), " %", sep="")
  })

  output$cvResultBaseline <- renderText({
    calculateCVbaseline()
  })

  calculateCVmodel <- eventReactive(input$cvButtonModel, {
    mypops = input$pop_predict_model
    accuracy = calculate_cross_validation(input$cvInputModel, mypops, dataset)*100
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
    pvalue = calculate_permutation_testing(baseline, model, numberOfPermutations, dataset)
    paste("P-Value = ", format(pvalue, digits=3), sep="")
  })

  output$ptResult <- renderText({
    calculatePT()
  })



  # 5. Generate Reports
  #---------------------------


  output$reportExplore <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "reportExplore.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(n = input$pop_explore)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

})


