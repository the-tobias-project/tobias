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

# Load Data
clinvar <- readData("../inputs/clinvar.exac.variants.gene.submission.diseases.alleles.tab")
clinvar$ACMG <- relevel(clinvar$CLNSIG, ref = "VUS")
populations <- names(pops)
dataset <- setAFs(clinvar, c("adj", populations))
dataset <- setResidualAFs(dataset, c("adj", populations))

pops <- list(
  "afr" = list("African", "purple"),
  "amr" = list("American", "darkgreen"),
  "eas" = list("East Asian", "orange"),
  "sas" = list("South Asian", "gold"),
  "fin" = list("Finn", "pink"),
  "nfe" = list("Non-Finn", "brown"),
  "oth" = list("Other", "tomato")
)

normalize <- function(id_label){
  return(unlist(pops[id_label])[1])
}

getColor <- function(id_label){
  return(unlist(pops[id_label])[2])
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
    title_label <- paste("All vs. ", normalize(input$pop_scatter), sep = "")
    y_label <- paste("AF(", normalize(input$pop_scatter) ,")", sep="")

    scatterPlot(dataset, "af_adj", af_label, "ACMG", colorcode, title_label, "AF(global)", y_label)

  })


  # 3. Enrichment Plot
  #---------------------------
   output$enrichment <- renderPlot({

    af_label <- paste("af_", input$pop_enrichement, sep="")
    colorcode<-c("blue","lightgreen","red")
    title_label <- paste("All vs. ", normalize(input$pop_enrichement), sep = "")
    y_label <- paste("AF(", normalize(input$pop_enrichement) ,")", sep="")

    testEnrichment(dataset, "af_adj", af_label, colorcode, title_label, "AF(global)", y_label)

   })

   # 4. Effects
   #---------------------------
   output$effects <- renderPlot({
     models <- fit_models(dataset, input$pop_effects)
     plotEffects(input$pop_effects, getColor(input$pop_effects), models$test_ancestry_f)
   })

})
