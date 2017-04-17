#---------------------------------------------------------------
# Project: Ancestry Bias in ClinVar (w.r.t ExAC population data)
# Author: Arturo Lopez Pineda <arturolp@stanford.edu>
# With code from: Snehit Prabhu <snehit@stanford.edu>
# Date: March 22, 2017
#---------------------------------------------------------------

library(shinydashboard)

#---------------------------
#Load once when app is launched
#---------------------------

# Load Scripts
source("../R/loadData.R")
source("../R/featurizeAFs.R")
source("../R/exploratoryTests.R")

# Load Data
clinvar <- readData("../inputs/clinvar.exac.variants.gene.submission.diseases.alleles.tab")

normalize <- function(id_label){
  pops <- list(
    "afr" = "African",
    "amr" = "American",
    "eas" = "East Asian",
    "sas" = "South Asian",
    "fin" = "Finn",
    "nfe" = "Non-Finn",
    "oth" = "Other"
  )
  return(pops[id_label])
}


#---------------------------
# Define server logic
#---------------------------

shinyServer(function(input, output) {

  # Histogram Population Allele Frequency
  #---------------------------
  output$histogramPAF <- renderPlot({
    populations <- c(input$pop_hist)
    dataset <- setAFs(clinvar, c("adj", populations))
    histAFs(dataset, populations, "goldenrod1")
    #goldenrod1, cornsilk3
  })

  # Histogram Global Allele Frequency
  #---------------------------
  output$histogramGAF <- renderPlot({
    populations <- c(input$pop_hist)
    dataset <- setResidualAFs(clinvar, c("adj", populations))
    histResidualAFs(dataset, populations, "goldenrod1")
    #goldenrod1, cornsilk3
  })

  # Scatter Plot
  #---------------------------
  output$scatterAF <- renderPlot({

    populations <- c(input$pop_scatter)
    clinvar$ACMG <- relevel(clinvar$CLNSIG, ref = "VUS")
    dataset <- setAFs(clinvar, c("adj", populations))

    af_label <- paste("af_", input$pop_scatter, sep="")
    colorcode<-c("blue","lightgreen","red")
    title_label <- paste("All vs. ", normalize(input$pop_scatter), sep = "")
    y_label <- paste("AF(", normalize(input$pop_scatter) ,")", sep="")

    scatterPlot(dataset, "af_adj", af_label, "ACMG", colorcode, title_label, "AF(global)", y_label)

  })


  # Enrichment Plot
  #---------------------------
   output$enrichment <- renderPlot({

    populations <- c(input$pop_enrichement)
    clinvar$ACMG <- relevel(clinvar$CLNSIG, ref = "VUS")
    dataset <- setAFs(clinvar, c("adj", populations))

    af_label <- paste("af_", input$pop_enrichement, sep="")
    colorcode<-c("blue","lightgreen","red")
    title_label <- paste("All vs. ", normalize(input$pop_enrichement), sep = "")
    y_label <- paste("AF(", normalize(input$pop_enrichement) ,")", sep="")

    testEnrichment(dataset, "af_adj", af_label, colorcode, title_label, "AF(global)", y_label)

   })

})
