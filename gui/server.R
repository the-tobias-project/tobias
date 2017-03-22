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

# Load Data
clinvar <- readData("../inputs/clinvar.exac.variants.gene.submission.diseases.alleles.tab")


#---------------------------
# Define server logic
#---------------------------

shinyServer(function(input, output) {

  #Example Plot
  #---------------------------
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })

  # Histogram Population Allele Frequency
  #---------------------------
  output$histogramPAF <- renderPlot({
    populations <- c(input$pop)
    dataset <- setAFs(clinvar, c("adj", populations))
    histAFs(dataset, populations, "goldenrod1")
    #goldenrod1, cornsilk3
  })

  # Histogram Global Allele Frequency
  #---------------------------
  output$histogramGAF <- renderPlot({
    populations <- c(input$pop)
    dataset <- setResidualAFs(clinvar, c("adj", populations))
    histResidualAFs(dataset, populations, "goldenrod1")
    #goldenrod1, cornsilk3
  })

})
