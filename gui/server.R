#---------------------------------------------------------------
# Project: Ancestry Bias in ClinVar (w.r.t ExAC population data)
# Author: Arturo Lopez Pineda <arturolp@stanford.edu>
# With code from: Snehit Prabhu <snehit@stanford.edu>
# Date: March 22, 2017
#---------------------------------------------------------------

library(shinydashboard)

#Load once when app is launched
#variants = dget("data/variantDistList.txt")
#source("scripts/barplotBias.R")

# Define server logic
shinyServer(function(input, output) {

  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })

})
