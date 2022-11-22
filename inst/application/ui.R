#================================================================
# Project: Ancestry Bias in ClinVar (w.r.t ExAC population data)
# Author: Arturo Lopez Pineda <arturolp@stanford.edu>
# With code from: Snehit Prabhu <snehit@stanford.edu>
# Date: March 22, 2017
#================================================================

library(shinydashboard)

#---------------------------
# Define UI for application
#---------------------------
pops <-list(
  # "European" = list("Finn"="fin", "Non-Finn"="nfe"),
  # "Asia" = list("East Asian"="eas", "South Asian"="sas"),
  "African" = "afr",
  "Non-Finn European"="nfe",
  "Finnish European"="fin",
  "East Asian"="eas",
  "South Asian"="sas",
  "Native American" = "amr",
  "Other" = "oth"
)



# Header of the application
#---------------------------
header <- dashboardHeader(
  title = "TOBIAS"
)

# Sidebar of the application
#---------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("Home", tabName = "home", icon = icon("home")),
              menuItem("Explore", tabName = "explore", icon = icon("puzzle-piece")),
              menuItem("Test", tabName = "test", icon = icon("bar-chart")),
              menuItem("Predict", tabName = "predict", icon = icon("line-chart"), selected = TRUE)
  )
)
#end of sidebar


# Body of the application
#---------------------------
body <- dashboardBody(
  tabItems(


    # First tab content
    tabItem(tabName = "home",
            h1("TOBIAS (Tests of bias)"),
            p("is a suite of exploratory statistical tests for detecting and untangling the sources of bias that can influence genetic test interpretation. Our initial release (version 1) focuses on the marker of genetic ancestry. Through a variety of hypotheses and models, we ask whether this marker - when ignored - can confound the clinical interpretation of a genetic lesion observed in a patient. Asking questions like these is made possible by data that is painstakingly aggregated and freely published by two resources: ClinVar and ExAC."),
            fluidRow(
              column(width = 6,
                     box(width = NULL,
                         h2("ExAC"),
                         p("The Exome Aggregation Consortium (ExAC) is a coalition of investigators seeking to aggregate and harmonize exome sequencing data from a variety of large-scale sequencing projects, and to make summary data available for the wider scientific community. The data set provided on this website spans 60,706 unrelated individuals sequenced as part of various disease-specific and population genetic studies. We have removed individuals affected by severe pediatric disease, so this data set should serve as a useful reference set of allele frequencies for severe disease studies. All of the raw data from these projects have been reprocessed through the same pipeline, and jointly variant-called to increase consistency across projects."),
                         p("Read more here: http://exac.broadinstitute.org/")
                     )
              ),
              column(width = 6,
                     box(width = NULL,
                         h2("ClinVar"),
                         p("ClinVar is a freely accessible, public archive of reports of the relationships among human variations and phenotypes, with supporting evidence. ClinVar thus facilitates access to and communication about the relationships asserted between human variation and observed health status, and the history of that interpretation. ClinVar processes submissions reporting variants found in patient samples, assertions made regarding their clinical significance, information about the submitter, and other supporting data."),
                         p("Read more here: https://www.ncbi.nlm.nih.gov/clinvar/")
                    )
              )
            )
    ),
    #end of tab

    # Second tab content
    tabItem(tabName = "explore",
            h1("Explore Potential Biases in Database"),
            p("This explorer lets you to visualize population allele frequencies of variants recorded in your clinical database."), 
            p("1. Please choose a population of your patient/case."),
            p("2. For variants in this database, confirm that AFs in patient population conform to global averages."),
            br(), 
            selectInput("pop_explore", "Choose population", choices = pops, selected="amr"),
            downloadButton("reportExplore", "Generate report"),
            br(),
            h2("Allele Frequency Histograms"),
            fluidRow(
              column(width = 6,
                     box(width = NULL,
                         title = "Population AF", status = "primary",
                         plotOutput("histogramPAF", height = 300)
                     )
              ),
              column(width = 6,
                     box(width = NULL,
                         title = "Population AF - Globally Averaged AF", status = "primary",
                         plotOutput("histogramGAF", height = 300)
                     )
              )
            ),
            br(),
            h2("Allele Frequency Scatters"),
            fluidRow(
              column(width = 12,
                     box(width = NULL,
                         title = "Population AF vs Global AF (color coded by interpretation)", status = "primary",
                         plotOutput("scatterAF", height = 500)
                     )
              )
            ),
            br()
    ),
    #end of tab
    
    # Third tab content
    tabItem(tabName = "test",
            h1("Tests For Bias In Database"),
            p("Test whether variant assertions recorded in this database are correlated with patient's genetic ancestry"),
            p("1. Choose population of your patient/case."),
            p("2. Choose a cutoff, marking variants whose AFs are \"appreciably different\" between patient population and global average."),
            p("3. Apply non-parametric test to check whether statistically significant bias exists."),
            br(), 
            selectInput("pop_test", "Choose population", choices = pops, selected="amr"),
            downloadButton("reportTest", "Generate report"),
            br(),
            h2("Are variant interpretations independent of their population AFs?"),
            br(),
            fluidRow(
              column(width = 12,
                     box(width = NULL,
                         title = "Population-based differences in allelic interpretation", status = "primary",
                         plotOutput("enrichment", height = 500)
                     )
              )
            )
    ), 
    #end of tab
    
    # Fourth tab content
    tabItem(tabName = "predict",
            h1("Predict Bias In Database"),
            p("As a final confirmatory step, check whether you can predict a variant's interpretation based (purely on) population frequency."), 
            p("1. Keep baseline-model fixed. For the alternate model, choose population of your patient/case. Choose multiple populations in case of admixed-patients"),
            p("2. Check whether the regression model can predict variant interpretations, using only AF to predict outcome. Better predictive ability suggests bias in database"),
            p("3. Confirm statistical significance of trained model by running permutation tests to calculate a p-value"),
            br(), 
            downloadButton("reportPredict", "Generate report"),
            br(),
            h2("Effects"),
            br(),
            fluidRow(
              column(width = 6,
                     box(width = NULL,
                         title = "Baseline Model", status = "primary", solidHeader = TRUE,
                         selectizeInput("pop_predict_baseline", "Choose Baseline model populations", choices = pops, multiple=TRUE),
                         plotOutput("effectsBaseline", height = 400),
                         htmlOutput("labelBaseline"),
                         hr(),
                         h5("Cross Validation Leave-%-out:"),
                         sliderInput("cvInputBaseline", NULL, value=10, min = 10, max = 100, step=10, tick=FALSE),
                         actionButton("cvButtonBaseline", "Calculate"),
                         br(), br(),
                         textOutput("cvResultBaseline")
                     )
              ),
              column(width = 6,
                     box(width = NULL,
                         title = "Full Model", status = "primary", solidHeader = TRUE,
                         selectizeInput("pop_predict_model", "Choose Full model populations", choices = pops, multiple=TRUE),
                         plotOutput("effectsModel", height = 400),
                         htmlOutput("labelModel"),
                         hr(),
                         h5("Cross Validation Leave-%-out:"),
                         sliderInput("cvInputModel", NULL, value=10, min = 10, max = 100, step=10, tick=FALSE),
                         actionButton("cvButtonModel", "Calculate"),
                         br(), br(),
                         textOutput("cvResultModel")
                     )
              )
            ),
            fluidRow(
              column(width=12,
                     box(width = NULL,
                         h5("Permutation Testing (number):"),
                         sliderInput("sliderPT", NULL, value=10, min = 10, max = 1000, step=10, tick=TRUE),
                         actionButton("buttonPT", "Calculate"),
                         br(), br(),
                         textOutput("ptResult")
                     )
              )
            ),
            br(), br()
    )
    #end of tab
  )
)
#end of body

# Calling the UI
#---------------------------
dashboardPage(
  skin="blue",
  header, sidebar, body
)

