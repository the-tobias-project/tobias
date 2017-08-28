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
  "African" = "afr",
  "American" = "amr",
  "Asia" = list("East Asian"="eas", "South Asian"="sas"),
  "Europe" = list("Finn"="fin", "Non-Finn"="nfe"),
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
              menuItem("Predict", tabName = "predict", icon = icon("snowflake-o"), selected = TRUE)
  )
)
#end of sidebar


# Body of the application
#---------------------------
body <- dashboardBody(
  tabItems(


    # First tab content
    tabItem(tabName = "home",
            h1("Tobias (Tests of bias)"),
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
            h1("Explore"),
            p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
            selectInput("pop_explore", "Choose 1 population", choices = pops, selected="amr"),
            downloadButton("reportExplore", "Generate report"),
            br(),
            h2("Histograms"),
            fluidRow(
              column(width = 6,
                     box(width = NULL,
                         title = "Histogram of Population Allele Frequency", status = "primary",
                         plotOutput("histogramPAF", height = 300)
                     )
              ),
              column(width = 6,
                     box(width = NULL,
                         title = "Histogram of Global Allele Frequency", status = "primary",
                         plotOutput("histogramGAF", height = 300)
                     )
              )
            ),
            br(),
            h2("Scatter Plot and Enrichment"),
            fluidRow(
              column(width = 6,
                     box(width = NULL,
                         title = "Scatter plot of Population Allele Frequency", status = "primary",
                         plotOutput("scatterAF", height = 500)
                     )
              ),
              column(width = 6,
                     box(width = NULL,
                         title = "Test of Enrichement", status = "primary",
                         plotOutput("enrichment", height = 500)
                     )
              )
            )
    ),
    #end of tab

    # Third tab content
    tabItem(tabName = "predict",
            h1("Predictive Modeling"),
            p("Here, you can test whether your patient's ancestry significantly influences the interpretation assigned to variants in the genes you are testing. Use the columns to compare two models, each accounting (or not accounting) for variant frequency in a set of populations. See how these influence your ability to predict a variant's interpretation. Then run some permutations to check whether these differences are anecdotal, or statistically significant."),
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

