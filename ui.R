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
  title = "Stanford | ToBias genomic project"
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
            h1("Test of Bias (ToBias)"),
            br(),
            p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
            fluidRow(
              column(width = 6,
                     box(width = NULL,
                         h2("ExAC"),
                         p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
                         p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                     )
              ),
              column(width = 6,
                     box(width = NULL,
                         h2("ClinVar"),
                         p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
                         p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
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
            h1("Predict"),
            p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."),
            downloadButton("reportPredict", "Generate report"),
            br(),
            h2("Effects"),
            br(),
            fluidRow(
              column(width = 6,
                     box(width = NULL,
                         title = "Test of Global + Populations Effects", status = "primary", solidHeader = TRUE,
                         selectizeInput("pop_predict_baseline", "Choose Baseline model populations", choices = pops, multiple=TRUE),
                         plotOutput("effectsBaseline", height = 400),
                         textOutput("labelBaseline"),
                         h5("Cross Validation Leave-%-out:"),
                         sliderInput("cvInputBaseline", NULL, value=10, min = 10, max = 100, step=10, tick=FALSE),
                         actionButton("cvButtonBaseline", "Calculate"),
                         br(), br(),
                         textOutput("cvResultBaseline")
                     )
              ),
              column(width = 6,
                     box(width = NULL,
                         title = "Test of Global + Populations Effects", status = "primary", solidHeader = TRUE,
                         selectizeInput("pop_predict_model", "Choose Full model populations", choices = pops, multiple=TRUE),
                         plotOutput("effectsModel", height = 400),
                         textOutput("labelModel"),
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
                         sliderInput("ptInputGlogalMultiPop", NULL, value=10, min = 10, max = 1000, step=10, tick=TRUE),
                         actionButton("ptButtonGlobalMultiPop", "Calculate"),
                         br(), br(),
                         textOutput("globalMultiPopLabelPT")
                     )
              )
            )
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

