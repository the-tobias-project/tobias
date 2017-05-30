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
populations <-list(
  "African" = "afr",
  "American" = "amr",
  "Asia" = list("East Asian"="eas", "South Asian"="sas"),
  "Europe" = list("Finn"="fin", "Non-Finn"="nfe"),
  "Other" = "oth"
)

pops <- list(
  "African" = "afr",
  "American" = "amr",
  "East Asian" = "eas",
  "South Asian" = "sas",
  "Finn" ="fin",
  "Non-Finn European" = "nfe",
  "Other" = "oth"
)

# Header of the application
#---------------------------
header <- dashboardHeader(
  title = "TOBIAS project"
)


# Sidebar of the application
#---------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Effects", tabName = "effects", icon = icon("snowflake-o")),
    menuItem("Histogram", tabName = "histogram", icon = icon("bar-chart")),
    menuItem("Scatter", tabName = "scatter", icon = icon("area-chart")),
    menuItem("Enrichment", tabName = "enrichment", icon = icon("star"))

  )
)


# Body of the application
#---------------------------
body <- dashboardBody(
  tabItems(

    # First tab content
    tabItem(tabName = "histogram",
            h2("Histogram"),
            br(),
            fluidRow(
              box(
                title = "Histogram of Population Allele Frequency", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("histogramPAF", height = 300)
              ),

              box(
                title = "Histogram of Global Allele Frequency", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("histogramGAF", height = 300)
              ),

              box(
                title = "Inputs", status = "warning", solidHeader = TRUE,
                br(),
                selectInput("pop_hist", "Population", populations, selected="amr")
              )
            )
    ),
    #end of tab

    # Second tab content
    tabItem(tabName = "scatter",
            h2("Scatter Plot"),
            br(),
            fluidRow(
              box(
                title = "Scatter plot of Population Allele Frequency", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("scatterAF", height = 400)
              ),

              box(
                title = "Inputs", status = "warning", solidHeader = TRUE,
                br(),
                selectInput("pop_scatter", "Population", populations, selected="amr")
              )
            )
    ),
    #end of tab

    # Third tab content
    tabItem(tabName = "enrichment",
            h2("Enrichment"),
            br(),
            fluidRow(
              box(width = "60%",
                  title = "Test of Enrichement", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("enrichment", height = 400)
              ),

              box(
                title = "Inputs", status = "warning", solidHeader = TRUE,
                br(),
                selectInput("pop_enrichement", "Population", populations, selected="amr")
              )
            )
    ),
    #end of tab

    # Fourth tab content
    tabItem(tabName = "effects",
            h2("Effects"),
            br(),
            fluidRow(
              box(
                title = "Inputs", status = "warning", solidHeader = TRUE,
                br(),
                selectInput("pop_effects", "Population", populations, selected="amr"),
                br(),
                uiOutput("choose_pop_effect")
                #br(),
                #uiOutput("choose_other_pops")
              ),

              box(
                  title = "Test of Global Effects", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("globalEffects", height = 400),
                  textOutput("globalLabel"),
                  hr(),
                  h5("Cross Validation Leave-%-out:"),
                  sliderInput("cvInputGlobal", NULL, value=10, min = 0, max = 100, step=10, tick=FALSE),
                  actionButton("cvButtonGlobal", "Calculate"),
                  br(), br(),
                  textOutput("globalLabelCV"),
                  hr(),
                  h5("Permutation Testing (number):"),
                  sliderInput("ptInputGlogal", NULL, value=10000, min = 10000, max = 100000, step=10000, tick=FALSE),
                  actionButton("ptButtonGlobal", "Calculate"),
                  br(), br(),
                  textOutput("globalLabelPT")
              ),

              box(
                title = "Test of Global + Population Effects", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("globalPopEffects", height = 400),
                textOutput("globalPopLabel"),
                hr(),
                h5("Cross Validation Leave-%-out:"),
                sliderInput("cvInputGlobalPop", NULL, value=10, min = 0, max = 100, step=10, tick=FALSE),
                actionButton("cvButtonGlobalPop", "Calculate"),
                br(), br(),
                textOutput("globalPopLabelCV"),
                hr(),
                h5("Permutation Testing (number):"),
                sliderInput("ptInputGlogalPop", NULL, value=10000, min = 10000, max = 100000, step=10000, tick=FALSE),
                actionButton("ptButtonGlobalPop", "Calculate"),
                br(), br(),
                textOutput("globalPopLabelPT")
              ),

               box(
                 title = "Test of Global + Multiple Populations Effects", status = "primary", solidHeader = TRUE,
                 collapsible = TRUE,
                 plotOutput("globalMultiPopEffects", height = 400),
                 textOutput("globalMultiLabel"),
                 hr(),
                 h5("Cross Validation Leave-%-out:"),
                 sliderInput("cvInputGlobalMultiPop", NULL, value=10, min = 0, max = 100, step=10, tick=FALSE),
                 actionButton("cvButtonGlobalMultiPop", "Calculate"),
                 br(), br(),
                 textOutput("globalMultiPopLabelCV"),
                 hr(),
                 h5("Permutation Testing (number):"),
                 sliderInput("ptInputGlogalMultiPop", NULL, value=10000, min = 10000, max = 100000, step=10000, tick=FALSE),
                 actionButton("ptButtonGlobalMultiPop", "Calculate"),
                 br(), br(),
                 textOutput("globalMultiPopLabelPT")
               )

            )
    )
    #end of tab


  )
)

# Calling the UI
#---------------------------
dashboardPage(
  skin="blue",
  header, sidebar, body
)

