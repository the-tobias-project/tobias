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


# Header of the application
#---------------------------
header <- dashboardHeader(
  title = "TOBIAS project"
)


# Sidebar of the application
#---------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
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
                selectInput("pop_hist", "Population", list(
                  "African" = "afr",
                  "American" = "amr",
                  "Asia" = list("East Asian"="eas", "South Asian"="sas"),
                  "Europe" = list("Finn"="fin", "Non-Finn"="nfe"),
                  "Other" = "oth"
                ), selected="amr")
              )
            )
    ),

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
                selectInput("pop_scatter", "Population", list(
                  "African" = "afr",
                  "American" = "amr",
                  "Asia" = list("East Asian"="eas", "South Asian"="sas"),
                  "Europe" = list("Finn"="fin", "Non-Finn"="nfe"),
                  "Other" = "oth"
                ), selected="amr")
              )
            )
    ),

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
                selectInput("pop_enrichement", "Population", list(
                  "African" = "afr",
                  "American" = "amr",
                  "Asia" = list("East Asian"="eas", "South Asian"="sas"),
                  "Europe" = list("Finn"="fin", "Non-Finn"="nfe"),
                  "Other" = "oth"
                ), selected="amr")
              )
            )
    )
  )
)

# Calling the UI
#---------------------------
dashboardPage(
  skin="blue",
  header, sidebar, body
)

