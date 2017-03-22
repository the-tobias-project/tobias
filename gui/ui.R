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
                plotOutput("histogramPAF", height = 250)
              ),

              box(
                title = "Histogram of Global Allele Frequency", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("histogramGAF", height = 250)
              ),

              box(
                title = "Inputs", status = "warning", solidHeader = TRUE,
                br(),
                selectInput("pop", "Population", list(
                  "All populations" = "all",
                  "African" = "afr",
                  "American" = "amr",
                  "Asia" = list("East Asian"="eas", "South Asian"="sas"),
                  "Europe" = list("Finn"="fin", "Non-Finn"="nfe")
                ), selected="nfe")
              )
            )
    ),

    # Second tab content
    tabItem(tabName = "scatter",
            h2("Scatter Plot"),
            br(),
            fluidRow(
              box(
                title = "Histogram", status = "primary", solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("plot1", height = 250)
              ),

              box(
                title = "Inputs", status = "warning", solidHeader = TRUE,
                "Box content here", br(), "More box content",
                sliderInput("slider", "Slider input:", 1, 100, 50),
                textInput("text", "Text input:")
              )
            )
    ),

    # Third tab content
    tabItem(tabName = "enrichment",
            h2("Enrichment"),
            br()
    )
  )
)

# Calling the UI
#---------------------------
dashboardPage(
  skin="blue",
  header, sidebar, body
)

