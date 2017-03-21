library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("TOBIAS: Tests of Bias"),

  # Sidebar with parameters
  sidebarLayout(
    position = "left",
    sidebarPanel(
      selectInput("pop1", "Population 1", list(
        "African" = "afr",
        "American" = "amr",
        "Asia" = list("East Asian"="eas", "South Asian"="sas"),
        "Europe" = list("European (all)"="eur", "Finn"="fin", "Non-Finn"="nfe")
      ), selected="eur"),
      selectInput("pop2", "Population 2", list(
        "African" = "afr",
        "American" = "amr",
        "Asia" = list("East Asian"="eas", "South Asian"="sas"),
        "Europe" = list("European (all)"="eur", "Finn"="fin", "Non-Finn"="nfe")
      ), selected="amr"),
      sliderInput("criteria", label = "Sliding Triangle", min = 0.5, max = 1, value = 1),
      actionButton("button", "Calculate"),
      br(),
      sliderInput("threshold", label = "Threshold", min = 0, max = 10, value = 5, step = 1)
    ),

    # Show a plot of the generated bias graphics
    mainPanel(
      plotOutput("barplot")
    )
  )
))
