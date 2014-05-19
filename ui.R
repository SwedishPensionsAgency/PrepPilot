

library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  "Premiepensionen",
  
  ## First plot ----
  tabPanel(
    "Plot1",
    fluidRow(
      column(3),
      column(6, plotOutput("distPlot")),
      column(3)
    )
  ),
  
  ## Second plot ----
  tabPanel(
    "Plot2"
  ),
  
  ## Menu ----
  navbarMenu(
    "Menu!",
    tabPanel("Stuff1"),
    tabPanel("Stuff2")
  )
))
