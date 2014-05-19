

library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  "Premiepensionen",
  
  ## First plot ----
  tabPanel(
    "Plot1",
    fluidRow(
      column(1),
      column(
        3,
        selectizeInput(
          "variable",
          "Variabel (y-axel)",
          choices = c(
            "Internr\u00E4nta" = "IRR",
            "Kontov\u00E4rde" = "KONTOVARDE"
          ),
          selected = "IRR"
        ),
        sliderInput(
          "bins",
          "Antal staplar (x-axel)",
          
          # Formatting
          min = 10, max = 500, step = 10, format = "##", locale = "se", ticks = FALSE,
          
          # Initial value
          value = 20
        )
      ),
      column(
        6,
        plotOutput("distPlot")
      ),
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
