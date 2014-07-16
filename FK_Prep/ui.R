

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    "Premiepensionen",
    
    tabPanel(
      "Start", 
      fluidRow(
        column(6, offset = 3, img(src="img/PM_logo.jpg"))
      ),
      fluidRow(
        column(
          6, offset = 3,
          h2("Premiepensionsportalen: FK-rapporterna", style = "text-align: center;"),
          p("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
        )
      )
    ),
    
    ## Antalsuppgifter ----
    navbarMenu(
      "Antalsuppgifter",
      tabPanel(
        "Lorem",
        
        column(1),
        
        ## > Lorem ----
        fluidRow("Test"),
        
        column(1)
      ),
      
      tabPanel(
        "Ipsum",
        
        column(1),
        
        ## > Ipsum ----
        fluidRow("Test"),
        
        column(1)
      ),
      
      tabPanel(
        "Dolor",
        
        column(1),
        
        ## > Dolor ----
        fluidRow("Test"),
        
        column(1)
      ),
      
      tabPanel(
        "Sit",
        
        column(1),
        
        ## > Sit ----
        fluidRow("Test"),
        
        column(1)
      ),
      
      tabPanel(
        "Amet",
        
        column(1),
        
        ## > Lorem ----
        fluidRow("Amet"),
        
        column(1)
      ),
    ),
    
    
    ## Om Applikationen ----
    navbarMenu(
      "Om applikationen",
      tabPanel("Data"),
      tabPanel("Licens"),
      tabPanel("Källkod")
    ),
    
    singleton(tags$head(
      tags$style(
        "h2 { font-family: 'Helvetica Neue', Helvetica; font-weight: 300; }"
      )))
  )
)
