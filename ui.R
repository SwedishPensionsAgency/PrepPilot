

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    "Premiepensionen",
    
    tags$head(tags$style("#controls {
              /* Appearance */
                background-color: white;
                padding: 0 20px 20px 20px;
                cursor: move;
              /* Fade out while not hovering */
                opacity: 0.55;
                zoom: 0.9;
                transition: opacity 300ms 0.8s;
              }
              #controls:hover {
              /* Fade in while hovering */
                opacity: 0.95;
              transition-delay: 0;
            }")),
    
    tabPanel("Start"),
    
    ## Kontoutveckling ----
    navbarMenu(
      "Kontoutveckling",
      
      ## > Fördelning, individuella konton ----
      tabPanel(
        "Fördelning, individuella konton",
        fluidRow(
          column(3),
          column(
            6,
            plotOutput("distPlot")
          ),
          column(3)
        ),
        fluidRow(
          column(3),
          column(2,
                 select2Input(
                   "variable",
                   "Variabel (y-axel)",
                   choices = c(
                     "Internränta" = "IRR",
                     "Kontovärde" = "KONTOVARDE",
                     "Garanterat belopp" = "GARANTBLP",
                     "Månatligt belopp" = "MONAMT"
                   ),
                   selected = "IRR",
                   selectize = TRUE
                 )
          ),
          column(2,
                 sliderInput(
                   "bins",
                   "Upplösning (x-axel)",
                   
                   # Formatting
                   min = 1, max = 11, step = 1, format = "##", locale = "se", ticks = FALSE,
                   animate = TRUE,
                   
                   # Initial value
                   value = 2
                 )
          ),
          column(2,
                 sliderInput(
                   "year",
                   "År",
                   
                   # Formatting
                   # Formatting
                   min = 2000, max = 2013, step = 1, format = "####", locale = "se", ticks = FALSE,
                   animate = TRUE,
                   
                   # Initial value
                   value = 2010
                 )
          ),
          column(3)
        )
      ),
      
      
      ## > Interaktivt, individuella konton ----
      tabPanel(
        "Datautforskare, individuella konton",
        
        # Flytande kontrollpanel
          absolutePanel(
            id = "controls", class = "modal", 
            fixed = TRUE, 
            # draggable = TRUE,
            top = 60, left = "auto", right = 20, bottom = "auto",
            width = 330, height = "auto",
            
            h2("Skärningar av data", style="font-family: 'Helvetica Neue', Helvetica; font-weight: 300"),
            
            fluidRow(
              column(
                6,
                checkboxGroupInput("chbSEX", "Kön", c("Kvinnor" = 0, "Män" = 1), selected = c(0, 1))
              ),
              column(
                6,
                checkboxGroupInput("chbINTE_DOD_AGARE", "Levandestatus (ägare)", c("Levande" = 1, "Döda" = 0), selected = c(1))
              )
            ),
            fluidRow(
              column(
                6,
                selectInput(
                  "selINTJANANDEAR", "Första intjänandeår",
                  choices = c(1995:2010),
                  multiple = TRUE, selectize = TRUE
                )
              ),
              column(
                6,
                selectInput(
                  "selFODAR", "Födelseår",
                  choices = 1914:2012,
                  selectize = TRUE
                )
              )
            ),
            hr(),
            fluidRow(
              column(
                6,
                selectInput(
                  "selXvar", "Primärvariabel",
                  choices = c(
                    "Internränta" = "IRR",
                    "Kontovärde" = "KONTOVARDE",
                    "Garanterat belopp" = "GARANTBLP",
                    "Månatligt belopp" = "MONAMT"
                  ),
                  selected = "IRR",
                  selectize = TRUE
                )
              ),
              column(
                6,
                selectInput(
                  "selYvar", "Sekundärvariabel",
                  choices = c(
                    "Internränta" = "IRR",
                    "Kontovärde" = "KONTOVARDE",
                    "Garanterat belopp" = "GARANTBLP",
                    "Månatligt belopp" = "MONAMT"
                  ),
                  selected = "MONAMT",
                  selectize = TRUE
                )
              )
            )
          ),
        
        column(3),
        column(
          6,
          plotOutput("indPlot")
        ),
        column(3)
      ),
      
      
      ## > Tabeller ----
      tabPanel("Tabeller")
    ),
    
    ## Fondrörelsen ----
    navbarMenu(
      "Fondrörelsen",
      
      ## > Värdeutveckling per kalenderår ----
      tabPanel(
        "Värdeutveckling per kalenderår",
        fluidRow(
          column(3),
          column(6, showOutput("fndTimeSeries","nvd3")),
          column(3)
        )
        
      ),
      
      ## > Tidsserier ----
      tabPanel("Tidsserier"),
      
      ## > Tabeller ----
      tabPanel("Tabeller")
    ),
    
    ## Menu ----
    navbarMenu(
      "Om applikationen",
      tabPanel("Data"),
      tabPanel("Licens"),
      tabPanel("Källkod")
    )
  ))
