

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    "Premiepensionen",
    
    tabPanel("Start", p("Lorem ipsum")),
    
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
                   selectize = FALSE
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
                   value = 2008
                 )
          ),
          column(3)
        )
      ),
      
      
      ## > Interaktivt, individuella konton ----
      tabPanel(
        "Datautforskare, individuella konton",
        
        tags$head(tags$style("#controls {
              /* Appearance */
                background-color: white;
                padding: 0 20px 20px 20px;
                cursor: move;
              /* Fade out while not hovering */
                opacity: 0.55;
                zoom: 0.9;
                transition: opacity 800ms 0.1s;
              }
              #controls:hover {
              /* Fade in while hovering */
                opacity: 0.95;
              transition-delay: 0;
            }")),
        
        ## Flytande kontrollpanel
        absolutePanel(
          id = "controls", class = "modal", 
          fixed = TRUE, 
          draggable = FALSE,
          top = 60, left = "auto", right = 20, bottom = "auto",
          width = 330, height = "auto",
          
          h2("Skärningar av data", style="font-family: 'Helvetica Neue', Helvetica; font-weight: 300"),
          
          h4("Demografi"),
          fluidRow(
            column(
              6,
              checkboxGroupInput("chbSEX", "Kön", c("Kvinnor" = 0, "Män" = 1), selected = c(0, 1))
            ),
            column(
              6,
              checkboxGroupInput("chbINTE_DOD_AGARE", "Levandestatus (ägare)",
                                 c("Levande" = 1, "Döda" = 0, "Okänt" = NA), selected = c(1))
            )
          ),
          fluidRow(
            column(
              6,
              selectInput(
                "selINTJANANDEAR", "Första intjänandeår",
                choices = c(1995:2010),
                selected = c(1997:2008),
                multiple = TRUE, selectize = TRUE
              )
            ),
            column(
              6,
              selectInput(
                "selFODAR", "Födelseår",
                choices = birthYears, selected = sample(birthYears, 15),
                multiple = TRUE, selectize = TRUE
              )
            )
          ),
          hr(),
          h4("Variabler"),
          fluidRow(
            column(
              6,
              select2Input(
                "selXvar", "Primärvariabel",
                choices = c(
                  "Internränta" = "IRR",
                  "Kontovärde" = "KONTOVARDE",
                  "Garanterat belopp" = "GARANTBLP",
                  "Månatligt belopp" = "MONAMT"
                ),
                selected = "IRR",
                options = list(width = "140px"), selectize = FALSE
              )
            ),
            column(
              6,
              select2Input(
                "selYvar", "Sekundärvariabel",
                choices = c(
                  "Internränta" = "IRR",
                  "Kontovärde" = "KONTOVARDE",
                  "Garanterat belopp" = "GARANTBLP",
                  "Månatligt belopp" = "MONAMT"
                ),
                selected = "MONAMT",
                options = list(width = "140px"), selectize = FALSE
              )
            )
          ),
          fluidRow(
            column(
              6,
              select2Input(
                "selYear",
                "Data för år",
                choices = c(2000:2013), selected = 2010,
                multiple = FALSE, # TODO: Implementera stöd för flera år åt gången
                options = list(width = "140px"), selectize = FALSE
              )
            )
          )
        ),
        
        ## Plotfönster
        fluidRow(
          column(3),
          # column(6,showOutput("indPlot","polycharts")),
          column(6, plotOutput("heatmap")),
          column(3)
        ),
        fluidRow(
          column(3),
          column(6, plotOutput("density")),
          # column(9, showOutput("indPlot2","nvd3")),
          column(3)
        )
      ),
      
      
      ## > Tabeller ----
      tabPanel("Tabeller")
    ),
    
    ## Fondrörelsen ----
    navbarMenu(
      "Fondrörelsen",
      
      ## > Värdeutveckling per kalenderår ----
      tabPanel("Värdeutveckling per kalenderår"),
      
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
