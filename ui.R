

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
        
        # Text
        fluidRow(
          column(3),
          column(
            6,
            h2("Fördelning av några variabler", style = "text-align: center;"),
            p("Grafen nedan visar fördelningen av vald variabel i ett histogram 
              längs x-axeln. ", em("Stapelbredden" ), "redovisas i diagramtexten. ",
              em("Stapelhöjden "), "återspeglar antal personer i varje kategori."),
            p("Genom att förändra reglagevärdena kan olika variabler utforskas
              för olika tidpunkt och olika upplösning."),
            hr()
          ),
          column(3)
        ),
        
        # Graph
        fluidRow(
          column(3),
          column(
            6,
            plotOutput("distPlot")
          ),
          column(3)
        ),
        
        # Controls
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
        
        # Text
        fluidRow(
          column(3),
          column(
            6,
            h2("Utforskare för individdata", style = "text-align: center;"),
            p("Graferna nedan visar olika aspekter av individdata för premiepensionen.
              För att välja olika skärningar på data kan menyn till höger användas.
              För musen över den för att visa menyn."),
            p("Graf 1 är en ", em("heatmap "), "som visar fördelningen av den valda 
              \"primärvariabeln\" för födelseår och intjänandeår."),
            p(strong("OBS! "), "Denna flik är inte byggd för prestanda. Det kan
              därför ta upp till 10-15 sekunder att generera graferna nedan även
              på en snabb dator."),
            hr()
          ),
          column(3)
        ),
        
        # Floating control panel
        tags$head(tags$style("#controls {
              /* Appearance */
                background-color: white;
                padding: 0 20px 20px 20px;
                cursor: move;
              /* Fade out while not hovering */
                opacity: 0.15;
                zoom: 0.9;
                transition: opacity 800ms 0.1s;
              }
              #controls:hover {
              /* Fade in while hovering */
                opacity: 0.95;
              transition-delay: 0;
            }")),
        
        absolutePanel(
          id = "controls", class = "modal", 
          fixed = FALSE, 
          draggable = FALSE,
          top = 60, left = "auto", right = 20, bottom = "auto",
          width = 330, height = "auto",
          
          h2("Skärningar av data"),
          
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
                selected = c(2003:2008),
                multiple = TRUE, selectize = TRUE
              )
            ),
            column(
              6,
              selectInput(
                "selFODAR", "Födelseår",
                choices = birthYears, selected = sample(birthYears, 5),
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
        h3("Utfall i premiepensionssystemet per födelseår och intjänandeår", style = "text-align: center;"),
        
        fluidRow(
          column(3),
          column(6, plotOutput("indHeatmap")),
          column(3)
        ),
        
        hr(),
        h3(""),
        fluidRow(
          column(3),
          column(6, plotOutput("indDensity")),
          column(3)
        )
      ),
      
      ## > Tabeller ----
      tabPanel("Tabeller")
    ),
    
    ## Fondrörelsen ----
    navbarMenu(
      "Fondrörelsen",
      
      ## > Tidsserier ----
      tabPanel(
        "Tidsserier",
        fluidRow(
          column(3),
          column(6, showOutput("fndTimeSeries","nvd3")),
          column(3)
        )
      ),
      
      ## > Värdeutveckling per kalenderår ----
      tabPanel(
        "Värdeutveckling per kalenderår",
        fluidRow(
          column(3),
          column(6, showOutput("fndYearlyGrowth","nvd3")),
          column(3)
        )
      ),
      
      ## > Tabeller ----
      tabPanel(
        "Tabeller"
      )
    ),
    
    ## Menu ----
    navbarMenu(
      "Om applikationen",
      tabPanel("Data"),
      tabPanel("Licens"),
      tabPanel("Källkod")
    ),
    
    singleton(tags$head(
      tags$style(
        "h2, h3 { font-family: 'Helvetica Neue', Helvetica; font-weight: 300; }"
      )))
  )
)
