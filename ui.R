

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
          h2("Premiepensionsportalen", style = "text-align: center;"),
          p("Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
        )
      )
    ),

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
            p("Grafen nedan är en ", em("heatmap "), "som visar fördelningen av den valda 
              \"primärvariabeln\" för födelseår och intjänandeår."),
            p("För att välja olika skärningar på data kan menyn till höger användas.
              För musen till höger på bildskärmen för att visa menyn."),
            p("GörEfter att valda ändringar gjorts, tryck på \"uppdatera graf\"-
              knappen."),
            p(strong("OBS! "), "Denna graf är inte byggd för prestanda. Det kan
              därför ta upp till 10-20 sekunder att generera graferna nedan även
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
                selected = c(2003:2006),
                multiple = TRUE, selectize = TRUE
              )
            ),
            column(
              6,
              selectInput(
                "selFODAR", "Födelseår",
                choices = birthYears, selected = sample(birthYears, 4),
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
                "selYear",
                "Data för år",
                choices = c(2000:2013), selected = 2010,
                multiple = FALSE, # TODO: Implementera stöd för flera år åt gången
                options = list(width = "140px"), selectize = FALSE
              )
            )
          ),
          hr(),
          fluidRow(
            column(6, offset = 3,
                   actionButton("indUpdate","Uppdatera data")
            )
          )
        ),
        
        ## Plotfönster
        fluidRow(
          column(3),
          # column(6,showOutput("indPlot","polycharts")),
          column(6, plotOutput("indHeatmap")),
          column(3)
        ),
        fluidRow(
          column(3),
          column(6, plotOutput("indDensity")),
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
      
      ## > Tidsserier ----
      tabPanel(
        "Tidsserier",
        
        # Text
        fluidRow(
          column(
            6, offset = 3,
            h2("Tidsserier", style="text-align: center;"),
            tags$small(p("Grafen visar den historiska utvecklingen för valt index
              på dagsbasis mellan 2000-12-13 och 2014-03-20. Index=100 är bestämt
              tid tidpunkten ", em("t "), "= 2000-12-13."),
            p("För musen över grafen för att se värdet för en enskild punkt i grafen.")),
            hr()
          )
        ),
        
        # Controls
        fluidRow(
          column(
            2, offset = 3,
            select2Input(
              "tsVar",
              "Visa tidsserie för",
              choices = c(
                "Internränta" = "IRR",
                "Internränta (real)" = "IRRReal",
                "Premiepensionsindex" = "PPINDEX",
                "AP7-index" = "AP7INDEX",
                "Marknadsvärde, fondrörelsen" = "MVFondrorelse",
                "Anskaffningsvärde" = "ANSKAFFNINGSVARDE"
              ),
              selected = "IRR",
              selectize = FALSE
            )
          ),
          column(
            2,
            select2Input(
              "tsRes",
              "Upplösning för tidsserie",
              choices = c(
                "Dag" = 1,
                "Vecka" = 7,
                "Månad" = 30
              ),
              selected = 30,
              selectize = FALSE
            )
          ),
          column(
            2,
            dateInput(
              "tsStartdate", "Välj tidsperiod", value = "2000-12-13",
              min = "2000-12-13", max = "2014-04-30", weekstart = 1,
              startview = "year",
              language = "sv"
            ),
            dateInput(
              "tsEnddate", "", value = "2014-04-30",
              min = "2000-12-13", max = "2014-04-30", weekstart = 1,
              startview = "year",
              language = "sv"
            )
          )
        ),
        
        # hr()
        fluidRow(column(6, offset = 3, hr())),
        
        # Graph
        fluidRow(
          column(3),
          column(6, showOutput("fndTimeSeries","nvd3")),
          column(3)
        ),
        
        # Controls
        fluidRow(
          column(3),
          column(
            6,
            hr(),
            sidebarPanel(
              downloadButton("fndDownloadData", "Ladda ned data"),
              radioButtons("fndFileFormat", "Filformat", c("CSV", "Excel"), selected = "CSV")
            )
          ),
          column(3)
        )
      ),
      
      ## > Index ----
      tabPanel(
        "Indexserier",
        
        # Text
        fluidRow(
          column(
            6, offset = 3,
            h2("Indexserier", style="text-align: center;"),
            tags$small(
              p("Här visas värdeutvecklingen för fondrörelsen, dvs alla fonder inklusive Premiesparfonden och AP7 Såfa sedan systemets start 2000-12-13."),
              p("Tidsviktad avkastning, TVA mäter avkastningen på en genomsnittlig krona som sattes in i premiepensionssystemet 2000-12-13. Premiepensionsindex kan jämföras med marknadsindex och enskilda fonders avkastning. Rabatten på fondavgiften, myndighetens avgift och arvsvinster är inte medräknade."),
              p("Real avkastning = avkastning/inflation (KPI)", br(),
                "Med årsavkastning menas genomsnittlig avkastning sedan start.")
            ),
            hr()
          )
        ),
        
        # Controls
        fluidRow(
          column(
            2, offset = 3,
            select2Input(
              "ixVar",
              "Visa tidsserie för",
              choices = c(
                # "Internränta" = "IRR",
                # "Internränta (real)" = "IRRReal",
                "Premiepensionsindex" = "PPINDEX",
                "AP7-index" = "AP7INDEX"
                # "Marknadsvärde, fondrörelsen" = "MVFondrorelse",
                # "Anskaffningsvärde" = "ANSKAFFNINGSVARDE"
              ),
              selected = "IRR",
              selectize = FALSE
            ),
            checkboxInput(
              "ixYearly",
              "Beräkna årsavkastning",
              value = FALSE
            )
          ),
          column(
            2,
            select2Input(
              "ixRes",
              "Upplösning för tidsserie",
              choices = c(
                "Dag" = 1,
                "Vecka" = 7,
                "Månad" = 30
              ),
              selected = 30,
              selectize = FALSE
            )
          ),
          column(
            2,
            dateInput(
              "ixStartdate", "Välj tidsperiod", value = "2000-12-13",
              min = "2000-12-13", max = "2014-04-30", weekstart = 1,
              startview = "year",
              language = "sv"
            ),
            dateInput(
              "ixEnddate", "", value = "2014-04-30",
              min = "2000-12-13", max = "2014-04-30", weekstart = 1,
              startview = "year",
              language = "sv"
            )
          )
        ),
        
        # hr()
        fluidRow(column(6, offset = 3, hr())),
        
        # Graph
        fluidRow(
          column(3),
          column(6, showOutput("fndIndexSeries","nvd3")),
          column(3)
        )
      ),
      
      ## > Värdeutveckling per kalenderår ----
      tabPanel(
        "Värdeutveckling per kalenderår",
        
        # Text
        fluidRow(
          column(3),
          column(
            6,
            h2("Värdeutveckling i Premiepensionsindex på årsbasis", style="text-align: center;"),
            p("Staplarna nedan visar utvecklingen i Premiepensionsindex per kalenderår."),
            p("För musen över en stapel för att se värdet för ett givet år."),
            hr()
          ),
          column(3)
        ),
        
        # Controls
        fluidRow(
          column(
            2, offset = 3,
            select2Input(
              "yrVar",
              "Visa tidsserie för",
              choices = c(
                "Internränta" = "IRR",
                "Internränta (real)" = "IRRReal",
                "Premiepensionsindex" = "PPINDEX",
                "AP7-index" = "AP7INDEX",
                "Marknadsvärde, fondrörelsen" = "MVFondrorelse",
                "Anskaffningsvärde" = "ANSKAFFNINGSVARDE"
              ),
              selected = "IRR",
              selectize = FALSE
            )
          ),
          column(
            2,
            checkboxInput(
              "yrDiff",
              "Visa årsdifferens",
              value = TRUE
            )
          )
        ),
        
        # hr()
        fluidRow(column(6, offset = 3, hr())),
        
        # Graph
        fluidRow(
          column(3),
          column(6, showOutput("fndYearlyGrowth","nvd3")),
          column(3)
        )
      ),
      
      ## > Tabeller ----
      tabPanel(
        "Tabeller",
        
        # Text
        fluidRow(
          column(3),
          column(
            6,
            h2("Fondtabeller", style="text-align: center;"),
            p("tabellen visar fondstatistik för samtliga fonder. Du kan själv välja vilken information som ska visas i tabellen."),
            p("Genom sökfunktionen kan du söka på namn på individuella fonder eller "),
            hr()
          ),
          column(3)
        ),
        
        # Controls
        fluidRow(
          column(
            6, offset = 3,
            select2Input(
              "tblVars",
              "Välj kolumner att visa i tabellen",
              choices = names(dataFond),
              selected = names(dataFond)[1:5],
              options = list(maximumSelectionSize = 6),
              multiple = TRUE, selectize = FALSE
            ),
            hr()
          )
        ),
        
        # Tables
        fluidRow(
          column(
            6, offset = 3,
            dataTableOutput("ppTables")
          )
        )
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
        "h2 { font-family: 'Helvetica Neue', Helvetica; font-weight: 300; }"
      )))
  )
)
