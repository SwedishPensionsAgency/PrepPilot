

library(shiny)

# Define UI for application that draws a histogram
navbarPage(
  title = "",
  #inverse = TRUE,
  collapsable = TRUE,
  # If we want, we could add code here that would be displayed on all the pages.
  # This could be useful for e.g. providing a logo beneath the menu, a 
  # copyright footer or other nice CI elements.
  #   header = tagList(
  #                   uiOutput("ribbon"), 
  #                   column(width = 1, offset = 1,div(id = "logo", img(src="img/PM_Logo_neg_farg_150mm150dpi.png", width = 80, height = 50))),
  #                   hr()
  #           ),
  footer = tagList(
    column(10, offset = 1, p(tags$small("Denna applikation är en prototyp utvecklad av Pensionsmyndigheten och är för närvarande under utveckling. Det finns ingen garanti för att uppgifterna i applikationen är tillförlitliga då dessa inte genomgått kvalitetssäkring och delvis bygger på fabricerad data.", style = "color: Grey;")))
  ),
  
  tabPanel(
    title = img(src="img/PM_logo.jpg", width = 35, style = "padding: 0px 0px"), 
    fluidRow(
      column(6, offset = 3, img(src="img/PM_logo.jpg"))
    ),
    fluidRow(
      column(
        10, offset = 1,
        h2("Premiepensionsportalen: FK-rapporterna", style = "text-align: center;"),
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
        column(
          10, offset = 1,
          uiOutput("ktoText")
        )
      ),
      
      # Graph
      fluidRow(
        column(
          10, offset = 1,
          plotOutput("ktoPlot")
        )
      ),
      
      # Controls
      fluidRow(
        uiOutput("ktoControls")
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
          10, offset = 1,
          uiOutput("ftsText")
        )
      ),
      
      # Controls
      fluidRow(
        uiOutput("ftsControls")
      ),
      
      # hr()
      fluidRow(column(10, offset = 1, hr())),
      
      # Graph
      fluidRow(
        column(10, offset = 1, showOutput("ftsChart","nvd3"))
      ),
      
      # Controls
      fluidRow(
        column(
          10, offset = 1,
          hr(),
          sidebarPanel(
            downloadButton("ftsDownloadHandler", "Ladda ned data"),
            radioButtons("ftsFileFormat", "Filformat", c("CSV", "Excel"), selected = "CSV")
          )
        )
      )
    ),
    
    ## > Index ----
    tabPanel(
      "Indexserier",
      
      # Text
      fluidRow(
        column(10, offset = 1, uiOutput('fixText'))
      ),
      
      # Controls
      fluidRow(
        uiOutput('fixControls')
      ),
      
      # hr()
      fluidRow(column(10, offset = 1, hr())),
      
      # Graph
      fluidRow(
        column(10, offset = 1, showOutput("fixChart","nvd3"))
      )
    ),
    
    ## > Värdeutveckling per kalenderår ----
    tabPanel(
      "Värdeutveckling per kalenderår",
      
      # Text
      fluidRow(
        column(
          10, offset = 1,
          uiOutput('cyrText')
        )
      ),
      
      # Controls
      fluidRow(
        uiOutput('cyrControls')
      ),
      
      # hr()
      fluidRow(column(10, offset = 1, hr())),
      
      # Graph
      fluidRow(
        column(10, offset = 1, showOutput("cyrChart","nvd3"))
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
          selectInput(
            "tblVars",
            "Välj kolumner att visa i tabellen",
            choices = names(dataFond),
            selected = names(dataFond)[1:5],
            # options = list(maximumSelectionSize = 6),
            multiple = TRUE, selectize = TRUE
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
  
  ## Antalsuppgifter ----
  navbarMenu(
    "Antalsuppgifter",
    
    ## > Antal pensionssparare ----
    tabPanel(
      "Antal pensionssparare",
      
      ## Controls
      fluidRow(
        column(
          5, offset = 1,
          uiOutput("ppsControls")
        ),
        column(
          5,
          uiOutput("ppsAdditionalControls")
        )
      ),
      
      hr(),
      
      ## Content
      fluidRow(
        column(
          5, offset = 1,
          dataTableOutput("ppsTable")
        ),
        column(
          5,
          tabsetPanel(
            type = "pills",
            selected = "Totalmått",
            position = "above",
            tabPanel(
              "Kartdiagram",
               tabsetPanel(
                type = "tabs",
                position = "above",                
                tabPanel("gvisGeoChartProvince",
                         column(4,htmlOutput("gvisGeoChart2"))
                ),
                tabPanel("leaflet",
                         htmlOutput("leafletmap"), #this function will update the leaflet object,
                         #create the leaflet object
                         column(1, leafletMap("map", width="300px", height="400px",
                                             initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                                             initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
                                             options=list(
                                               center = c(63.5 , 16,1),
                                               zoom = 4,
                                               maxBounds = list(list(38, 0), list(88,40)) # Show SE only
                                             )))                        
                         
                ),
                tabPanel("gvisGeoChartLatLong",
                         column(4,htmlOutput("gvisGeoChart1"))
                ),                 
#                 tabPanel("readShapePoly", 
#                          column(4,plotOutput("renderMunicipalityShape", height = "600px", width = "600px"))
#                 ),
                column(width = 3,offset = 4,                      
                       uiOutput("geoControls")
                )
              ) 
            ),
            tabPanel("Totalmått",
                     # fluidRow(plotOutput("ppsPlot")),
                     fluidRow(plotOutput("ppsText"))
            )
          ))
      ),
      ## More content
      fluidRow(
        column(
          5, offset = 1
        ),
        column(
          5
        )
      ),
      
      column(1)
    ),
    
    
    ## > Fondval ----
    tabPanel(
      "Fondval",
      
      ## Controls
      fluidRow(
        column(
          5, offset = 1,
          uiOutput("fvlControls")
        ),
        column(
          5,
          uiOutput("fvlAdditionalControls")
        )
      ),
      
      hr(),
      
      ## Content
      fluidRow(
        column(
          10, offset = 1,
          dataTableOutput("fvlTable")
        )
      ),
      
      ## More content
      fluidRow(
        column(
          5, offset = 1
        ),
        column(
          5
        )
      ),
      
      column(1)
    ),

    
    ## > Dolor ----
    tabPanel(
      "Dolor",
      
      column(1),
      
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
      
      ## > Amet ----
      fluidRow("Amet"),
      
      column(1)
    )
  ),
  
  
  ## Om Applikationen ----
  navbarMenu(
    "Om applikationen",
    tabPanel("Data"),
    tabPanel("Licens"),
    tabPanel("Källkod")
  ),
  
  singleton(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="css/site.css")
    , tags$link(rel="stylesheet", type="text/css", href="css/ribbon.css")
    , tags$link(rel="stylesheet", type="text/css", href="css/dataTable.css")
    ))
)
