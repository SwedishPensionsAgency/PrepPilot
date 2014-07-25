

library(shiny)

# Define UI for application that draws a histogram
navbarPage(
  title = "FK PreP",
  collapsable = TRUE,
  
  # If we want, we could add code here that would be displayed on all the pages.
  # This could be useful for e.g. providing a logo beneath the menu, a 
  # copyright footer or other nice CI elements.
  # header = tagList(column(1), column(10, p("Test")), column(1)),
  footer = tagList(column(1), column(10, p(tags$small("Denna applikation är en prototyp utvecklad av Pensionsmyndigheten och är för närvarande under utveckling. Det finns ingen garanti för att uppgifterna i applikationen är tillförlitliga då dessa inte genomgått kvalitetssäkring och delvis bygger på fabricerad data.", style = "color: Grey;"))), column(1)),
  
  tabPanel(
    "Start", 
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
                tabPanel("gvisGeoChartLatLong",
                         column(4,htmlOutput("gvisGeoChart1"))
                ),    
                tabPanel("gvisGeoChartProvince",
                         column(4,htmlOutput("gvisGeoChart2"))
                ),                  
                tabPanel("readShapePoly", 
                         column(4,plotOutput("renderMunicipalityShape", height = "600px", width = "600px"))
                ),
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
  
  singleton(tags$head(
    tags$style("h2 { font-family: 'Helvetica Neue', Helvetica; font-weight: 300; }")    
    ))
)
