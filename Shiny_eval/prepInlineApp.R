prepInlineApp <- function() {
  
  require(Coldbir)
  individDB <- cdb("..//FK_Prep//Data//DB2014_04")
  # load("/Users/love/dev/PrepPilot/FK_Prep/Data/Individdata.DB")
  # print(getwd())
  
  shinyApp(
    ui = fluidPage(
      fluidRow(h2("Fördelning av några variabler", style = "text-align: center;"),
               p("Grafen nedan visar fördelningen av vald variabel i ett histogram 
               längs x-axeln. ", em("Stapelbredden" ), "redovisas i diagramtexten. ",
                 em("Stapelhöjden "), "återspeglar antal personer i varje kategori."),
               p("Genom att förändra reglagevärdena kan olika variabler utforskas
               för olika tidpunkt och olika upplösning."),
               hr()
      ),
      
      # Graph
      fluidRow(plotOutput("distPlot")),
      
      # Controls
      fluidRow(
        column(3,
               selectInput(
                 "variable",
                 "Variabel (y-axel)",
                 choices = c(
                   "Internränta" = "IRR",
                   "Kontovärde" = "Kontovarde",
                   "Månatligt belopp" = "Monamt"
                 ),
                 selected = "IRR",
                 selectize = TRUE
               )
        ),
        column(3,
               sliderInput(
                 "bins",
                 "Upplösning (x-axel)",
                 # Formatting
                 min = 1, max = 11, step = 1, format = "##", locale = "se",
                 ticks = FALSE, animate = TRUE,
                 # Initial value
                 value = 2
               )
        ),
        column(3,
               sliderInput(
                 "year",
                 "År",
                 # Formatting
                 min = 2000, max = 2013, step = 1, format = "####", locale = "se",
                 ticks = FALSE, animate = TRUE,
                 # Initial value
                 value = 2008
               )
        )
      )
    ),
    
    server = function(input, output) {
      ktoData <- reactive({
        data <- individDB[input$variable, c(input$year, 12)][[1]]
        data <- data[!is.na(data)]
        return(data)
      })
      
      # Plot function
      output$distPlot <- renderPlot({
        x <- ktoData()
        x <- x[x < quantile(x, 0.99) & x > quantile(x, 0.01)] #Trim off outlier data
        
        bins <- seq(min(x), max(x),
                    length.out = ifelse(
                      input$bins < 11, 
                      ceiling(exp(input$bins+0.5)/((input$bins-1)^2 + 1)),
                      10000)
        )
        
        # draw the histogram with the specified number of bins
        options(scipen = 10) # We need this to avoid scientific format notation in plot axes
        
        hist(x, breaks = bins,
             col = 'orange', border = ifelse(length(bins) < 100, 'white', 'orange'),
             main = paste0(
               "F\u00F6rdelning av ",
               input$variable
               #            " (kolumnbredd:",
               #            signif(bins[2]-bins[1], 3),
               #            ")"
             ),
             xlab = format(input$variable, scientific = FALSE),
             # ylab = "Antal"
             ylab = "",
             yaxt='n'
        )
      })
    },
    options = list(height = 600))
}