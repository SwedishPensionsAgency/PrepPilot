library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  prepData <- reactive({
    base_data$xvar <- prepDB[input$variable, 2012]
    
    return(base_data)
  })
  
  
  output$distPlot <- renderPlot({
    data = prepData()
    x    <- data$xvar
    x <- x[!is.na(x)]
    x <- x[x < quantile(x, 0.99) & x > quantile(x, 0.01)] #Trim off outlier data
    
    bins <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'skyblue', border = 'white')
  })
})
