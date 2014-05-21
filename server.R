library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ## Kontoutveckling ----
  
  ## > Fördelning, individuella konton ----
  distData <- reactive({
    data <- prepDB[input$variable, input$year][[1]]
    
    return(data)
  })
  
  output$distPlot <- renderPlot({
    x <- distData()
    x <- x[!is.na(x)]
    x <- x[x < quantile(x, 0.99) & x > quantile(x, 0.01)] #Trim off outlier data
    
    bins <- seq(min(x), max(x),
                length.out = ifelse(input$bins < 11, ceiling(exp(input$bins+0.5)/((input$bins-1)^2 + 1)), 10000))
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'skyblue', border = ifelse(length(bins) < 100, 'white', 'skyblue'))
    
  })
  
  
  ## > Interaktivt, individuella konton ----
  indData <- reactive({
    input$chbSEX
  })
  
  output$indPlot <- renderPlot({
    plotdata <- indData()
  })
  
  
  ## Fondrörelsen ----
  
  ## > Värdeutveckling per kalenderår ----
  output$fndTimeSeries <- renderChart2({
    plotdata <- copy(ppindex)
    
    pr1 <- nPlot(PPINDEX ~ UPDEDT, data = plotdata, type = "lineChart")
    pr1$xAxis(
      tickFormat = "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d * 24 * 60 * 60 * 1000));}!#"
    )
    pr1$layer(PPINDEX ~ UPDEDT, data = plotdata, type = "multiBarChart")
    
    return(pr1)
    
    #     hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
    #     n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, type = "multiBarChart")
    #     return(n1)
  })
})
