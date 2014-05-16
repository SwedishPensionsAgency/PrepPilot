

library(shiny)

shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
     
    # generate and plot an rnorm distribution with the requested
    # number of observations
    dist <- rnorm(input$obs)
    hist(dist)
    
  })
  
})
