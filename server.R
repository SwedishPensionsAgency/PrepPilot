library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ## Kontoutveckling ----
  
  ## > Fördelning, individuella konton ----
  distData <- reactive({
    data <- individDB[input$variable, input$year][[1]]
    data <- data[!is.na(data)]
    
    return(data)
  })
  
  output$distPlot <- renderPlot({
    x <- distData()
    x <- x[x < quantile(x, 0.99) & x > quantile(x, 0.01)] #Trim off outlier data
    
    bins <- seq(min(x), max(x),
                length.out = ifelse(
                  input$bins < 11, 
                  ceiling(exp(input$bins+0.5)/((input$bins-1)^2 + 1)),
                  10000)
    )
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'skyblue', border = ifelse(length(bins) < 100, 'white', 'skyblue'))
    
  })
  
  
  ## > Interaktivt, individuella konton ----
  indData <- reactive({
    # Error handling
    if (is.null(input$selINTJANANDEAR) |
          is.null(input$selFODAR) |
          is.null(input$chbSEX) |
          is.null(input$chbINTE_DOD_AGARE)
    ) { return() }
    
    plotData <- data.table(
      base_data,
      individDB[input$selXvar, input$selYear],
      individDB[input$selYvar, input$selYear],
      individDB['INTE_DOD_AGARE', input$selYear]
    )
    setnames(plotData,
             c(
               paste0('INTE_DOD_AGARE___', input$selYear),
               paste0(input$selXvar, '___', input$selYear),
               paste0(input$selYvar, '___', input$selYear)
             ),
             c('INTE_DOD_AGARE', input$selXvar, input$selYvar)
    )
    
    plotData <- plotData[SEX %in% input$chbSEX &
                           INTE_DOD_AGARE %in% input$chbINTE_DOD_AGARE &
                           INTJANANDEAR %in% input$selINTJANANDEAR &
                           FODAR %in% input$selFODAR &
                           !is.na(plotData[[input$selXvar]]) &
                           !is.na(plotData[[input$selXvar]])
                         ]
    
    cat("Plotdata för individplot har", nrow(plotData), "rader.\n")
    print(names(plotData))
    
    return(plotData)
  })
  
  output$indPlot <- renderChart({
    plotdata <- indData()
    
    # Summarizations
    call <- substitute({
      byearSexSummaries <- plotdata %>%
        group_by(FODAR, SEX) %>%
        summarise(
          primmean = mean(primvar, na.rm=TRUE),
          secmean = mean(secvar, na.rm=TRUE)
        )},
      list(primvar = as.name(input$selXvar),
           secvar = as.name(input$selYvar))
    )
    eval(call)
    
    pr1 <- rPlot("primmean" ~ "FODAR", data = byearSexSummaries,
                 type = "point", color = "SEX")
    pr1$addParams(height = 300, dom = 'indPlot', 
                     title = "SomeTitle")
    
    return(pr1)
    
  })
  
  output$indPlot2 <- renderChart({
    plotdata <- indData()
    
    pr2 <- nPlot(y="IRR", x="FODAR", group = "SEX", data = plotdata, type = "multiBarChart")
    
    return(pr2)
  })
  
  output$heatmap <- renderPlot({
    print("Hej")
    plotdata <- indData()
    call <- substitute({
      byearSexSummaries <- plotdata %>%
        group_by(FODAR, SEX) %>%
        summarise(
          primmean = mean(primvar, na.rm=TRUE),
          secmean = mean(secvar, na.rm=TRUE)
        )},
      list(primvar = as.name(input$selXvar),
           secvar = as.name(input$selYvar))
    )
    eval(call)
    print("Hej2")
    print(byearSexSummaries)
    
    p <- ggplot(byearSexSummaries, aes(x = as.factor(SEX), y = as.factor(FODAR))) + 
      geom_tile(aes(fill = primmean), colour = "white") + 
      scale_fill_gradient(low = "white", high = "steelblue") + 
      labs(x = "Kön", y = "Födelseår")
    print(p)
    
    return(p)
  })
})
