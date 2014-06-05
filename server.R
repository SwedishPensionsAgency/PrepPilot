library(shiny)

shinyServer(function(input, output) {
  
  ## Kontoutveckling ----
  
  ## > Fördelning, individuella konton ----
  
  # Reactive function to generate data for the distribution plot
  distData <- reactive({
    data <- individDB[input$variable, input$year][[1]]
    data <- data[!is.na(data)]
    
    return(data)
  })
  
  # Plot function
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
    options(scipen = 10) # We need this to avoid scientific format notation in plot axes
    
    hist(x, breaks = bins,
         col = 'skyblue', border = ifelse(length(bins) < 100, 'white', 'skyblue'),
         main = paste0(
           "F\u00F6rdelning av ",
           input$variable,
           " (kolumnbredd:",
           signif(bins[2]-bins[1], 3),
           ")"
         ),
         xlab = format(input$variable, scientific = FALSE),
         ylab = "Antal"
    )
    
  })
  
  
  ## > Interaktivt, individuella konton ----
  
  ## Reactive function to generate data for individual-based data
  indData <- reactive({
    
    # Error handling
    if (is.null(input$selINTJANANDEAR) |
          is.null(input$selFODAR) |
          is.null(input$chbSEX) |
          is.null(input$chbINTE_DOD_AGARE)
    ) { return() }
    
    # Load data from Coldbir database
    plotData <- data.table(
      base_data,
      individDB[input$selXvar, input$selYear],
      individDB[input$selYvar, input$selYear],
      individDB['INTE_DOD_AGARE', input$selYear]
    )
    
    # Name munging
    setnames(plotData,
             c(
               paste0('INTE_DOD_AGARE___', input$selYear),
               paste0(input$selXvar, '___', input$selYear),
               paste0(input$selYvar, '___', input$selYear)
             ),
             c('INTE_DOD_AGARE', input$selXvar, input$selYvar)
    )
    
    # Filter by parameters from selector menu
    plotData <- plotData[SEX %in% input$chbSEX &
                           INTE_DOD_AGARE %in% input$chbINTE_DOD_AGARE &
                           INTJANANDEAR %in% input$selINTJANANDEAR &
                           FODAR %in% input$selFODAR &
                           !is.na(plotData[[input$selXvar]]) &
                           !is.na(plotData[[input$selXvar]])
                         ]
    
    cat("Plotdata för individplot har", nrow(plotData), "rader.\n")
    
    return(plotData)
  })
  
  ## THE FOLLOWING RCHARTS FUNCTIONS ARE NOT IN USE AND SHOULD BE CONSIDERED FOR DELETION
  #   output$indPlot <- renderChart({
  #     plotdata <- indData()
  #     
  #     # Summarizations
  #     call <- substitute({
  #       byearSexSummaries <- plotdata %>%
  #         group_by(FODAR, SEX) %>%
  #         summarise(
  #           primmean = mean(primvar, na.rm=TRUE),
  #           secmean = mean(secvar, na.rm=TRUE)
  #         )},
  #       list(primvar = as.name(input$selXvar),
  #            secvar = as.name(input$selYvar))
  #     )
  #     eval(call)
  #     
  #     pr1 <- rPlot("primmean" ~ "FODAR", data = byearSexSummaries,
  #                  type = "point", color = "SEX")
  #     pr1$addParams(height = 300, dom = 'indPlot', 
  #                      title = "SomeTitle")
  #     
  #     return(pr1)
  #     
  #   })
  #   
  #   output$indPlot2 <- renderChart2({
  #     plotdata <- indData()
  #     
  #     pr2 <- nPlot(y="IRR", x="FODAR", group = "SEX", data = plotdata, type = "multiBarChart")
  #     
  #     return(pr2)
  #   })
  
  
  output$indHeatmap <- renderPlot({
    plotdata <- indData()
    call <- substitute({
      byearSexSummaries <- plotdata %>%
        group_by(FODAR, INTJANANDEAR) %>%
        summarise(
          primmean = mean(primvar, na.rm=TRUE),
          secmean = mean(secvar, na.rm=TRUE)
        )},
      list(primvar = as.name(input$selXvar),
           secvar = as.name(input$selYvar))
    )
    eval(call)
    
    # Define plot
    base_size = 14
    
    p <- ggplot(byearSexSummaries, aes(x = as.factor(INTJANANDEAR), y = as.factor(FODAR))) + 
      geom_tile(aes(fill = primmean), colour = "white") + 
      scale_fill_gradient(low = "white", high = "steelblue") + 
      labs(x = "Intjänandeår", y = "Födelseår", title = input$selXvar) + 
      theme_grey(base_size = base_size) +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) + 
      theme(legend.position = "none", 
           axis.ticks = element_blank(), 
           axis.text.x = element_text(size = base_size * 0.8, angle = 330, hjust = 0, colour = "grey50"),
           panel.background = element_blank()
      )
    
    print(p)
    
    #     return(p)
  })
  
  
  ## Facet density plot
  output$indDensity <- renderPlot({
    NULL
  })
  
  
  ## Fondrörelsen ----
  
  ## > Tidsserier ----
  output$fndTimeSeries <- renderChart2({
    # Get data
    plotdata <- copy(ppindex)
    
    # Define plot
    pr1 <- nPlot(PPINDEX ~ UPDEDT, data = plotdata, type = "lineChart")
    pr1$xAxis(
      tickFormat = "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d * 24 * 60 * 60 * 1000));}!#"
    )
    
    return(pr1)
  })
  
  ## > Värdeutveckling per kalenderår ----
  output$fndYearlyGrowth <- renderChart2({
    # Get data
    plotdata <- ppindex %>%
      # Find year of datestamp
      mutate(year = year(UPDEDT)) %>%
      # Year 2000 and 2014 are incomplete so we exclude them
      filter(year > 2000 & year < 2014) %>%
      # Keep only first and last observation of the year
      group_by(year) %>%
      filter(UPDEDT %in% c(min(UPDEDT), max(UPDEDT))) %>%
      # Calculate fund development and discard unneccessary rows
      mutate(growth = PPINDEX / lag(PPINDEX, 1, default = PPINDEX[1]) - 1) %>%
      filter(UPDEDT == max(UPDEDT))
      
    # Define plot
    pr2 <- nPlot(growth ~ year, data = plotdata, type = "multiBarChart")
    pr2$yAxis(axisLabel = "Årlig tillväxt", width = 62)
    pr2$xAxis(axisLabel = "År")
    
    return(pr2)
  })
  
  
  ## DEV ----
#   output$text <- renderText({
#     sprintf("The capital of %s is %s.", "a", input$click$)
#   })
})
