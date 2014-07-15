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
  
  
  ## > Interaktivt, individuella konton ----
  
  ## Reactive function to generate data for individual-based data
  indData <- reactive({
    input$indUpdate
    
    isolate({
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
        individDB['INTE_DOD_AGARE', input$selYear]
      )
      
      # Name munging
      setnames(plotData,
               c(
                 paste0('INTE_DOD_AGARE___', input$selYear),
                 paste0(input$selXvar, '___', input$selYear)
               ),
               c('INTE_DOD_AGARE', input$selXvar)
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
  })
  
  output$indHeatmap <- renderPlot({
    plotdata <- indData()
    call <- substitute({
      byearSexSummaries <- plotdata %>%
        group_by(FODAR, INTJANANDEAR) %>%
        summarise(
          primmean = mean(primvar, na.rm=TRUE)
        )},
      list(primvar = as.name(input$selXvar))
    )
    eval(call)
    
    # Define plot
    base_size = 14
    
    p <- ggplot(byearSexSummaries, aes(y = as.factor(INTJANANDEAR), x = as.factor(FODAR))) + 
      geom_tile(aes(fill = primmean), colour = "white") + 
      scale_fill_gradient(low = "white", high = "steelblue") + 
      labs(y = "Intj\u00E4nande\u00E5r", x = "F\u00F6delse\u00E5r", title = input$selXvar) + 
      theme_grey(base_size = base_size) +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) + 
      theme(legend.position = "none", 
           axis.ticks = element_blank(), 
           axis.text.x = element_text(size = base_size * 0.8, angle = 330, hjust = 0, colour = "grey50"),
           panel.background = element_blank()
      )
    
    pp <<- copy(p)
    
    print(p)
    
    #     return(p)
  })
  
  
  ## Fondrörelsen ----
  
  
  ## > Tidsserier ----
  # Reactive method for getting ppindex data
  tsdata <- reactive({    
    data <- copy(ppindex) %>%
      setnames(c("UPDEDT", input$tsVar), c("Datum","plotVar")) %>%
      filter(!is.na(plotVar) & Datum >= input$tsStartdate & Datum <= input$tsEnddate) %>%
      mutate(Week = week(Datum), Month = month(Datum), Year = year(Datum))
    
    #     if (input$tsVar %in% c("PPINDEX", "AP7INDEX")) {
    #       data = realIndex(
    #         data,
    #         dateCol = "Datum",
    #         indexCol = "plotVar",
    #         CPICol = "KPI",
    #         dateStart = input$tsStartdate, dateEnd = input$tsEnddate
    #       )
    #     }
    
    # Date filtering
    if (input$tsRes == 7) {
      data <- data %>%
        group_by(Year, Week) %>%
        filter(Datum == min(Datum))
    }
    if (input$tsRes == 30) {
      data <- data %>%
        group_by(Year, Month) %>%
      filter(Datum == max(Datum))
    }    
    
    return(data)
  })
  
  
  # Chart output
  output$fndTimeSeries <- renderChart2({
    # Get data
    plotdata <- tsdata()
    # print(plotdata)
    
    # Define plot
    pr1 <- nPlot(y="plotVar", x="Datum", data = plotdata, type = "lineChart")
    pr1$xAxis(
      tickFormat = "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d * 24 * 60 * 60 * 1000));}!#"
    )
    
    
    return(pr1)
  })
  
  # Download handler
  output$fndDownloadData <- downloadHandler(
    filename = function() {
      paste('fonddata-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      temp_file <- paste0(tempfile(),".xlsx")
      on.exit(unlink(temp_file))
      
      # Function to write XLSX file
      xlfun <- function(input, output) {
        # Error handling
        if(!require(XLConnect)) {
          warning("Could not find package 'XLConnect'. Exporting data to CSV instead of XLS. Please run install.packages('XLConnect') to enable export to XLS.")
          
          write.csv(input, file = output, row.names = FALSE)
        } else {
          wb <- loadWorkbook(output, create = TRUE)
          createSheet(wb, name = "output")
          writeWorksheet(wb, input, sheet = "output")
          saveWorkbook(wb)
        }
      }
      
      # Write file of the selected to tempfile
      if (input$fndFileFormat == "Excel") {
        xlfun(tsdata(), temp_file)
      } else {
        write.csv(tsdata(), file = temp_file, row.names = FALSE)
      }
      
      # Read tempfile and return it to the user
      bytes <- readBin(temp_file, "raw", file.info(temp_file)$size)
      writeBin(bytes, con)
    }
  )
  
  ## > Index ----
  # Reactive method for getting ppindex data
  ixdata <- reactive({    
    data <- copy(ppindex) %>%
      setnames(c("UPDEDT", input$ixVar), c("Datum","plotVar")) %>%
      filter(!is.na(plotVar) & Datum >= input$ixStartdate & Datum <= input$ixEnddate) %>%
      mutate(Week = week(Datum), Month = month(Datum), Year = year(Datum))
    
    if (input$ixYearly) {
      data = realIndexYearlyRate(
        data,
        dateCol = "Datum",
        indexCol = "plotVar",
        CPICol = "KPI",
        dateStart = input$ixStartdate, dateEnd = input$ixEnddate
      )
    } else {
      data = realIndex(
        data,
        dateCol = "Datum",
        indexCol = "plotVar",
        CPICol = "KPI",
        dateStart = input$ixStartdate, dateEnd = input$ixEnddate
      )
    }
    
    # Date filtering
    if (input$ixRes == 7) {
      data <- data %>%
        group_by(Year, Week) %>%
        filter(Datum == min(Datum))
    }
    if (input$ixRes == 30) {
      data <- data %>%
        group_by(Year, Month) %>%
        filter(Datum == max(Datum))
    }    
    
    return(data)
  })
  
  
  # Chart output
  output$fndIndexSeries <- renderChart2({
    # Get data
    plotdata <- ixdata()
    # print(plotdata)
    
    # Define plot
    if (input$ixYearly) {
      pr1 <- nPlot(y="YEARLY", x="Datum", data = plotdata, type = "lineChart")
    } else {
      pr1 <- nPlot(y="plotVar", x="Datum", data = plotdata, type = "lineChart")
    }
    
    pr1$xAxis(
      tickFormat = "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d * 24 * 60 * 60 * 1000));}!#"
    )
    
    return(pr1)
  })
  
  
  ## > Värdeutveckling per kalenderår ----
  # Reactive function to get yearly data
  yrdata <- reactive({    
    data <- copy(ppindex) %>%
      setnames(c("UPDEDT", input$yrVar), c("Datum","plotVar")) %>%
      # Select only relevant variables
      select(Datum, plotVar) %>%
      # Remove NAs
      filter(!is.na(plotVar)) %>%
      # Find timestamps
      mutate(Year = year(Datum)) %>%
      # Year 2000 and 2014 are incomplete so we exclude them
      filter(Year > 2000 & Year < 2014) %>%
      # Keep only first and last observation of the year
      group_by(Year) %>%
      filter(Datum %in% c(min(Datum), max(Datum)))
    
    if (input$yrDiff) {
      data <- data %>%
        # Calculate fund development and discard unneccessary rows
        mutate(growth = plotVar / lag(plotVar, 1, default = plotVar[1]) - 1) %>%
        filter(Datum == max(Datum))
    } else {
      data <- data %>%
        mutate(growth = plotVar) %>%
        filter(Datum == max(Datum))
    }
  })
  
  # Render plot
  output$fndYearlyGrowth <- renderChart2({
    # Get data
    plotdata <- yrdata()
    # print(plotdata)

    # Define plot
    pr2 <- nPlot(growth ~ Year, data = plotdata, type = "multiBarChart")
    pr2$yAxis(axisLabel = "\u00C5rlig tillv\u00E5xt", width = 62)
    pr2$xAxis(axisLabel = "\u00C5r")
    
    return(pr2)
  })
  
  ## > Tabeller ----
  
  tbldata <- reactive({
    dataFond[,input$tblVars, with = FALSE]
  })
  
  output$ppTables <- renderDataTable(
  {
    ## TODO: Fyll i här!
    tbldata()
  },
  options = list(
    aLengthMenu = list(c(5, 10, 20, -1), c('5', '10', '20', 'All')),
    bSortClasses = TRUE,
    iDisplayLength = 10,
    bFilter = list(
      caseInsensitive = TRUE
    )
  )
  )
})
