library(shiny)

shinyServer(function(input, output, session) {
  
  ## Kontoutveckling ----
  
  ## > Fördelning, individuella konton ("kto") ----
  
  ## > Fördelning, individuella konton >> Data ----
  ktoData <- reactive({
    data <- individDB[input$variable, c(input$year, 12)][[1]]
    data <- data[!is.na(data)]
    
    return(data)
  })
  
  ## > Fördelning, individuella konton >> Text ----
  output$ktoText <- renderUI({tagList(
    h2("Fördelning av några variabler", style = "text-align: center;"),
    p("Grafen nedan visar fördelningen av vald variabel i ett histogram 
            längs x-axeln. ", em("Stapelbredden" ), "redovisas i diagramtexten. ",
      em("Stapelhöjden "), "återspeglar antal personer i varje kategori."),
    p("Genom att förändra reglagevärdena kan olika variabler utforskas
            för olika tidpunkt och olika upplösning."),
    hr()
  )})
  
  ## > Fördelning, individuella konton >> Plot ----
  output$ktoPlot <- renderPlot({
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
  
  ## > Fördelning, individuella konton >> Controls ----
  output$ktoControls <- renderUI({tagList(
    column(2, offset = 3,
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
    )
  )})
  
  
  ## Fondrörelsen ----
  
  ## > Tidsserier ("fts") ----
  ## > Tidsserier >> Data ----
  ftsData <- reactive({    
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
  
  ## > Tidsserier >> Text ----
  output$ftsText <- renderUI({tagList(
    h2("Tidsserier", style="text-align: center;"),
    tags$small(p("Grafen visar den historiska utvecklingen för valt index
                       på dagsbasis mellan 2000-12-13 och 2014-03-20. Index=100 är bestämt
                       tid tidpunkten ", em("t "), "= 2000-12-13."),
               p("För musen över grafen för att se värdet för en enskild punkt i grafen.")),
    hr()
  )})
  
  ## > Tidsserier >> Controls ----
  output$ftsControls <- renderUI({tagList(
    column(
      2, offset = 1,
      selectInput(
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
        selectize = TRUE
      )
    ),
    column(
      2,
      selectInput(
        "tsRes",
        "Upplösning för tidsserie",
        choices = c(
          "Dag" = 1,
          "Vecka" = 7,
          "Månad" = 30
        ),
        selected = 30,
        selectize = TRUE
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
  )})
  
  ## > Tidsserier >> Chart ----
  output$ftsChart <- renderChart2({
    # Get data
    plotdata <- tsdata()
    
    # Define plot
    pr1 <- nPlot(y="plotVar", x="Datum", data = plotdata, type = "lineChart")
    pr1$xAxis(
      tickFormat = "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d * 24 * 60 * 60 * 1000));}!#"
    )
    
    
    return(pr1)
  })
  
  ## > Tidsserier >> Download handler ----
  output$ftsDownloadHandler <- downloadHandler(
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
  
  ## > Index ("fix") ----
  ## > Index >> Data ----
  fixData <- reactive({    
    data <- copy(tidsserie) %>%
      setnames(c("UPDEDT", input$fixVar), c("Datum","plotVar")) %>%
      filter(!is.na(plotVar) & Datum >= input$fixStartdate & Datum <= input$fixEnddate) %>%
      mutate(Week = week(Datum), Month = month(Datum), Year = year(Datum))
    
    if (input$fixYearly) {
      data = realIndexYearlyRate(
        data,
        dateCol = "Datum",
        indexCol = "plotVar",
        CPICol = "KPI",
        dateStart = input$fixStartdate, dateEnd = input$fixEnddate
      )
    } else {
      data = realIndex(
        data,
        dateCol = "Datum",
        indexCol = "plotVar",
        CPICol = "KPI",
        dateStart = input$fixStartdate, dateEnd = input$fixEnddate
      )
    }
    
    # Date filtering
    if (input$fixRes == 7) {
      data <- data %>%
        group_by(Year, Week) %>%
        filter(Datum == min(Datum))
    }
    if (input$fixRes == 30) {
      data <- data %>%
        group_by(Year, Month) %>%
        filter(Datum == max(Datum))
    }
    
    data <- data %>% mutate(Datum = as.double(Datum))
    
    return(data)
  })
  
  ## > Index >> Text ----
  output$fixText <- renderUI({tagList(
    h2("Indexserier", style="text-align: center;"),
    tags$small(
      p("Här visas värdeutvecklingen för fondrörelsen, dvs alla fonder inklusive Premiesparfonden och AP7 Såfa sedan systemets start 2000-12-13."),
      p("Tidsviktad avkastning, TVA mäter avkastningen på en genomsnittlig krona som sattes in i premiepensionssystemet 2000-12-13. Premiepensionsindex kan jämföras med marknadsindex och enskilda fonders avkastning. Rabatten på fondavgiften, myndighetens avgift och arvsvinster är inte medräknade."),
      p("Real avkastning = avkastning/inflation (KPI)", br(),
        "Med årsavkastning menas genomsnittlig avkastning sedan start.")
    ),
    hr()
  )})
  
  ## > Index >> Controls ----
  output$fixControls <- renderUI({tagList(
    fluidRow(
      column(
        2, offset = 1,
        selectInput(
          "fixVar",
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
          selectize = TRUE
        ),
        checkboxInput(
          "fixYearly",
          "Beräkna årsavkastning",
          value = FALSE
        )
      ),
      column(
        2,
        selectInput(
          "fixRes",
          "Upplösning för tidsserie",
          choices = c(
            "Dag" = 1,
            "Vecka" = 7,
            "Månad" = 30
          ),
          selected = 30,
          selectize = TRUE
        )
      ),
      column(
        2,
        dateInput(
          "fixStartdate", "Välj tidsperiod", value = "2000-12-13",
          min = "2000-12-13", max = "2014-04-30", weekstart = 1,
          startview = "year",
          language = "sv"
        ),
        dateInput(
          "fixEnddate", "", value = "2014-04-30",
          min = "2000-12-13", max = "2014-04-30", weekstart = 1,
          startview = "year",
          language = "sv"
        )
      )
    )
  )})
  
  ## > Index >> Chart ----
  output$fixChart <- renderChart2({
    # Get data
    plotdata <- fixData()
    
    # Define plot
    if (input$fixYearly) {
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
  
  tblData <- reactive({
    dataFond[,input$tblVars, with = FALSE]
  })
  
  output$ppTables <- renderDataTable({
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
  ))

  ## Antalsuppgifter ----
  
  ## > Antal pensionssparare ("pps") ----
  ## > Antal pensionssparare >> Data ----
  ppsData <- reactive({
    # This is where we put all relevant data modifications
    print(input$measure)
    data <- switch(
      c(input$measure),
      "Prdcod100" = base_data %>%
        mutate(Prdcod = individDB['Prdcod', c(2014,4)][[1]]) %>%
        filter(Prdcod == 100),
      "Prdcod110" = base_data %>%
        mutate(Prdcod = individDB['Prdcod', c(2014,4)][[1]]) %>%
        filter(Prdcod == 110),
      "Prdcod120" = base_data %>%
        mutate(Prdcod = individDB['Prdcod', c(2014,4)][[1]]) %>%
        filter(Prdcod == 120),
      "Prdcod110120" = base_data %>%
        mutate(Prdcod = individDB['Prdcod', c(2014,4)][[1]]) %>%
        filter(Prdcod == 110 | Prdcod == 120),
      "Prdcod210" = base_data %>%
        mutate(Prdcod = individDB['Prdcod', c(2014,4)][[1]]) %>%
        filter(Prdcod == 210),
      "Prdcod220" = base_data %>%
        mutate(Prdcod = individDB['Prdcod', c(2014,4)][[1]]) %>%
        filter(Prdcod == 220),
      "Prdcod210220" = base_data %>%
        mutate(Prdcod = individDB['Prdcod', c(2014,4)][[1]]) %>%
        filter(Prdcod == 210 | Prdcod == 220),
      "AntalFondval" = base_data %>%
        mutate(AntalFondval = individDB['AntalFondval', c(2014,3)][[1]]) %>%
        filter(AntalFondval > 0),
      "Kontovarde" = base_data %>%
        mutate(Kontovarde = individDB['Kontovarde', c(2014,4)][[1]]),
      base_data # Default
    )
    data
  })
  
  base_data
  
  geoData <- reactive({
    coordinates = tbl_df(geoTblRegion)   
    ppsDataGeo <- left_join(ppsData(), coordinates, by = "region")
    
    #convert selection to province code instead of text
    #todo: throws exception on join
    regionSelection <- left_join(tbl_df(data.frame(regionText = input$regionInput))
                                ,coordinates, 
                                by = "regionText")
    
    ppsDataGeo <- tbl_df(data.frame(
      region = ppsDataGeo['region'][[1]],
      regionText = ppsDataGeo['regionText'][[1]],
      lat = ppsDataGeo['lat'][[1]],
      long = ppsDataGeo['long'][[1]]
    )) %>%
      filter(region %in% regionSelection$region) %>%
      group_by(region, regionText) %>%      
      summarise(freq = n(), lat = max(ave(lat)), long = max(ave(long))) %>%
      arrange(region) %>%
      mutate(latlong = paste(lat, long, sep = ":")) #%>%
      #select(region:regionText:latlong)
    
    return(ppsDataGeo)
  })
  
  
  ## > Antal pensionssparare >> Controls ----
  output$ppsControls <- renderUI({list(
    column(12, wellPanel(selectInput(
      "measure",
      "M\u00e5tt",
      choices = c("Totalt antal pensionssparare" = "Prdcod100",
                  "Antal pensionssparare med fondf\u00f6rs\u00e4kring och efterlevandeskydd under pensionstiden" = "Prdcod120",
                  "Antal pensionssparare med fondf\u00f6rs\u00e4kring utan efterlevandeskydd under pensionstiden" = "Prdcod110",
                  "Antal pensionssparare med fondf\u00f6rs\u00e4kring under pensionstiden" = "Prdcod110120",
                  "Antal pensionssparare med traditionell f\u00f6rs\u00e4kring och efterlevandeskydd under pensionstiden" = "Prdcod220",
                  "Antal pensionssparare med traditionell f\u00f6rs\u00e4kring utan efterlevandeskydd under pensionstiden" = "Prdcod210",
                  "Antal pensionssparare med traditionell f\u00f6rs\u00e4kring under pensionstiden" = "Prdcod210220",
                  "Antal personer som bytt fonder f\u00f6reg\u00e5ende m\u00e5nad" = "AntalFondval",
                  "Genomsnittlig beh\u00e5llning per pensionssparare under pensionstiden (marknadsv\u00e4rde)" = "Ktovarde",
                  "Genomsnittlig beh\u00e5llning per pensionssparare f\u00f6r samtliga pensionssparare (marknadsv\u00e4rde)" = "J",
                  "Genomsnittlig beh\u00e5llning per pensionssparare f\u00f6re pensionstiden (marknadsv\u00e4rde)" = "K",
                  "Genomsnittlig intj\u00e4nad pensionsr\u00e4tt f\u00f6r premiepension per pensionssparare f\u00f6re pensionstiden" = "L",
                  "Totalt antal pensionssparare f\u00f6re pensionstiden" = "M",
                  "Totalt antal pensionssparare under pensionstiden" = "N"),
      selected = "Totalt antal pensionssparare",
      width = '100%'
    )))
  )})
  
  output$ribbon <- renderUI({
          div(class="ribbon-banner", "Beta")
  })
  
  output$ppsAdditionalControls <- renderUI({tagList(
    column(3, selectInput(
      "grpvar",
      "Visa grupper",
      choices = c("K\u00f6n" = "Kon",
                  "Lever" = "Lever",
                  "\u00c5ldersgrupp" = "Aldgrp"),
      selected = "K\u00f6n",
      multiple = TRUE
    )),
    column(3, selectInput(
      "time",
      "Tid",
      choices = c(201401:201404),
      selected = "201401"
    )),
    column(3, selectInput(
      "kommun",
      "Kommun",
      choices = c("Samtliga"),
      selected = "Samtliga"
    )),         
    column(3)
  )})
  
  ## > Antal pensionssparare >> Tables ----
  output$ppsTable <- renderDataTable({
    partTable(ppsData(), byvar = input$grpvar, grpvar = "Aldgrp", sumrow = "top")
  },
  options = list(
    "bFilter" = FALSE,
    "iDisplayLength" = 25,
    "bLengthChange" = FALSE)
  )
  
  ## > Antal pensionssparare >> Graphs ----
  output$ppsPlot <- renderPlot({
    p <- ggplot(base_data, aes(x = Aldgrp, y = Fodelsemanad)) + geom_boxplot()
    p
  })
  
  output$geoControls <- renderUI({
    geoTblRegion['regionText'][[1]]
    selectInput(multiple = TRUE,
                label = "L\u00e4n",
                inputId = "regionInput",
                choices = sort(unique(geoTblRegion['regionText'][[1]])),
                selected = sort(unique(geoTblRegion['regionText'][[1]])),
                width = "100%")
  })
  
  output$gvisGeoChart1 <- renderGvis({
    geoData()
    obj <- gvisGeoChart(
      data = geoData(),
      locationvar = "latlong",
      sizevar="freq",
      colorvar="freq",
      hovervar = "regionText",
      chartid = "gvisGeoChart1",
      options=list(
        region="SE"
        , showTip=TRUE
        , height = 500
        , width = 500        
        , resolution="provinces"
        , colorAxis = "{colors: ['#e7711c', '#4374e0']}"
        #, colorAxis="{values:[200000,400000,600000,800000, 1000000],
        #            colors:[\'red', \'pink\', \'blue', \'orange',\'green']}"
      )
    )
    return(obj)
  })  
  
  output$gvisGeoChart2 <- renderGvis({
    obj <- gvisGeoChart(
      data = geoData()
      , colorvar="freq"
      , hovervar="regionText"
      , chartid = "gvisGeoChart2"
      , options=list(
        region="SE"
        , showTip=TRUE
        , height = 500
        , width = 500
        , resolution="provinces"
        , colorAxis = "{colors: ['#e7711c', '#4374e0']}"
        , enableRegionInteractivity = TRUE
      )
    )
    return(obj)
  })
  
  #function to update leaflet map object
  output$leafletmap <- renderGvis({
    data <- geoData()
    map <- createLeafletMap(session, 'map')
    map$clearShapes()
    for(i in 1:nrow(geoData())){
      map$addCircle(
        geoData()$lat[i],
        geoData()$long[i],
        sqrt(geoData()$freq[i]) *800 / max(5, input$map_zoom)^2,
        geoData()$regionText[i],
        list(stroke=FALSE, fill=TRUE, fillOpacity=0.4, color='#FF0000')
      )
    }        
    return(NULL)
  })  
    
  output$renderMunicipalityShape <- renderPlot({
    geoShapeMunicipality <- readShapeSpatial("Data/geoCounty_SCB/alla_lan.shp")
    plot(geoShapeMunicipality)
  })
  
  
  
  ## > Fondval ("fvl") ----
  ## > Fondval >> Data ----
  fvlData <- reactive({
    # This is where we put all relevant data modifications
    print(input$fvlMeasure)
    data <- base_id
    
    print(input$fvlMunicipality )
    
    if (!"Samtliga" %in% input$fvlMunicipality ) {
      data <- data %>% filter(municipality %in% input$fvlMunicipality)
    }
    
    data
  })
  
  ## > Fondval >> Controls ----
  output$fvlControls <- renderUI({list(
    column(12, wellPanel(selectInput(
      "fvlMeasure",
      "M\u00e5tt",
      choices = c("Antal valda fonder" = 'Antal',
                  "Marknadsv\u00e4rde SEK" = 'MV'),
      selected = "Antal valda fonder",
      width = '100%',
      selectize = TRUE
    )))
  )})
  
  output$fvlAdditionalControls <- renderUI({tagList(
    column(3, selectInput(
      "fvlByvar",
      "Horisontell indelning",
      choices = c("Kategori (bred)" = "Kategori_bred",
                  "Kategori (smal)" = "Kategori_smal",
                  "Fondtyp" = "Fondtyp"),
      selected = "Kategori (bred)",
      multiple = FALSE
    )),
    column(3, selectInput(
      "fvlGrpvar",
      "Vertikal indelning",
      choices = c("Kategori (bred)" = "Kategori_bred",
                  "Kategori (smal)" = "Kategori_smal",
                  "Fondtyp" = "Fondtyp"),
      selected = "Fondtyp",
      multiple = FALSE
    )),
    column(3, selectizeInput(
      "fvlMunicipality",
      "Kommun",
      choices = c("Samtliga", sort(as.character(unique(base_id$municipality)))),
      selected = "Samtliga",
      multiple = TRUE,
      options = list(
        'maxItems' = 5
      )
    )),
    column(3)
  )})
  
  ## > Fondval >> Tables ----
  output$fvlTable <- renderDataTable({
    tblInput <- switch(
      input$fvlMeasure,
      'Antal' = list(.fun = NULL, funvar = NULL),
      list(.fun = sum, funvar = input$fvlMeasure)
    )

    partTable(fvlData(), byvar = input$fvlByvar, grpvar = input$fvlGrpvar, sumrow = "top", .fun = tblInput$.fun, funvar = tblInput$funvar)
  },
  options = list(
    "bFilter" = FALSE,
    "iDisplayLength" = 20,
    "bLengthChange" = FALSE)
  )
  
})
