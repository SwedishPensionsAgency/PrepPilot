library(shiny)

shinyServer(function(input, output) {
  
  ## Antalsuppgifter ----
  
  ## > Antal pensionssparare ("pps") ----
  ## >> Data ----
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
  
  ## >> Controls ----
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
  
  output$ppsAdditionalControls <- renderUI({list(
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
  
  ## >> Tables ----
  output$ppsTable <- renderDataTable({
    partTable(ppsData(), byvar = input$grpvar, grpvar = "Aldgrp", sumrow = "top")
  },
  options = list(
    "bFilter" = FALSE,
    "iDisplayLength" = 20,
    "bLengthChange" = FALSE))
  
})
