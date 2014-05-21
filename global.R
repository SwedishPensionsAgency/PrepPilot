## REQUIREMENTS FOR THE PrepPilot SHINY APP ##

## Libraries ----
require(shiny) # >= 0.9
require(Coldbir) # >= 0.5
require(data.table) # >= 1.9.2¨
require(ggplot2)
require(sparkle)
require(rCharts)
require(dplyr)

## Data ----
prepDB <- cdb("Data//DataBas")

system.time(base_data <- data.table(
  prepDB['FODAR'],
  prepDB['FONDPLACERINGSAR'],
  prepDB['INTJANANDEAR'],
  prepDB['INTRADESAR'],
  prepDB['SEX']
))


# Fonddata
load("Data//ppindex.RData")
