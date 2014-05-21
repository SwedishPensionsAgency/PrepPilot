## REQUIREMENTS FOR THE PrepPilot SHINY APP ##

## Libraries ----
require(shiny) # >= 0.9
require(Coldbir) # >= 0.5
require(data.table) # >= 1.9.2¨
require(ggplot2)
require(sparkle)
require(dplyr)
require(rCharts)

## Data ----
individDB <- cdb("Data//DataBas")

system.time(base_data <- data.table(
  individDB['FODAR'],
  individDB['FONDPLACERINGSAR'],
  individDB['INTJANANDEAR'],
  individDB['INTRADESAR'],
  individDB['SEX']
))

birthYears <- sort(unique(base_data$FODAR))
entryYears <- sort(unique(base_data$INTJANANDEAR))

# Fonddata: ppindex
load("Data//ppindex.RData")
