## REQUIREMENTS FOR THE PrepPilot SHINY APP ##

## Libraries ----
require(shiny) # >= 0.9
require(Coldbir) # install_github("SwedishPensionsAgency/Coldbir")
require(data.table) # >= 1.9.2¨
require(ggplot2)
require(sparkle) # install_github("metagraf/sparkle")
require(rCharts) # install_github("ramnathv/rCharts", ref="dev")
require(plyr)
require(dplyr)
require(ggthemes)

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
load("Data//indexData.RData")
# indexData.RData contains a data.frame called "data" so we rename it
ppindex <- tbl_dt(data); rm(data)
ppindex[,UPDEDT := as.Date(as.character(UPDEDT))]

load("Data//dataFond.RData")



## Functions ---.-
source("functions.R")
