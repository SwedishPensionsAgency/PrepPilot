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
require(tidyr)
require(ggthemes)
require(XLConnect)
require(pmreports) # install_stash("pmreports")
require(stringr)

## Data ----
# individDB <- cdb("Data//DB2014_04")
load("Data//Individdata.DB")
 
system.time(
  base_data <- tbl_df(data.table(
    individDB['Fodelsear'],
    individDB['Fodelsemanad'],
    individDB['Kon'],
    individDB['Lever', c(2014, 4)]
  )) %>% 
    mutate(Alder = 2014 - Fodelsear,
           Aldgrp = cut(Alder, seq(0, 120, 5))
    ) %>%
    rename(c("Lever___2014___4" = "Lever"))
)


# birthYears <- sort(unique(base_data$FODAR))
# entryYears <- sort(unique(base_data$INTJANANDEAR))
# 
# # Fonddata: ppindex
# load("Data//tidsserie.RData")
# # tidsserie.RData contains a data.frame called "tidsserie" so we rename it
# ppindex <- tbl_dt(tidsserie); rm(tidsserie)
# ppindex[,UPDEDT := as.Date(as.character(UPDEDT))]
# 
# # Fonddata: 
# load("Data//dataFond.RData")


## Functions ---.-
source("functions.R")

## > Create variable list for dplyr select() operations ----
varlist = function(x) {
  x = str_c('^(',paste(x, collapse='|'),')$')
  x = str_replace_all(x,'\\.','\\\\.')
  return(x)
}

