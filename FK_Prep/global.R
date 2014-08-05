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
require(googleVis)
#require(maptools)
require(leaflet)

## Data ----

## Individdata
# individDB <- cdb("Data//DB2014_04")
# save(individDB, file = "Data//Individdata.DB")
load("Data//Individdata.DB")
 
base_data <- tbl_df(data.table(
  individDB['Fodelsear'],
  individDB['Fodelsemanad'],
  individDB['Kon'],
  individDB['Lever', c(2014, 4)],
  individDB['region']
)) %>% 
  mutate(Alder = 2014 - Fodelsear,
         Aldgrp = cut(Alder, seq(0, 120, 5))
  ) %>%
  plyr::rename(c("Lever___2014___4" = "Lever"))

geoTblRegion <- read.csv("Data/coordinate_Region.csv", head = TRUE, sep = ";")


## Fonddata ----
# fondDB <- cdb("Data//FNDDB")
# save(fondDB, file = "Data//Fonddata.DB")
load("Data//Fonddata.DB")

fnd_data <- tbl_df(data.frame(
  FNDID = fondDB['FNDID'][[1]],
  Fondnamn = fondDB['FONDNAMN', c(2014, 4)][[1]],
  Fondtyp = fondDB['FONDTYP', c(2014, 4)][[1]],
  Kategori_bred = fondDB['KATEGORI_BRED', c(2014, 4)][[1]],
  Kategori_smal = fondDB['KATEGORI_SMAL', c(2014, 4)][[1]]
))


## Fond/individdata
load("Data//FondIndivid//MV_2014_4.RData")


base_id <- base_data %>%
  mutate(PEDALID = individDB['PEDALID'][[1]]) %>%
  
  # Join to cross table
  left_join(MV_2014_4, by = "PEDALID") %>%
  filter(!is.na(FNDID)) %>%
  
  # Join to fund table
  left_join(fnd_data, by = "FNDID")


## Functions ---.-
source("functions.R")
source("../R/utils.R")

## > Create variable list for dplyr select() operations ----
varlist = function(x) {
  x = str_c('^(',paste(x, collapse='|'),')$')
  x = str_replace_all(x,'\\.','\\\\.')
  return(x)
}