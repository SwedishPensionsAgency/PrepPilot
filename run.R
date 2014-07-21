#' To run app: Run the code in this file. These are the steps:
#' 
#' 1. Load libraries
#' 2. Load functions in R/utils.R
#' 3. Set WD to either "Prep" or "FK_Prep"
#' 4. Load data
#' 5. Use the Run() command defined in Rutils.R to load the app
#' 


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

## Include functions to run applications
source("R/utils.R")

## Set options
options("shiny.launch.browser" = TRUE)
options(shiny.reactlog = TRUE)

## Set Wd -- CHANGE THIS TO CHANGE APP
setwd("FK_Prep")

## Load data
source("munge.R")

## Run PreP app
Run()
