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


## Run PreP app
Run("FK_Prep")
