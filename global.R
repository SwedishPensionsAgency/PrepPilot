## REQUIREMENTS FOR THE PrepPilot SHINY APP ##

## Libraries ----
require(shiny) # >= 0.9
require(Coldbir) # >= 0.5
require(data.table) # >= 1.9.2¨


## Data ----
prepDB <- cdb("Data//DataBas")

system.time(base_data <- data.table(
  FODAR = prepDB['FODAR'],
  FONDPLACERINGSAR = prepDB['FONDPLACERINGSAR'],
  INTJANANDEAR = prepDB['INTJANANDEAR'],
  INTRADESAR = prepDB['INTRADESAR'],
  SEX = prepDB['SEX']  
))