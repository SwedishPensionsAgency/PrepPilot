## REQUIREMENTS FOR THE PrepPilot SHINY APP ##

## Libraries ----
require(shiny) # >= 0.9
require(Coldbir) # >= 0.5
require(data.table) # >= 1.9.2

## Data ----
prepDB <- cdb("Data//DataBas")
