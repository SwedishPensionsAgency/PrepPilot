PrepPilot
=========

A project to create a prototype/mockup revamp of several statistical reports at Pensionsmyndigheten

### Requirements

For a list of the required packages, see `Prep/global.R`.
Most of the project requirements are on CRAN. However, you might also want to download and install the following packages from Github:

```r
devtools::install_github("ramnathv/rCharts")
devtools::install_github("hadley/tidyr")
devtools::install_github("metagraf/sparkle")
devtools::install_github("SwedishPensionsAgency/Coldbir")
```

### Running the apps
Run the apps included using the following code:

```r
## PreP-Pilot
shiny::runApp("Prep") # This will change your working directory for now

```