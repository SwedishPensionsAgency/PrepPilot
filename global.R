## Libraries ----

library(shiny)


## Runtime vars ----
debug_output <- TRUE

## Source UI elements
pages <- list()
files <- list.files("pages/", full.names = TRUE, recursive = TRUE)
do.call(source, as.list(files))
