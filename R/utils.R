#' Run a shiny app without changing the base directory
#' 
#' @export

Run <- function(app) {
  app_path <- file.path(getwd(), app)
  old <- setwd(app_path)
  on.exit(setwd(old))
  
  runApp(".")
}