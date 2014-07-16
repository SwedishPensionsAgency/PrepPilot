#' Run a shiny app without changing the base directory
#' 
#' @export

Run <- function(app = NULL) {
  app_path <- file.path(getwd(), app %||% "")
  old <- setwd(app_path)
  on.exit(setwd(old))
  
  runApp(".")
}


#' Create a partitioned frequency table from panel data
#' 
#' @export

partTable <- function(data, byvar = NULL, grpvar = NULL, freqvar = NULL, sumname = "total") {
  # Start with totals column
  maindata <- data %>%
    regroup(as.list(grpvar)) %>%
    summarise("tot" = n())
  
  # Add columns for each of the by values in each of the by variables
  for (var in byvar) {
    if (!is.null(grpvar)) {
      freqtbl <- data.frame(data) %>%
        tbl_df() %>%
        regroup(as.list(c(var, unlist(grpvar)))) %>%
        summarise(freq = n()) %>%
        spread_(key_col = var, "freq", fill = 0)
      
      maindata <- maindata %>% 
        left_join(freqtbl, by = grpvar)
    } else {
      # If no 'grpvar' is supplied, we want end up with a one-row data frame
      freqvec <- data %>%
        tbl_df() %>%
        regroup(as.list(var)) %>%
        summarise(freq = n()) %>%
        t() %>%
        as.data.frame(., stringsAsFactors = FALSE)
      
      setnames(freqvec, names(freqvec), as.character(freqvec[1,]))
      freqvec <- freqvec[2,] %>% mutate_each(., funs(as.integer))
      maindata <- cbind(maindata, freqvec)
    }
  }
  
  # Rename totals column and return
  setnames(maindata, "tot", sumname)
  return(maindata)
}

