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
#' A flexible function to create a summary table which can be horizontally divided into subgroups
#' 
#' @param byvar A character vector of variable names by which to divide the data into subgroups
#' @param grpvar The 
#' @export

partTable <- function(data, byvar = NULL, grpvar = NULL, freqvar = NULL, sumname = "Total", sumrow = c("top", "bottom")) {
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
  
  # Sum row
  if (!is.null(sumrow)) {
    sums <- list()
    sums[[grpvar]] <- sumname
    sums <- append(sums, colSums(maindata %>% select(2:ncol(maindata))))
        
    levels(maindata[[grpvar]]) <- c(levels(maindata[[grpvar]]), sumname)
    
    if (sumrow[[1]] == "bottom") {
      maindata <- rbind(maindata, sums)
    } else if (sumrow[[1]] == "top") {
      maindata <- rbind(sums, maindata)
    }
  }
  
  # Rename totals column and return
  setnames(maindata, "tot", sumname)
  return(maindata)
}



#' Set a default value for an object
#'
#' This function sets the value of an object to a default value if it is not defined.
#' @params x object
#' @params y object
#' @export
`%||%` <- function(x, y){
  if (is.null(x)) y else x
}