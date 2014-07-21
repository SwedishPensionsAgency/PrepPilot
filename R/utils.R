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
#' @param grpvar The variable by which to create horizontal groups
#' @param .fun An unquoted function to apply groupwise to variable \code{funvar}, i.e. \code{sum} or \code{mean}. If supplied, the resulting table will contain the results of \code{.fun} instead of frequencies.
#' @param funvar The column on which to apply \code{.fun}. If none is submitted, partTable chooses the leftmost column that is not in either \code{byvar} or \code{grpvar} to be the one counted. Only relevant if \code{.fun} is submitted.
#' @export

partTable <- function(
  data,
  byvar = NULL,
  grpvar = NULL,
  sumname = "Total",
  sumrow = c("top", "bottom"),
  .fun = NULL,
  funvar = NULL
) {
  # Error handling
  if (is.null(.fun) & !is.null(funvar)) {
    stop("Argument .fun must be submitted along with argument funvar!")
  }
  
  # If no funvar was assigned, give it the contents of any other variable that's not in grpvar or byvar
  funvar <- funvar %||% names(data)[!names(data) %in% c(byvar, grpvar)][[1]]
  # If no function was submitted, use length to calculate frequency counts
  .fun <- .fun %||% length
  
  # Start with totals column
  names(data)[names(data) == funvar] <- 'funvar'
  maindata <- data %>%
    regroup(as.list(grpvar)) %>%
    summarise("tot" = .fun(funvar))
  
  
  # Add columns for each of the by values in each of the by variables
  for (var in byvar) {
    if (!is.null(grpvar)) {
      freqtbl <- data.frame(data) %>%
        tbl_df() %>%
        regroup(as.list(c(var, unlist(grpvar)))) %>%
        summarise(freq = .fun(funvar)) %>%
        spread_(key_col = var, "freq", fill = 0)
      
      maindata <- maindata %>% 
        left_join(freqtbl, by = grpvar)
    } else {
      # If no 'grpvar' is supplied, we want end up with a one-row data frame
      freqvec <- data %>%
        tbl_df() %>%
        regroup(as.list(var)) %>%
        summarise(freq = .fun(funvar)) %>%
        t() %>%
        as.data.frame(., stringsAsFactors = FALSE)
      
      setnames(freqvec, names(freqvec), as.character(freqvec[1,]))
      freqvec <- freqvec[2,] %>% mutate_each(., funs(as.integer))
      maindata <- cbind(maindata, freqvec)
    }
  }
  
  # Sum row
  if (!is.null(sumrow) & !is.null(grpvar)) {
    sums <- list()
    sums[[grpvar]] <- sumname
    sums <- append(sums, colSums(maindata %>% select(2:ncol(maindata))))
    
    if (is.factor(maindata[[grpvar]])) {
      levels(maindata[[grpvar]]) <- c(levels(maindata[[grpvar]]), sumname)
    }
    
    if (sumrow[[1]] == "bottom") {
      maindata <- rbind(maindata, sums)
    } else if (sumrow[[1]] == "top") {
      maindata <- rbind(sums, maindata)
    }
  }
  
  # Restore names in data set (for some weird reason this seems to actually
  # change the global object and not just the in-function object!)
  names(data)[names(data) == 'funvar'] <- funvar
  
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