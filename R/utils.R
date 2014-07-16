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
  freqtbl <- data.frame(data) %>%
    tbl_df() %>%
    regroup(as.list(c(unlist(byvar), unlist(grpvar)))) %>%
    summarise(freq = n()) %>%
    spread_(key_col = byvar, "freq", fill = 0)
  
  # If no "grpvar" is supplied, i.e. if the return data should have only one row,
  # then partTable returns a table that must be flattened before being returned.
  if (is.null(grpvar)) {
    freqtbl <- colwise(sum, na.rm = TRUE)(freqtbl)
    freqtbl$tot <- sum(freqtbl)
    
  } else {
    freqtbl <- data %>%
      regroup(as.list(grpvar)) %>%
      summarise("tot" = n()) %>%
      left_join(freqtbl, by = grpvar)
  }
  
  setnames(freqtbl, "tot", sumname)
  
  return(freqtbl)
}



mfdata <- tbl_df(data.frame(
  kon = letters[sample(c(6, 13, 15), 50, replace = TRUE)],
  someval = runif(50),
  utb = sample(1:2, 50, replace = TRUE)
))