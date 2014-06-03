library(data.table)

CPICol <- "KPI"
dateCol  <-  'UPDEDT'
indexCol  <- 'PPINDEX'

dateStart <- as.Date('2005-12-13')
dateEnd <- as.Date('2013-12-31')

realIndex(data, dateCol, indexCol, CPICol, dateStart, dateEnd)
yearlyRate(data,dateCol,indexCol,dateStart,dateEnd)
index(data,dateCol,indexCol,dateStart, dateEnd)

### Calculates the yearly return -----------------------------------------------
#' Arguments:
#' data = the dataset data.table with date column and index column
#' dateCol = The column name in string format of the date column
#' indexCol = The column name in string format of the index column
#' dateStart = The start date in date format
#' dateEnd = The end value in date format

yearlyRate <- function(data, dateCol, indexCol, dateStart, dateEnd) {
  
  # Extracts the first and the last value of the index
  v1 <- data[data[[dateCol]]==dateStart][,indexCol, with=FALSE][[1]]
  v2 <- data[data[[dateCol]]==dateEnd][,indexCol, with=FALSE][[1]]
  
  # If period is less than one year return NULL
  if ( as.integer(dateEnd - dateStart) >= 365 ) {
    r <- (v2/v1)^( 365 / ( as.integer(dateEnd-dateStart) ) )
    r <- (r-1)*100                
  } else { (r <- NULL) }
  
  # The total value change during the period
  total = (v2/v1-1)*100
  
  R <- data.table(YEARLY=r, TOTAL=total)
  return(R)
}


### Returns normalized values given dates --------------------------------------
#' Arguments:
#' data = the dataset
#' dateCol = The column name in string format of the date column
#' indexCol = The column name in string format of the index column
#' dateStart = The start date in date format
#' dateEnd = The end value in date format

index <- function(data, dateCol, indexCol, dateStart, dateEnd) {
  
  # Subsetting data for the given period
  data2 <- data[data[[dateCol]] >= dateStart & data[[dateCol]] <= dateEnd]
  
  # Extract the first value for the Index
  v1 <- data[data[[dateCol]]==dateStart][,indexCol, with=FALSE][[1]]
  
  # Normalize the data with the first value
  data2[[indexCol]] <- data2[[indexCol]]/v1*100
  
  # Subsetting columns to return
  data2 <- data2[,c(dateCol,indexCol),with=FALSE]
  
  return(data2)
}


### Convert nominal values to real for data.table ------------------------------
#' Arguments:
#' data = the dataset
#' dateCol = The column name in string format of the date column
#' indexCol = The column name in string format of the index column
#' CPICol = The column name of the consumer price index 
#' dateStart = The start date in date format
#' dateEnd = The end value in date format
  
realIndex <-  function(data, dateCol, indexCol, CPICol, dateStart, dateEnd) {
  
  # Subsetting data for the given period
  data2 <- data[data[[dateCol]] >= dateStart & data[[dateCol]] <= dateEnd]
  
  # Extract the first value for the Index and CPI
  v1 <- data[data[[dateCol]]==dateStart][,indexCol, with=FALSE][[1]]
  v1CPI <- data[data[[dateCol]]==dateStart][,CPICol, with=FALSE][[1]]
  
  # Normalize the data with the first values
  data2[[indexCol]] <- data2[[indexCol]]/v1*100
  data2[[CPICol]] <- data2[[CPICol]]/v1KPI*100
  
  # Rename CPI to inflation
  setnames(data2,CPICol,"INFLATION")
  
  # Create column name for the real valued column
  realCol <- paste0(indexCol,"REAL")
  
  # Adding a column with real values
  data2[[realCol]] <- data2[[indexCol]]/data2[["INFLATION"]]*100
  
  # Subsetting columns to return
  data2 <- data2[,c(dateCol, indexCol, "INFLATION", realCol),with=FALSE]
  
  return(data2)
}



