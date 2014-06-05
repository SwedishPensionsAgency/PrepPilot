## Headerkommentar

library(data.table)

### Exempel --------------------------------------------------------------------
# 
# CPICol <- "KPI"
# dateCol  <-  'UPDEDT'
# indexCol  <- 'PPINDEX'
# 
# dateStart <- as.Date('2000-12-13')
# dateEnd <- as.Date('2013-12-31')
# 
# # Tabell värden
# yearlyRate(data,dateCol,indexCol,dateStart,dateEnd)
# realYearlyRate(data,dateCol,indexCol, CPICol, dateStart, dateEnd)
# 
# # Index grafer
# index(data,dateCol,indexCol,dateStart, dateEnd)
# realIndex(data, dateCol, indexCol, CPICol, dateStart, dateEnd)
# 
# # Årlig avkastningsgrafer
# indexYearlyRate(data,dateCol,indexCol,dateStart, dateEnd)
# realIndexYearlyRate(data,dateCol,indexCol, CPICol, dateStart, dateEnd)
# 




### Calculates the yearly return -----------------------------------------------
#' Arguments:
#' data = the dataset data.table with date column and index column
#' dateCol = The column name in string format of the date column
#' indexCol = The column name in string format of the index column
#' dateStart = The start date in date format
#' dateEnd = The end value in date format

yearlyRate <- function(data, dateCol, indexCol, dateStart, dateEnd, numberOfDecimals) {
  
  if( missing(numberOfDecimals) ) {numberOfDecimals=2} 
  
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
  
  R <- data.table(YEARLYYIELD = round(r,numberOfDecimals), 
                  TOTALREALYIELD = round(total,numberOfDecimals) )
  return(R)
}


### Calculates the yearly return -----------------------------------------------
#' Arguments:
#' data = the dataset data.table with date column and index column
#' dateCol = The column name in string format of the date column
#' indexCol = The column name in string format of the index column
#' CPICol = The column name of the consumer price index 
#' dateStart = The start date in date format
#' dateEnd = The end value in date format

realYearlyRate <- function(data, dateCol, indexCol, CPICol, dateStart, dateEnd, numberOfDecimals) {
  
  if( missing(numberOfDecimals) ) {numberOfDecimals=2} 
  
  # Extracts the first and the last value of the index
  v1 <- data[data[[dateCol]]==dateStart][,indexCol, with=FALSE][[1]]
  v2 <- data[data[[dateCol]]==dateEnd][,indexCol, with=FALSE][[1]]
  
  cpiv1 <- data[data[[dateCol]]==dateStart][,CPICol, with=FALSE][[1]]
  cpiv2 <- data[data[[dateCol]]==dateEnd][,CPICol, with=FALSE][[1]]
  
  # If period is less than one year return NULL
  if ( as.integer(dateEnd - dateStart) >= 365 ) {
    r <- (v2/v1)^( 365 / ( as.integer(dateEnd-dateStart) ) )
    inflation <- (cpiv2/cpiv1)^( 365 / ( as.integer(dateEnd-dateStart) ) )
    rReal <- r/inflation
    
    r <- (r-1)*100       
    inflation <- (inflation-1)*100  
    rReal <- (rReal-1)*100
  } else { (r <- NULL) }
  
  # The total value change during the period
  totalYield = (v2/v1-1)*100
  totalInflation = (cpiv2/cpiv1-1)*100
  totalRealYield = ((v2/v1)/(cpiv2/cpiv1)-1)*100
  
  # Result 
  R <- data.table(YEARLYYIELD = round(r,numberOfDecimals), 
                  YEARLYREALYIEDL = round(rReal,numberOfDecimals),
                  YEARLYYINFLATION = round(inflation,numberOfDecimals), 
                  TOTALYIELD = round(totalYield,numberOfDecimals),
                  TOTALREALYIELD = round(totalRealYield,numberOfDecimals),
                  TOTALINFLATION = round(totalInflation,numberOfDecimals)
                   )
  return(R)
}




### Returns normalized values given dates --------------------------------------
#' Arguments:
#' data = the dataset
#' dateCol = The column name in string format of the date column
#' indexCol = The column name in string format of the index column
#' dateStart = The start date in date format
#' dateEnd = The end value in date format

index <- function(data, dateCol, indexCol, dateStart, dateEnd, numberOfDecimals) {
  
  if( missing(numberOfDecimals) ) {numberOfDecimals=2} 
  
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
  data2[[CPICol]] <- data2[[CPICol]]/v1CPI*100
  
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





### Returns annual rate for index values --------------------------------------
#' Arguments:
#' data = the dataset in data.table format
#' dateCol = The column name in string format of the date column
#' indexCol = The column name in string format of the index column
#' dateStart = The start date in date format
#' dateEnd = The end value in date format

indexYearlyRate <- function(data, dateCol, indexCol, dateStart, dateEnd) {
  
  # Subsetting data for the given period
  data2 <- data[data[[dateCol]] >= dateStart & data[[dateCol]] <= dateEnd]
  
  # Count the days from first observation
  data2[,DAYS:=as.integer(UPDEDT-dateStart)]
  
  # Extract the first value for the Index
  v1 <- data[data[[dateCol]]==dateStart][,indexCol, with=FALSE][[1]]
  
  # Initiate column to avoid error message
  data2[,YEARLY:=1]
  
  # Normalize the data with the first value
  data2[[indexCol]] <- data2[[indexCol]]/v1
  
  # Yearly rate per day
  data2[,YEARLY:=(data2[[indexCol]]^(365/DAYS)-1)*100]
  data2[,YEARLY:=ifelse(DAYS<365,NA,YEARLY)]
  
  # Subsetting columns to return
  data2 <- data2[,c(dateCol,"YEARLY"),with=FALSE]
  
  return(data2)
}


### Returns annual rate for index values in real and nominal terms -------------
#' Arguments:
#' data = the dataset in data.table format
#' dateCol = The column name in string format of the date column
#' indexCol = The column name in string format of the index column
#' CPICol = The column name of the consumer price index 
#' dateStart = The start date in date format
#' dateEnd = The end value in date format

realIndexYearlyRate <-  function(data, dateCol, indexCol, CPICol, dateStart, dateEnd) {
  
  # Subsetting data for the given period
  data2 <- data[data[[dateCol]] >= dateStart & data[[dateCol]] <= dateEnd]
  
  # Count the days from first observation
  data2[,DAYS:=as.integer(UPDEDT-dateStart)]
  
  # Extract the first value for the Index and CPI
  v1 <- data[data[[dateCol]]==dateStart][,indexCol, with=FALSE][[1]]
  v1CPI <- data[data[[dateCol]]==dateStart][,CPICol, with=FALSE][[1]]
  
  # Initiate column to avoid error message
  data2[,YEARLY:=1]
  data2[,YEARLYREAL:=1]
  data2[,YEARLYINFLATION:=1]
  
  # Normalize the data with the first values
  data2[[indexCol]] <- data2[[indexCol]]/v1
  data2[[CPICol]] <- data2[[CPICol]]/v1CPI
   
  # Rename CPI to inflation
  setnames(data2,CPICol,"INFLATION")
  
  # Create column name for the real valued column
  realCol <- paste0(indexCol,"REAL")
  
  # Adding a column with real values
  data2[[realCol]] <- data2[[indexCol]]/data2[["INFLATION"]]
  
  # Yearly rate per day
  data2[,YEARLY:= (data2[[indexCol]]^(365/DAYS)-1)*100]
  data2[,YEARLY:=ifelse(DAYS<365,NA,YEARLY)]
  
  data2[,YEARLYREAL:= (data2[[realCol]]^(365/DAYS)-1)*100]
  data2[,YEARLYREAL:= ifelse(DAYS<365,NA,YEARLYREAL)]
  
  data2[,YEARLYINFLATION:= (INFLATION^(365/DAYS)-1)*100]
  data2[,YEARLYINFLATION:= ifelse(DAYS<365,NA,YEARLYINFLATION)]
  
  # Subsetting columns to return
  data2 <- data2[,c(dateCol, "YEARLY", "YEARLYREAL", "YEARLYINFLATION"),with=FALSE]
  
  return(data2)
}

