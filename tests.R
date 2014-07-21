
# PlotData
pd <- tbl_df(data.frame(
  individDB['FODAR'],
  individDB['IRR', 2012]
)) %>% rename()

pd <- pd %>% filter(!is.na(IRR))


p <- ggplot(pd, aes(x=IRR)) + geom_density()
p




### Tester av funktionerna i functions.R ---------------------------------------

CPICol <- "KPI"
dateCol  <-  'UPDEDT'
indexCol  <- 'PPINDEX'

dateStart <- as.Date('2000-12-13')
dateEnd <- as.Date('2013-12-31')

# Tabell värden
yearlyRate(ppindex,dateCol,indexCol,dateStart,dateEnd)
realYearlyRate(ppindex,dateCol,indexCol, CPICol, dateStart, dateEnd)

# Index grafer
index(ppindex,dateCol,indexCol,dateStart, dateEnd)
realIndex(ppindex, dateCol, indexCol, CPICol, dateStart, dateEnd)

# Årlig avkastningsgrafer
indexYearlyRate(ppindex,dateCol,indexCol,dateStart, dateEnd)
realIndexYearlyRate(ppindex,dateCol,indexCol, CPICol, dateStart, dateEnd)





