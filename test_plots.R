
# PlotData
pd <- tbl_df(data.frame(
  individDB['FODAR'],
  individDB['IRR', 2012]
)) %>% rename()

pd <- pd %>% filter(!is.na(IRR))


p <- ggplot(pd, aes(x=IRR)) + geom_density()
p
