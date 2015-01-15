require(plyr)
require(dplyr)
require(tidyr)

samplesize <- 500

mfdata <- tbl_df(data.frame(
  kon = letters[sample(c(6, 13), samplesize, replace = TRUE)],
  someval = runif(samplesize),
  utb = sample(1:2, samplesize, replace = TRUE),
  aldgrp = LETTERS[sample(c(1:3), samplesize, replace = TRUE)]
))

data <- copy(mfdata)

byvar = "kon"; grpvar = NULL
byvar = "kon"; grpvar = "utb"
byvar = c("kon", "aldgrp"); grpvar = "utb"; funvar = NULL; .fun = NULL
byvar = c("kon", "aldgrp"); grpvar = NULL; funvar = NULL; .fun = NULL
byvar = c("kon", "aldgrp"); grpvar = "utb"; funvar = "someval"; .fun = mean


## Create benchmarks
require(lineprof)

source("R/utils.R")


bm <- lineprof(
  partTable(data = data, byvar = byvar, grpvar = grpvar, funvar = funvar, .fun = .fun)
)
shine(bm)
