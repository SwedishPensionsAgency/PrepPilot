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

# data <- mfdata; byvar = "kon"; grpvar = NULL
# data <- mfdata; byvar = "kon"; grpvar = "utb"
data <- mfdata; byvar = c("kon", "aldgrp"); grpvar = "utb"
# data <- mfdata; byvar = c("kon", "aldgrp"); grpvar = NULL


## Create benchmarks
require(lineprof)

source("R/utils.R")

bm <- lineprof(partTable(data = mfdata, byvar = byvar, grpvar = grpvar))
shine(bm)
