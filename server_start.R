#!/usr/bin/env Rscript
# Starts Plumber server

setwd('.')
library(plumber)
r <- plumb('plumber.R')
r$run(host = '0.0.0.0', port = 8000)
