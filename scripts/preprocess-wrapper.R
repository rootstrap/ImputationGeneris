#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
source('scripts/prepare_23andme_genome.R')

# test if there is at least one argument: if not, return an error
if (length(args)<2) {
  stop("At least two arguments must be supplied (full path to raw DNA file and DNA filename).n", call.=FALSE)
} else if (length(args)==2) {
  path <- args[1]
  filename <- args[2]
  print(path) 
  print(filename)
  prepare_23andme_genome(path, filename)
} else if (length(args)>2) {
  path <- args[1]
  filname <- args[2]
  wd <- args[3]
  prepare_23andme_genome(path, filename, wd)
}



