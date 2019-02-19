args = commandArgs(trailingOnly=TRUE)
source('scripts/prepare_23andme_genome.R')

download_raw_file<-function(url, upload_id) {
  setwd('.')
  #Filename will be upload_x.extension
  extension <- tools::file_ext(sub("\\?.+", "", url))
  file_name <- paste('upload_', upload_id, '.', extension, sep="")
  file_path <- paste('raw_files/', file_name, sep="")
  print('Downloading file')
  download.file(url, file_path, quiet=FALSE, cacheOK=TRUE, headers=NULL)
  print('Download finished')
  prepare_23andme_genome(file_path, file_name)
  print('Preparing file format')
}

if (length(args)<2) {
  stop("At least two arguments must be supplied (full url to raw DNA file and the API upload id ).n", call.=FALSE)
} else {
  url <- args[1]
  upload_id <- args[2]
  download_raw_file(url, upload_id)
}
