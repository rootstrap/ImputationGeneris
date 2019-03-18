args = commandArgs(trailingOnly=TRUE)
source('scripts/download_remote_file.R')

if (length(args)<2) {
  stop("At least two arguments must be supplied (full url to raw DNA file and the API upload id ).n", call.=FALSE)
} else {
  url <- args[1]
  upload_id <- args[2]
  download_remote_file(url, upload_id)
}
