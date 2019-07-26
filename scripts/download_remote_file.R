source('scripts/prepare_23andme_genome.R')
source('misc_files/config.r')
source("scripts/custom_functions.R")
source("scripts/email_sender.R")

download_remote_file<-function(url, upload_id) {
  setwd('.')
  #Filename will be upload_x.extension
  extension <- tools::file_ext(sub("\\?.+", "", url))
  file_name <- paste('upload_', upload_id, '.', extension, sep="")
  file_path <- paste('raw_files/', file_name, sep="")
  tryCatch({
    print('Downloading file')
    download.file(url, file_path, quiet=FALSE, cacheOK=TRUE, headers=NULL)
    print('Download finished')
    print('Preparing file format')
    prepare_23andme_genome(file_path, file_name, upload_id)
  }, error = function(error_message) {
    message <- paste("File format preparation failed for:", file_name, "The error was:", error_message)
    write_logs(LOGS, message)
    send_email(message)
  })
}
