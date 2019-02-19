# plumber.R

#* Starts a new imputation via step1.sh script
#* @param upload_id The upload identification in the API
#* @param file_location The remote URL of the file to impute
#* @post /new_raw_file
function(upload_id, file_location) {
  setwd('.')
  safe_location <- paste('"', file_location, '"', sep="")
  start_command <- paste('./step1.sh', safe_location, upload_id, '&')
  print(start_command)
  system(start_command)
}
