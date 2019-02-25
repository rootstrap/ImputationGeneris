# plumber.R

#* Starts a new imputation via step1.sh script
#* @param upload_id The upload identification in the API
#* @param file_location The remote URL of the file to impute
#* @post /new_raw_file
function(res, upload_id, file_location) {
  setwd('.')
  if (missing(upload_id) || missing(file_location) || upload_id == '' || file_location == '') {
    res$status <- 400
    return(list(error='Your request did not include a required parameter.'))
  }

  safe_location <- paste('"', file_location, '"', sep='')
  start_command <- paste('./step1.sh', safe_location, upload_id, '&')
  print(start_command)
  system(start_command)
}
