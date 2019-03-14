source('misc_files/config.r')

# Call this function after imputation is ready with the location 
# of the output file in the output_url param.
notify_api_after_imputation<-function(upload_id, output_url) {
  library(httr)

  url <- paste(apiServer, "uploads/", upload_id, "/imputation_ready", sep="")
  params <- list(imputation_output_url = output_url)

  response <- POST(url, body = params, encode = "json")
  stop_for_status(response, task = paste("Notify API about imputation ready for upload:", upload_id))
}
