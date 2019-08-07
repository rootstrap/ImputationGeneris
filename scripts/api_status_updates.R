source('misc_files/config.r')

# Call this function after imputation is ready with the location 
# of the output file in the output_url param.
notify_api_of_status_change <- function(upload_id, status_params) {
  library(httr)

  url <- paste(apiServer, "uploads/", upload_id, "/imputation_status", sep="")
  params <- list(upload = status_params)

  response <- PUT(url, body = params, encode = "json")
  stop_for_status(response, task = paste("Notify API about imputation status change for upload:", upload_id))
}

notify_imputation_status <- function(upload_id, status) {
  notify_api_of_status_change(upload_id, list(imputation_status = status))
}

notify_finished_imputation <- function(upload_id, output_url) {
  params <- list(imputation_status = imputationStatus$finished,
                 imputed_dna = output_url)
  notify_api_of_status_change(upload_id, params)
}

notify_failed_imputation <- function(upload_id, error_message) {
  params <- list(imputation_status = imputationStatus$failed, 
                 last_error = error_message)
  notify_api_of_status_change(upload_id, params)
}
