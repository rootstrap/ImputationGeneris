source('misc_files/config.r')
source('scripts/api_status_updates.R')
source('scripts/custom_functions.R')

upload_output_to_S3 <- function (upload_id, local_file_path) {
  library("aws.s3")
  
  if (upload_id == "" || local_file_path == "" || bucketName == "" ||
      length(upload_id) != 1 || length(local_file_path) != 1 || length(bucketName) != 1) {
    exit_on_fail("Upload to S3 failed. Missing parameter.")
  }
  if (!file.exists(local_file_path)) {
    exit_on_fail(paste("Did not find file at path:", local_file_path))
  }
  file_name = basename(local_file_path)
  upload_location <- paste(remoteOutputFolder, upload_id, "/", file_name, sep = "")
  result <- put_object(file = local_file_path, object = upload_location, bucket = bucketName, show_progress = TRUE, verbose = TRUE)
  if (result) {
    upload_location
  } else {
    NULL
  }
}
