#!/bin/bash
# STEP 1
# Run get raw DNA & format for imputation
# run this for every user upon file upload
# ./step1.sh remote_url upload_id

file_location=$1
upload_id=$2

echo "File location: "$file_location
echo "Upload ID: "$upload_id
Rscript scripts/download_raw_file.R file_location upload_id > logs/warnings.logs
