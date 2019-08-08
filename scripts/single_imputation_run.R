single_imputation_run <- function(uniqueID, rawdata) {
  #if running as node, we also create the output dir already here
  if(serverRole== "Node"){
    dir.create(paste("data/",uniqueID,sep=""))
  }

  notify_imputation_status(uniqueID, status = imputationStatus$processing)

  #run the imputation
  run_imputation(uniqueID=uniqueID, rawdata=rawdata)

  #summarizing files
  destination <- paste0(homePath, "data")
  dir.create(destination)
  output_files <- summarize_imputation(uniqueID=uniqueID,destinationDir=destination)

  #Storing relevant output to S3
  remote_file_location <- upload_output_to_S3(upload_id = uniqueID, local_file_path = output_files[1])
  if (!is.null(remote_file_location)) {
    full_url <- paste0("https://s3.", region, ".amazonaws.com/", bucketName, remote_file_location)
    notify_finished_imputation(upload_id = uniqueID, output_url = full_url)
  } else {
    exit_on_fail(paste("Something went wrong uploading to S3:", uniqueID))
  }

  #If this is running as a node, we need to copy it back around here
  if(serverRole== "Node"){
    cmd5 <- paste("scp -r data/",uniqueID, admin,"@",hubAddress,":data",sep="")
    system(cmd5)
  }

  doneProcessFolder <- paste0(homePath, "done/")
  dir.create(doneProcessFolder)
  #making a link out to where the data can be retrieved	(different on hub and node)
  if(serverRole== "Node"){
    cmd6 <- paste("ssh ", admin,"@",hubAddress," 'ln -s data/",uniqueID,"/",uniqueID,".simple_format.zip ", doneProcessFolder, uniqueID,".simple_format.zip'",sep="")
    system(cmd6)
    
    cmd7 <- paste("ssh ", admin,"@",hubAddress," 'ln -s data/",uniqueID,"/",uniqueID,".gen.zip", doneProcessFolder, uniqueID,".gen.zip'",sep="")
    system(cmd7)
    
    # cmd8 <- paste("ssh ", admin,"@",hubAddress," 'ln -s data/",uniqueID,"/",uniqueID,"_data.json done/",uniqueID,"_data.json'",sep="")
    # system(cmd8)
  }else if(serverRole== "Hub"){
    file.symlink(
      from = paste(destination, "/", uniqueID,"/",uniqueID,".simple_format.zip",sep=""),
      to = paste(doneProcessFolder, uniqueID,".simple_format.zip",sep="")
    )
    file.symlink(
      from = paste(destination, "/", uniqueID,"/",uniqueID,".gen.zip",sep=""),
      to = paste(doneProcessFolder, uniqueID,".gen.zip",sep="")
    )
    # file.symlink(
    #   from = paste("data/",uniqueID,"/",uniqueID,"_data.json",sep=""),
    #   to = paste("done/",uniqueID,"_data.json",sep="")
    # )
  }else{stop("Invalid Server role.")}

  setwd(homePath)
  unlink(runDir,recursive=TRUE)
  unlink(paste0("raw_files/upload_", uniqueID, ".*"))

  #also clear the hub imputation_folder if running as node
  if(serverRole== "Node"){
    cmd9 <- paste("ssh ", admin,"@",hubAddress," 'rm -r imputations/imputation_folder_",uniqueID,"'",sep="")
    system(cmd9)
    
    #also don't leave the finished data here
    unlink(paste("data/",uniqueID,sep=""),recursive=TRUE)
  }
}
