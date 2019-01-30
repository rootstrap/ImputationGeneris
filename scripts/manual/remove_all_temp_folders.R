  #A function that will crawl all data directories and remove any lingering temp folders - only use with manual execution
remove_all_temp_folders <- function(uniqueIDs=NULL){  
  if(is.null(uniqueIDs)){
    uniqueIDs <- list.files("data")
  }else{
    if(class(uniqueIDs)!="character")stop("UniqueIDs must be of class character")
    if(!all(file.exists(paste("data/",uniqueIDs,sep=""))))stop("Not all UniqueIDs given were found")
  }
  
  
  for(uniqueID in uniqueIDs){
    tempFolder <- paste("data/", uniqueID, "/temp", sep="")
    if(file.exists(tempFolder)){
      print(paste("Deleting", tempFolder))
      unlink(tempFolder, recursive=T)
    }
  }
}