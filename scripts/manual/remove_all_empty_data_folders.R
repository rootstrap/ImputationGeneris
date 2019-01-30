#A function that will crawl all data directories and remove any that are empty. 
#These can happen on submission errors. Best to just execute manually
remove_all_empty_data_folders <- function(uniqueIDs=NULL){ 
  if(is.null(uniqueIDs)){
    uniqueIDs <- list.files("data")
  }else{
    if(class(uniqueIDs)!="character")stop("UniqueIDs must be of class character")
    if(!all(file.exists(paste("data/",uniqueIDs,sep=""))))stop("Not all UniqueIDs given were found")
  }
  
  for(uniqueID in uniqueIDs){
    dataFolder <- paste("data/",uniqueID,sep="")
    filesInside <- list.files(dataFolder)
    if(length(filesInside) == 0){
      print(paste("Deleting", dataFolder,"because it was empty"))
      unlink(dataFolder, recursive=T)
    }
  }
}