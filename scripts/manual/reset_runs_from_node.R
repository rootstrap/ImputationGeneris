reset_runs_from_node<-function(uniqueIDs, check_is_running=T){
  #function to reset a bulk-run from Node. Useful if there was a crash and we need to re-run
  
  if(class(uniqueIDs)!="character")stop("uniqueIDs must be a character")
  if(!all(nchar(uniqueIDs)==12))stop("uniqueIDs must be of length 12")
  if(length(grep("^id_",uniqueIDs)) != length(uniqueIDs))stop("all uniqueIDs must start with id_")
  
  if(check_is_running){
    for(uniqueID in uniqueIDs){
      cmd1 <- paste0("ssh  ",admin,"@",hubAddress," 'cat imputations/imputation_folder_",uniqueID,"/job_status.txt'")
      status <- system(cmd1,intern=T)
      if(status!="Job is remote-running")stop(paste("Status for",uniqueID,"was not remote-running. This must be the case for a reset. Aborting with no change. Status was",status))
    }
  }
  
  
  imp_to_delete <- list.files("imputations/")
  if(!all(uniqueIDs %in% sub("imputation_folder_","",imp_to_delete))){
    missing <- uniqueIDs[!uniqueIDs %in% sub("imputation_folder_","",imp_to_delete)]
    uniqueIDs <- uniqueIDs[uniqueIDs %in% sub("imputation_folder_","",imp_to_delete)]
    print(paste("These",length(missing),"uniqueIDs were not found in local imputation folder. They will be ignored also when resetting hub:",paste(missing,collapse=",")))
  }
  
  if(length(imp_to_delete)>length(uniqueIDs)){
    print(paste("Note that there was",length(imp_to_delete),"folders in imputations, but only a request for deleting",length(uniqueIDs),"uniqueIDs. The additional will be deleted nonetheless"))
  }else{
    print(paste("Deleting",length(uniqueIDs),"uniqueIDs from local imputation folder."))
  }
  unlink(paste0("imputations",imp_to_delete),recursive=T)  
  
  bulk_to_delete <- list.files("bulk_imputations/")
  if(length(bulk_to_delete)==1){
    print("Also deleting one folder in .bulk_imputations")
  }else{
    print(paste("Deleting",length(bulk_to_delete),"folders in bulk_imputations:",paste(bulk_to_delete,collapse=", ")))
  }
  unlink(paste0("bulk_imputations",bulk_to_delete),recursive=T)  
  
  print(paste("Setting Job ready tag for",length(uniqueIDs),"uniqueIDs on hub at:",hubAddress))
  for(uniqueID in uniqueIDs){
    cmd2 <- paste0("ssh ",admin,"@",hubAddress," 'echo Job is ready > imputations/imputation_folder_",uniqueID,"/job_status.txt'")
    system(cmd2)
  }
  
}
