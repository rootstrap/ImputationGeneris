make_overview_of_samples <- function(verbose=T){
  uniqueIDs <- list.files("data")
  all_pData <- list()
  for(uniqueID in uniqueIDs){
    pDataFile <- paste("data/",uniqueID,"/pData.txt",sep="")
    if(file.exists(pDataFile)){
      all_pData[[uniqueID]] <- try(read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t"))
    }else{
      if(verbose)print(paste("Didn't find a pData file for",uniqueID))	
    }
  }
  all_columns <- unique(unlist(lapply(all_pData,colnames)))
  pData <- as.data.frame(matrix(nrow=0,ncol=length(all_columns),dimnames=list(NULL,all_columns)))
  for(uniqueID in names(all_pData)){
    p < -all_pData[[uniqueID]]
    for(missing_col in all_columns[!all_columns%in%colnames(p)]){
      p[1,missing_col]<-NA
    }
    pData<-rbind(pData,p[,all_columns])
  }
  rownames(pData) <- pData[,"uniqueID"]
  return(pData)
}

