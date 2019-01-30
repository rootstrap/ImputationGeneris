re_check_md5sums <- function(){
  library(tools)
  all_md5sums <- read.table("misc_files/md5sums.txt",sep="\t",stringsAsFactors = F)[,1]
  
  otherPersons <- list.files("data",full.names=T)
  for(otherPerson in otherPersons){
    if(!file.info(otherPerson)[["isdir"]])next
    if(!file.exists(paste(otherPerson,"pData.txt",sep="/")))next
    other_person_md5sum <- try(read.table(paste(otherPerson,"pData.txt",sep="/"),sep="\t",header=T,stringsAsFactors=F,comment.char="",quote="")[1,"md5sum"],silent=T)
    if(class(other_person_md5sum)=="try-error")next
    if(is.null(other_person_md5sum))next
    
    all_md5sums <- c(all_md5sums,other_person_md5sum)
  }
  #checking if this job is not already in queue
  for(otherPerson in paste0(list.files('imputations',full.names=T),"/")){
    if(!file.info(otherPerson)[["isdir"]])next
    
    raw_data_file <- grep("raw_data\\.txt", list.files(otherPerson,full.names=T), value=T)
    if(length(raw_data_file)!=1)stop("odd")
    other_person_md5sum <- md5sum(raw_data_file)
    all_md5sums <- c(all_md5sums, other_person_md5sum)
    
  }
  print(paste(sum(duplicated(all_md5sums)), "of", length(all_md5sums), "were duplicated"))
  all_md5sums <- unique(all_md5sums)
  writeLines(all_md5sums, "misc_files/md5sums.txt")
}