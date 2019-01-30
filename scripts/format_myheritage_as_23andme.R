format_myheritage_as_23andme<-function(path){
  #this is a function to be called whenever a text file with , separation needs to be reformatted to a text file with 4 columns and no header rows. I.e. when reforming from myheritage.com to 23andme format.
  
  if(class(path)!="character")stop(paste("path must be character, not",class(path)))
  if(length(path)!=1)stop(paste("path must be lengh 1, not",length(path)))
  if(!file.exists(path))stop(paste("Did not find file at path:",path))
  
  #logging
  m <- c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"invoking format_myheritage_as_23andme",path)
  m <- paste(m,collapse="\t")
  write(m,file="logs/submission/submission_log.txt",append=TRUE)			
   
  testRead <- read.table(path,nrow=10,stringsAsFactors=F,header=T,sep=",")
  if(ncol(testRead)!=4){stop("testRead of file didn't have 5 columns (as it should have when invoking myheritage conversion)")}
  snpID <- list('rs','i','d')
  if(unique(unique(sub("[0-9]+$","",testRead2[,1]))%!in%snpID))stop(safeError("testRead seemed like myheritage data, but didn't have rs IDs in column 1"))
  
  #inserting # at first rsid palce
  cmd1 <- paste("sed 's/^RSID/#RSID/' ",path," > tempOut0.txt",sep="")
  system(cmd1)
  
  
  #inserting # at first rsid palce
  cmd1 <- paste("sed -e 's/\\,/\\t/g' -e 's/\"//g' tempOut0.txt > tempOut1.txt",sep="")
  system(cmd1)
  
  #retain only non-commented lines
  cmd2 <- paste("awk 'NF && $1!~/^#/' tempOut1.txt > tempOut2.txt",sep="")
  system(cmd2)
  
  #Saving header and insert the right number of commented out lines (20)
  linestoinsert <- 20
  cmd4 <- paste("sed  -n '/^\\s*#/!{=;q}' tempOut0.txt",sep="")
  commentLineCount <- as.numeric(system(cmd4,intern=T))-1
  header <- readLines("tempOut0.txt",n=commentLineCount)
  
  f <- file("spacer","w")
  headerFull <- c(rep("#",linestoinsert-length(header)),header)
  writeLines(paste(headerFull,collapse="\n"),f)
  close(f)
  cmd5 <- paste("cat spacer tempOut2.txt > tempOut3.txt")
  system(cmd5)
  
  file.rename("tempOut3.txt",path)
  
  unlink("spacer")
  unlink("tempOut1.txt")
  unlink("tempOut0.txt")
  unlink("tempOut2.txt")
}

