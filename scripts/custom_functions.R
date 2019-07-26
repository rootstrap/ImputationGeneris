source('scripts/format_ancestry_com_as_23andme.R')
source('scripts/format_myheritage_as_23andme.R')
# 1. Create uniqueID
create_uniqueID <- function() 
{
  uniqueID <- paste("id_",sample(1000:9000,1),sample(10000:90000,1),sep="")
  numberOfLetters <- sample(c(1,1,2,3),1)
  if(numberOfLetters > 0){
    positionsToInsertLetter <- sample(5:(nchar(uniqueID)-1),numberOfLetters)
    
    l<-c(LETTERS,letters)
    l<-l[!l%in%c("o","O")] #I hate it when O is in
    for(x in positionsToInsertLetter){
      substr(uniqueID,x,x) <- sample(l,1)
    }
  }
  if(uniqueID%in%list.files("data/"))
  {
    write_log(LOGS, path, uniqueID, homeDir, 'double_id')
    stop("Problem with unique ID generation. Please re-load and try again.")
  } else { return(uniqueID)}
}

# 2. Write errors to log file
write_log <- function(LOGS, path, uniqueID, homeDir, short_err)
{
  message <- paste(short_err, uniqueID)
  write_logs(LOGS, message)
  unlink(homeDir,recursive=T)
}

# 3. Check if file is compressed, if so unzip
check_zipped <- function(path, newTempPath, newUnzippedPath, homeFolder, LOGS)
{
  file.copy(path, newTempPath)	
  gunzipResults <- suppressWarnings(unzip(newTempPath,exdir=homeFolder))
  if(length(gunzipResults)==1){ #Its zipped
    file.rename(gunzipResults, newUnzippedPath)		
  } else{ #then it's probably not, check if it is a gz file
    filetype <- system(paste("file ", newTempPath),intern=T)
    if(length(grep("gzip compressed",filetype))==1){
      write_log(LOGS, path, uniqueID, homeFolder, 'gzip_file')
      stop("Don't submit gz-files. Only uncompressed text or zip-files. If you already know what a gz file is, this should be easy for you. Please format as tab separated text files.")
    } else{
      #otherwise just rename
      file.rename(newTempPath, newUnzippedPath)		
    }
  }
  return()
}
'%!in%' <- function(x,y)!('%in%'(x,y))

# 4. Test format of sample read
test_read <- function(path, uniqueID, LOGS, homeFolder)
{ 
  testRead <- try(read.table(path,nrow=10,stringsAsFactors=F))
  if(ncol(testRead)==5) {
    #This could be an ancestry.com file. Check that first
    testRead2 <- read.table(path,nrow=10,stringsAsFactors=F,header=T)
    snpID <- list('rs','i','d')
    if(unique(unique(sub('[0-9]+$','',testRead2[,1]))%!in%snpID)){
      write_log(LOGS, path, uniqueID, homeFolder, 'ancestry_problem')
      stop("Your file seemed like ancestry.com data, but didn't have rs IDs in column 1")
    } 
    else { #ok, this is probably an ancestry.com file. Let's reformat.
      reformat_outcome <- try(format_ancestry_com_as_23andme(path))
    }
    
  } else if(ncol(testRead)==1){#this could be myheritage. Let's try with that
    reformat_outcome <- try(format_myheritage_as_23andme(path))
  }else{
    reformat_outcome <- "didn't try"
  }
  
  if(class(reformat_outcome)=="try-error"){
    write_log(LOGS, path, uniqueID, homeFolder, 'reformat_error')
    stop("Your file didn't seem to match any of our import algorithms. If you think this data type should be supported, then you are welcome to write an email and attach a snippet of the data for our inspection.")
  }
}

# 5. Test the format of a second sampled read.
test_read2 <- function(path, uniqueID, LOGS, homeFolder) {
  testRead2 <- read.table(path,nrow=10,stringsAsFactors=F)
  if(ncol(testRead2)!=4){
    write_log(LOGS, path, uniqueID, homeFolder, 'test_read_4_columns')
    stop(safeError("Your file didn't have 4 columns (or 5 for ancestry.com data). If you think this data type should be supported, then you are welcome to write an email and attach a snippet of the data for our inspection."))
  }
  snpID <- list('rs','i','d')
  if(unique(unique(sub('[0-9]+$','',testRead2[,1]))%!in%snpID)){
    write_log(LOGS, path, uniqueID, homeFolder, 'test_read_no_rs_id')
    stop("Your file didn't have rs IDs in column 1. If you think this data type should be supported, then you are welcome to write an email and attach a snippet of the data for our inspection.")
  }
}


# 6. Checking if it is a consistent file & acceptable format
check_consistent_file <- function(path, uniqueID, LOGS, homeFolder)
{
  testRead <- try(read.table(path,nrow=10,stringsAsFactors=F))
  if(class(testRead)=="try-error"){
    write_log(LOGS, path, uniqueID, homeFolder, 'general_data_file_problem')
    stop("Your file didn't seem like genomic data at all. It must contain many rows, one per SNP, with information about your genotype. Please write an email if you think this is a mistake and that this file format should be supported.")
  }
}

# 7. Check that input file is/is not from "Good 4 Genes"
check_good4genes <- function(path, uniqueID, LOGS, homeFolder)
{
  if(length(grep("genes for good",tolower(readLines(path,n=2))))>0) {
    write_log(LOGS, path, uniqueID, homeFolder, 'genes_for_good_error')
    stop(paste0("Your file seemed to be from Genes for Good. At the moment we can't accept data from Genes for Good because it is made in a different genomic version than other direct-to-consumer data. If you know how to translate to GRCH37-built yourself, you may remove the 'Genes for Good' line in the header and try to resubmit. Otherwise - we are working on a solution."))
  }
}

# 8. Check that the number of lines of the input files is consistent with genome-wide array
check_numlines <- function(path, uniqueID, LOGS, homeFolder)
{
  cmd1 <- paste0("wc -l < ", path, "| awk '{print $1}'") # WC is faster than counting in R
  lines <- as.numeric(system(cmd1,intern=T))
  if(lines < 10000){
    write_log(LOGS, path, uniqueID, homeFolder, 'too_few_lines_error')
    stop(paste0("Your file only had ", lines," lines. Each line represents a measurement and there are too few measurements to perform imputation. Measurements from a genome-wide microarray are needed. Genome-wide microarray files have many formats and come from many places (23andme, myheritage, ancestry, geneplaza, etc), but they always have hundreds of thousands of measurements"))
  }
}

# 9. Check the mdsum of the file
check_md5sum <- function(path, uniqueID, LOGS, homeFolder) 
{
  this_person_md5sum <- md5sum(path)
  info <- file.info("misc_files/md5sums.txt")
  if (info$size > 5) {
    all_md5sums <- read.table("misc_files/md5sums.txt",sep="\t",stringsAsFactors = F)[,1]
    if(this_person_md5sum %in% all_md5sums){
      write_log(LOGS, path, uniqueID, homeFolder, 'md5sum_match')
      stop("A person with this genome was already analyzed by the system. Write an email if you wish to clear this flag.")
    }
  } else { return(this_person_md5sum)}
  return(this_person_md5sum)
}

# Generic function to write logs to any file
write_logs <- function(logs_file, message) {
  m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"), message)
  m<-paste(m, collapse="\t")
  write(m, file = logs_file, append = TRUE)
}

exit_on_fail <- function(message) {
  write_logs(LOGS, message = message)
  stop(message)
}
