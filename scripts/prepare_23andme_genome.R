source('misc_files/config.r')

prepare_23andme_genome<-function(path, filename, upload_id, wd='.')
{
  # PATH = Full path and file name of input file
  # FILENAME = File name of input file
  # WD = Full path to working directory
  library(tools)
  source('scripts/custom_functions.R')
  # Set local variables here:
  setwd(wd)
  
  # Check format of input arguments
  if(class(path)!="character")stop(paste("path must be character, not",class(path)))
  if(length(path)!=1)stop(paste("path must be lengh 1, not",length(path)))
  if(!file.exists(path))stop(paste("Did not find file at path:",path))
  
  if(class(filename)!="character")stop(paste("filename must be character, not",class(filename)))
  if(length(filename)!=1)stop(paste("filename must be lengh 1, not",length(filename)))
  
  # instead of pulling errors, just fix it yourself (observed some users got confused already)
  filename <- gsub("\\ ","_",filename)
  filename <- gsub("[\\$\\&\\+\\,\\:\\;\\=\\?\\@\\#\\\"\\\']","",filename)
  
  ## 1. Create uniqueID of 9 chars
  ## Check that generated uniqueID is not found in sample names
  ## If so, stop input file processing and print error
  print("1. Create uniqueID")
#  uniqueID <- create_uniqueID()
  uniqueID <- upload_id
  print(paste0('UniqueID: ', uniqueID))
  
  ## 2. Create imputation folder and output data folder
  print("2. Create imputation folder and output data folder")
  homeFolderShort <- paste("imputation_folder",uniqueID,sep="_")
  homeFolder <- paste("imputations/",homeFolderShort,"/",sep="")
  jobStatusFile <- paste(homeFolder, "job_status.txt", sep="")
  dir.create(homeFolder, recursive=TRUE)
  write.table("Job is not ready yet",file=jobStatusFile,col.names=F,row.names=F,quote=F)
  
  ## 3. Unzipping (or not) and moving to new place
  print("3. Unzipping (or not) and moving to new place")
	newTempPath <- paste(homeFolder,paste(uniqueID,"_raw_data",sep=""),sep="")
  newUnzippedPath <- paste(homeFolder,paste(uniqueID,"_raw_data.txt",sep=""),sep="")
  check_zipped(path, newTempPath, newUnzippedPath, homeFolder, LOGS)
  path <- newUnzippedPath

################## FILE COPIED & UNCOMPRESSED ################

  ## 4. Checking if it is a consistent file & acceptable format
  print("4. Checking if it is a consistent file & acceptable format")
  check_consistent_file(path, uniqueID, LOGS, homeFolder)
  
  ## 5. Checking if it as a Genes for Good file (have to reject those, since it's different genome built)
  check_good4genes(path, uniqueID, LOGS, homeFolder)
  print("5. Passed Good 4 Genes check")
  
  ## 6. Checking if there is at least 10k lines otherwise imputation is not possible)
  print('6. Checking if there is at least 10k lines otherwise imputation is not possible')
  check_numlines(path, uniqueID, LOGS, homeFolder)

  ### REFORMAT INPUT FILE if in alternative format   
  ## 7. Running the alternative format converters.
  print('7. Running the alternative format converters.')
  test_read(path, uniqueID, LOGS, homeFolder)
  
  print('7b. Passed test_read')
  #after reformat attempts, perform one more test read and consider
  test_read2(path, uniqueID, LOGS, homeFolder)
  print('7b. Test reads passed formatting check')
  
  print("Finalize...")
  save(uniqueID,filename,file=paste(homeFolder,"variables.rdata",sep=""))
  
  unlink(jobStatusFile)
  write.table("Job is ready",file=jobStatusFile,col.names=F,row.names=F,quote=F)
  print("Preprocessing... Success!! Ready for imputation")
}
