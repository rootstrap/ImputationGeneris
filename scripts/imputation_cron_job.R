# 
#Strategy - setup this to run every hour on the hour, 
# 	
#Don't run it as root. THis is better
# crontab -u ubuntu -e
# 50 * * * * Rscript scripts/imputation_cron_job.R > misc_files/cron_logs/`date +\%Y\%m\%d\%H\%M\%S`-impute-cron.log 2>&1

library("rJava")
library("tools")
source("scripts/functions.R")
source('misc_files/config.r')
source("scripts/custom_functions.R")
source("scripts/email_sender.R")

#First checking if node is already at max load (maxImputations)
foldersToCheck <- grep("^imputation_folder",list.files("imputations/"),value=T)
runningJobCount <- 0
remoteRunningJobCount <- 0
for(folderToCheck in foldersToCheck){
  jobStatusFile <- paste("imputations/",folderToCheck,"/job_status.txt",sep="")
  if(file.exists(jobStatusFile)){
    jobStatus <- read.table(jobStatusFile,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
    if(jobStatus=="Job is running"){
      runningJobCount <- runningJobCount+1
    }
    if(jobStatus=="Job is remote-running"){
      remoteRunningJobCount <- remoteRunningJobCount+1
    }
  }
}
if(runningJobCount > (maxImputations-1)){
  stop(paste("Found", runningJobCount,"running jobs, and max is",maxImputations,"so doing nothing"))
}

#money saving implementation. If this is hub and there's a job, just send an email an turn on a node server. That way server can run on a small computer
# if(serverRole== "Hub"){
#     stop("Hub running is not currently implemented, but could easily be in the future")
# }


#If the computer is not too busy and the serverRole is node - we fetch ONE job
if(serverRole== "Node"){
  #sort checking order by time entered
  cmd1 <- paste("ssh ", admin,"@",hubAddress," ls -l --time-style='+\\%Y-\\%m-\\%d-\\%H:\\%M:\\%S' imputations/  | tail -n +2",sep="")
  remotedata <- system(cmd1,intern=T)
  Sys.sleep(0.2)
  remotedata_df <- as.data.frame(do.call(rbind,strsplit(remotedata,"\\s+")),stringsAsFactors=F)
  remotedata_df <- remotedata_df[order(remotedata_df[,6]),]
  remoteFoldersToCheck <- remotedata_df[,7]
  
  
  #check if there's any fast-queue jobs to put up-front
  cmd0 <- paste("ssh ", admin,"@",hubAddress," cat misc_files/fast_queue_emails.txt
                ",sep="")
  f <- system(cmd0,intern=T)
  remoteFoldersToCheck <- c(remoteFoldersToCheck[remoteFoldersToCheck%in%f],remoteFoldersToCheck[!remoteFoldersToCheck%in%f])
  
  
  #then loop over all remote folders
  for(remoteFolderToCheck in remoteFoldersToCheck){
    cmd2 <- paste("ssh ", admin,"@",hubAddress," cat imputations/",remoteFolderToCheck,"/job_status.txt",sep="")
    jobStatus<-system(cmd2,intern=T)
    #Check if the job is ready
    if(jobStatus=="Job is ready"){
      print(paste("Found job-status file and job is ready",remoteFolderToCheck))
      
      #First write to job-status that now the job is off to a remote server
      cmd3 <- paste("ssh ", admin,"@",hubAddress," 'echo Job is remote-running > imputations/",remoteFolderToCheck,"/job_status.txt'",sep="")
      system(cmd3)
      
      #then copy all the files to here
      cmd4 <- paste("scp -r ", admin,"@",hubAddress,":imputations/",remoteFolderToCheck," imputations/",remoteFolderToCheck,sep="")
      system(cmd4)
      
      #Then write locally that job is ready
      job_status_file <- paste("imputations/",remoteFolderToCheck,"/job_status.txt",sep="")
      unlink(job_status_file)
      write.table("Job is ready",file=job_status_file,col.names=F,row.names=F,quote=F)
      break
    }
  }
  #Update the local foldersToCheck to reflect new arrivals
  foldersToCheck <- grep("^imputation_folder",list.files("imputations/"),value=T)
}

#Then - no matter the role - we check locally which, if any, folders are ready to run
imputeThisFolder <- NA
for(folderToCheck in foldersToCheck){
  job_status_file <- paste("imputations/",folderToCheck,"/job_status.txt",sep="")
  if(!file.exists(job_status_file)){
    print(paste("Didn't find a job-status file - should probably auto-delete",folderToCheck))
    next
  }
  jobStatus <- read.table(job_status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
  if(jobStatus=="Job is not ready yet"){
    print(paste("Found job-status file - but job is not ready yet",folderToCheck))
    next
  }
  if(jobStatus=="Job is running"){
    print(paste("Found job-status file - but job is already running",folderToCheck))
    next
  }
  if(jobStatus=="Job is ready"){
    print(paste("Found job-status file and job is ready",folderToCheck))
    unlink(job_status_file)
    write.table("Job is running",file=job_status_file,col.names=F,row.names=F,quote=F)
    imputeThisFolder <- folderToCheck
    break
  }
}
#Stop if none are found
if(is.na(imputeThisFolder)){
  stop("No folders were found to be ready for imputation")
}

runDir <- paste("imputations/", imputeThisFolder, sep="")
setwd(runDir)
load("variables.rdata")
rawdata <- paste(uniqueID, "_raw_data.txt", sep="")

captured_imputation_error <- function(failure_reason) {
  message <- paste(failure_reason, "ID:", uniqueID)
  write_logs(LOGS, message)
  notify_failed_imputation(uniqueID, error_message = message)
  send_email(message = message)
}

# Capturing errors to retry and communicate admins
tryCatch({
  single_imputation_run(uniqueID, rawdata)
}, error = function(error_message) {
  captured_imputation_error(paste("The imputation process failed. The error was:", error_message))
  tryCatch({
    single_imputation_run(uniqueID, rawdata)
  }, error = function(error_message) {
    captured_imputation_error(paste("The imputation process failed its 2nd attempt. The error was:", error_message))
  })
})
