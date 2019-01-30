summarize_imputation<-function(
  runDir='.',
  uniqueID,
  destinationDir,
  gtool='tools/Gtool/gtool',
  plink="tools/Plink/plink" #note, as of 2015-08-31 this must be plink 1.07, otherwise we get a bug
){
  if(class(runDir)!="character")stop(paste("runDir must be character, not",class(runDir)))
  if(length(runDir)!=1)stop(paste("runDir must be lengh 1, not",length(runDir)))
  if(!file.exists(runDir))stop(paste("Did not find runDir at path:",runDir))
  if(length(grep("/$",runDir))!=0)stop("Please don't use a trailing slash in the runDir")
  setwd(runDir)
  
  if(class(uniqueID)!="character")stop(paste("uniqueID must be character, not",class(uniqueID)))
  if(length(uniqueID)!=1)stop(paste("uniqueID must be lengh 1, not",length(uniqueID)))
  
  if(class(destinationDir)!="character")stop(paste("destinationDir must be character, not",class(destinationDir)))
  if(length(destinationDir)!=1)stop(paste("destinationDir must be lengh 1, not",length(destinationDir)))
  if(!file.exists(destinationDir))stop(paste("Did not find destinationDir at path:",destinationDir))
  if(length(grep("/$",destinationDir))!=0)stop("Please don't use a trailing slash in the destinationDir")
  
  if(class(gtool)!="character")stop(paste("gtools must be character, not",class(gtool)))
  if(length(gtool)!=1)stop(paste("gtools must be lengh 1, not",length(gtool)))
  if(!file.exists(gtool))stop(paste("Did not find gtools at path:",gtool))
  
  if(class(plink)!="character")stop(paste("plink must be character, not",class(plink)))
  if(length(plink)!=1)stop(paste("plink must be lengh 1, not",length(plink)))
  if(!file.exists(plink))stop(paste("Did not find plink at path:",plink))
  
  if(file.exists(paste0(destinationDir,"/",uniqueID))){
    if(length(list.files(paste0(destinationDir,"/",uniqueID)))>0){
      stop(paste0("The destinationDir '",paste0(destinationDir,"/",uniqueID),"' already exists and has files in it. This is a major unforeseen error")  )
    }
    
  }

  allFiles1 <- list.files(runDir)
  step7Files <- grep("^step_7_chr",allFiles1,value=T)
  step7ResultsFiles <- grep("[0-9]$",step7Files,value=T)
  chromosomes <- unique(sub("_[0-9-]+$","",sub("^step_7_chr","",step7ResultsFiles)))
  chromosomes <- chromosomes[order(suppressWarnings(as.numeric(chromosomes)))]
  
  for(chr in chromosomes){
    print(paste("Merging chunks in chromosome",chr))
    s <- grep(paste("^step_7_chr",chr,"_",sep=""), step7ResultsFiles,value=T)
    s <- s[order(as.numeric(sub("-[0-9]","",sub("^.+_","",s))),as.numeric(substr(sub("^.+_","",s),3,3)))]
    print(paste("For chr",chr,"these were the files to merge:",paste(s,collapse=", ")))
    cmd1 <- paste("cat ",paste(s,collapse=" ")," > ",uniqueID,"_chr",chr,".gen",sep="")
    system(cmd1)
    unlink(s)
    
  }	
  
  
  genFiles <- paste(uniqueID,"_chr",chromosomes,".gen",sep="")
  if(length(genFiles)==0)stop("Didn't find a single gen-file")
  
  #running a conversion first to plink then to 23andme	
  for(genFile in genFiles){
    
    chr <- sub("\\.gen$","",sub("^.+_chr","",genFile))
    print(paste("Simplifying in chromosome",chr))
    sampleFile <- paste("step_4_chr",chr,".sample",sep="")
    
    #make list of non-indels
    cmd2 <- paste("awk -F' ' '{ if ((length($4) > 1 ) || (length($5) > 1 )) print $2 }'",genFile,">",paste("step_8_chr",chr,"_snps_to_exclude",sep=""))
    system(cmd2)
    
    #exclude indels
    cmd3 <- paste(gtool," -S --g ",genFile," --s ",sampleFile," --exclusion step_8_chr",chr,"_snps_to_exclude --og step_8_chr",chr,".gen",sep="")
    system(cmd3)
    
    #Convert to ped format
    cmd4 <- paste(gtool," -G --g step_8_chr",chr,".gen --s ",sampleFile," --chr ",chr," --snp",sep="")
    system(cmd4)
    
    #reform to plink fam/bim/bed file			
    cmd5 <- paste(plink," --file step_8_chr",chr,".gen --recode transpose --noweb --out step_9_chr",chr,sep="")
    system(cmd5)

    #re-order to 23andme format
    cmd6 <- paste("awk '{ print $2 \"\t\" $1 \"\t\"$4\"\t\" $5 $6}' step_9_chr",chr,".tped  > step_10_chr",chr,".txt",sep="")
    system(cmd6)
    
    
    #The step 8 and also 9 sometime fails for no apparent reason. Probably memory. We therefore make a checkup, where
    #it is checked if the file actually exists and if not - a more complicated step splits it up in chunks.
    #It's not looking nice, but at least the split-up only needs to run in very low memory settings
    fileExists <- file.exists(paste("step_10_chr",chr,".txt",sep=""))
    if(fileExists){
      size <- file.info(paste("step_10_chr",chr,".txt",sep=""))["size"]
    }else{
      size <- 0	
    }
    
    #	arbitraly re-run if it's less than 100 bytes (fair to assume something was wrong then)
    if(size < 100 ){
      print(paste("retrying step 8-9 command for chr",chr,". Trying to split it in pieces (non-normal low memory running)"))
      cmd7 <- paste("split --verbose --lines 5000000 step_8_chr",chr,".gen step_8_extra_chr",chr,".gen",sep="")
      system(cmd7)
      chunks <- grep(paste("step_8_extra_chr",chr,"\\.gena[a-z]$",sep=""),list.files(runDir),value=T)
      for(chunk in chunks){
        ch <- sub("^.+\\.","",chunk)
        cmd8 <- paste(gtool," -G --g ",chunk," --s ",sampleFile," --chr ",chr," --snp",sep="")
        system(cmd8)
        #reform to plink fam/bim/bed file			
        cmd9 <- paste(plink," --file ",chunk," --recode --transpose --noweb --out step_9_chr",chr,"_",ch,sep="")
        system(cmd9)
        #re-order to 23andme format
        cmd10 <- paste("awk '{ print $2 \"\t\" $1 \"\t\"$4\"\t\" $5 $6}' step_9_chr",chr,"_",ch,".tped  > step_9_chr",chr,"_split_",ch,".txt",sep="")
        system(cmd10)
      }
      cmd11 <- paste("cat ",paste(paste("step_9_chr",chr,"_split_",sub("^.+\\.","",chunks),".txt",sep=""),collapse=" ")," > step_10_chr",chr,".txt",sep="")
      system(cmd11)			
    }
    
    #remove NN
    cmd12 <- paste("awk '{ if($4 != \"NN\") print}' step_10_chr",chr,".txt  >", sub("\\.gen$","",genFile),".simple_format.txt",sep="")
    system(cmd12)
    
    
    #removing some temporary files
    unlink(list.files(runDir,pattern=paste0("^step_8_chr",chr),full.names=T))
    unlink(list.files(runDir,pattern=paste0("^step_9_chr",chr),full.names=T))
    unlink(list.files(runDir,pattern=paste0("^step_10_chr",chr),full.names=T))
    
  }
  
  #preparing destinationDir
  prepDestinationDir <- paste(destinationDir,"/",uniqueID,sep="")
  if(!file.exists(prepDestinationDir))dir.create(prepDestinationDir, recursive = TRUE)
  
  #zipping and moving simple_format files
  zipFile_simpleformat <- paste(runDir,paste(uniqueID,".simple_format.zip",sep=""),sep="/")
  twentythreeandmeFiles <- paste(uniqueID,"_chr",chromosomes,".simple_format.txt",sep="")
  zip(zipFile_simpleformat, twentythreeandmeFiles, flags = "-r9X", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
  file.rename(zipFile_simpleformat, paste(prepDestinationDir,basename(zipFile_simpleformat),sep="/"))
  unlink(list.files(runDir,pattern="23andme",full.names=T))
  
  #zipping gen files
  zipFileGen <- paste(runDir,paste(uniqueID,".gen.zip",sep=""),sep="/")
  zip(zipFileGen, genFiles, flags = "-r9X", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
  file.rename(zipFileGen, paste(prepDestinationDir,basename(zipFileGen),sep="/"))
  unlink(genFiles)
  
  #move the original file as well
  zipFileOriginal <- paste(runDir,paste(uniqueID,".input_data.zip",sep=""),sep="/")
  zip(zipFileOriginal, paste(uniqueID,"_raw_data.txt",sep=""), flags = "-r9X", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
  file.rename(zipFileOriginal, paste(prepDestinationDir,basename(zipFileOriginal),sep="/"))
  
  
  
  
  #creating the pData file
  load(paste0(runDir,"/variables.rdata"))
  timeStamp <- format(Sys.time(),"%Y-%m-%d-%H-%M")
  md5sum <- md5sum(paste(uniqueID,"_raw_data.txt",sep=""))
  gender <- system(paste("cut --delimiter=' ' -f 6 ",runDir,"/step_4_chr22.sample",sep=""),intern=T)[3]
  f <- file(paste0(prepDestinationDir,"/pData.txt"),"w")
  writeLines(paste(c("uniqueID","filename","first_timeStamp","md5sum","gender"),collapse="\t"),f)
  writeLines(paste(c(uniqueID,filename,timeStamp,md5sum,gender),collapse="\t"),f)
  close(f)
  
  #determine if it is a bulk or single imputation
  crontabs <- grep("^#",system("crontab -l",intern=T),invert = T,value=T)
  crontabs <- sub(" .+$","",sub("^.+Rscript /home/ubuntu/srv/impute-me/imputeme/","",crontabs))
  if(any(c("bulk_imputation_cron_job.R","imputation_cron_job.R")%in%crontabs)){
    pData<-read.table(paste0(prepDestinationDir,"/pData.txt"),header=T,sep="\t",stringsAsFactors = F)
    if("imputation_cron_job.R"%in%crontabs){
      pData[1,"imputation_type"]<-"single"  
    }else{
      pData[1,"imputation_type"]<-"bulk"  
    }
    write.table(pData,file=paste0(prepDestinationDir,"/pData.txt"),sep="\t",col.names=T,row.names=F,quote=F)
  }
  
  #return paths
  returnPaths <- c(
    paste(prepDestinationDir,basename(zipFile_simpleformat),sep="/"),
    paste(prepDestinationDir,basename(zipFileGen),sep="/")
  )
  names(returnPaths) <- c("23andme","gen")
  
  return(returnPaths)
}

