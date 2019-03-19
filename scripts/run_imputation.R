source("misc_files/config.r")

run_imputation<-function(
  uniqueID,
  rawdata, # path to DNA raw data file, in ancestry23 format
  runDir='.', 
  shapeit="tools/Shapeit",
  plink="tools/Plink",
  impute2="tools/Impute2",
  #minimac='tools/Minimac3/bin/Minimac3',
  #mach="tools/Mach/mach1",
  gtool='tools/gtool',
  sample_ref="ref/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3.sample"
)
{
  library(tools)
#  library(Mega2R)
  REF_PATH = 'ref/ALL_1000G_phase1integrated_v3_impute/'
  setwd(homePath)

  if(class(rawdata)!="character")stop(paste("rawdata must be character, not",class(rawdata)))
  if(length(rawdata)!=1)stop(paste("rawdata must be lengh 1, not",length(rawdata)))
  if(!file.exists(rawdata))stop(paste("Did not find rawdata at path:",rawdata))
  
  if(class(runDir)!="character")stop(paste("runDir must be character, not",class(runDir)))
  if(length(runDir)!=1)stop(paste("runDir must be lengh 1, not",length(runDir)))
  if(!file.exists(runDir))stop(paste("Did not find runDir at path:",runDir))
  if(length(grep("/$",runDir))!=0)stop("Please don't use a trailing slash in the runDir")
  
  if(class(shapeit)!="character")stop(paste("shapeit must be character, not",class(shapeit)))
  if(length(shapeit)!=1)stop(paste("shapeit must be lengh 1, not",length(shapeit)))
  if(!file.exists(shapeit))stop(paste("Did not find shapeit at path:",shapeit))
  
  if(class(plink)!="character")stop(paste("plink must be character, not",class(plink)))
  if(length(plink)!=1)stop(paste("plink must be lengh 1, not",length(plink)))
  if(!file.exists(plink))stop(paste("Did not find plink at path:",plink))
  
#  if(class(mach)!="character")stop(paste("plink must be character, not",class(mach)))
#  if(length(mach)!=1)stop(paste("plink must be lengh 1, not",length(mach)))
#  if(!file.exists(mach))stop(paste("Did not find plink at path:",mach))
  
#  if(class(minimac)!="character")stop(paste("plink must be character, not",class(minimac)))
#  if(length(minimac)!=1)stop(paste("plink must be lengh 1, not",length(minimac)))
#  if(!file.exists(minimac))stop(paste("Did not find plink at path:",minimac))
  
  if(class(gtool)!="character")stop(paste("gtool must be character, not",class(gtool)))
  if(length(gtool)!=1)stop(paste("gtool must be lengh 1, not",length(gtool)))
  if(!file.exists(gtool))stop(paste("Did not find gtool at path:",gtool))
  
  if(class(impute2)!="character")stop(paste("impute2 must be character, not",class(impute2)))
  if(length(impute2)!=1)stop(paste("impute2 must be lengh 1, not",length(impute2)))
  if(!file.exists(impute2))stop(paste("Did not find impute2 at path:",impute2))
  
  if(class(sample_ref)!="character")stop(paste("sample_ref must be character, not",class(sample_ref)))
  if(length(sample_ref)!=1)stop(paste("sample_ref must be lengh 1, not",length(sample_ref)))
  if(!file.exists(sample_ref))stop(paste("Did not find sample_ref at path:",sample_ref))
  
  #need to always check if the genes_for_good_cleaner should be run
  if(length(grep("genes for good",tolower(readLines(rawdata,n=5)))>0)){
    genes_for_good_cleaner(uniqueID,runDir)
  }
  
  #Load data using plink 1.9, recode 23andme format into .MAP & .PED format
  cmd1 <- paste(plink, "--noweb --23file", rawdata, uniqueID, uniqueID, "--recode --out step_1")
  # cmd1 <- paste(plink,"--noweb --23file",rawdata,"John Doe --recode --out step_1")
  out1 <- system(cmd1)
  
  
  #If the standard command fails, we run an extensive error rescue. 
  #Hopefully shouldn't be used too often, but is nice for when people submit weird custom-setup data
  if(out1 == 3){
    special_error_check(uniqueID,runDir)
  }  
  
  #Rscript to omit duplicates
  map <- read.table('step_1.map',sep='\t',stringsAsFactors=F,comment.char="")
  exclude <- map[duplicated(map[,4]),2]
  print(paste('Removed',length(exclude),'SNPs that were duplicated'))
  write.table(exclude,file='step_2_exclusions',sep='\t',row.names=FALSE,col.names=F,quote=F)
  
  
  #loop over chromosomes
  for(chr in c("X",as.character(1:22))){
    
    #First in loop - extract only one specific chromosome for .MAP & .PED files
    cmd2 <- paste(plink," --file step_1 --chr ",chr," --recode --out step_2_chr",chr," --exclude step_2_exclusions",sep="")
    out2 <- system(cmd2)
    
    #if X chromosome is missing it is allowed to skip forward
    if(out2 == 13 & chr == "X"){
      print("Didn't find X-chr data, so skipping that")
      next
    }
    
    # Phase SNPs & then check for strand flips etc. 
    cmd3 <- paste(shapeit," -check --input-ped step_2_chr",chr,".ped step_2_chr",chr,".map -M ", REF_PATH, "genetic_map_chr",chr,"_combined_b37.txt --input-ref ",REF_PATH, "ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz ",REF_PATH, "ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_2_chr",chr,"_shapeit_log",sep="")
#### SWITCH ShapeIt to MACH
    system(cmd3)
    
    
    #Many homozygote SNPs will fail the check, because, well - of course, they don't have 
    #the ref-allele. So we make more detailed R script for sorting them
    logFile <- read.table(paste("step_2_chr",chr,"_shapeit_log.snp.strand",sep=""),sep='\t',stringsAsFactors=FALSE,header=F,skip=1)
    omitMissing <- logFile[logFile[,1] %in% 'Missing',3]
    logStrand <- logFile[logFile[,1] %in% 'Strand',]
    omitNonIdentical <- logStrand[logStrand[,5] != logStrand[,6],3]
    omitBlank <- logStrand[logStrand[,5]%in%'',3]
    
    #These are super-annoying. We have to create another (fake) person with the alternative allele 
    #just for their sake. This next command takes all the homozygotes, minus the indels 
    #(which are too complicated to lift out from 23andme)
    forceHomozygoteTable <- logStrand[
      logStrand[,5] == logStrand[,6] & 
        nchar(logStrand[,9])==1 & 
        nchar(logStrand[,10])==1 &
        !logStrand[,5] %in% c("D","I") &
        !logStrand[,6] %in% c("D","I") 
      ,]
    
    #This removes any cases where there are more than two alleles involved
    forceHomozygoteTable <- forceHomozygoteTable[sapply(apply(forceHomozygoteTable[,c(5,6,9,10)],1,unique),length)==2,]
    
    #This removes any duplicates there might be
    forceHomozygoteTable <- forceHomozygoteTable[!duplicated(forceHomozygoteTable[,4]),]
    map <- read.table(paste("step_2_chr",chr,".map",sep=""),sep="\t",stringsAsFactors=F,comment.char = "")
    
    #This loads the ped file, and doubles it
    ped2 <- ped1 <- strsplit(readLines(paste("step_2_chr",chr,".ped",sep=""))," ")[[1]]
    ped2[1] <- "Temporary"
    ped2[2] <- "Non_person"
    if((length(ped1)-6) / 2 !=nrow(map))stop("mismatch between map and ped")
    replacementPos <- which(map[,2]%in%forceHomozygoteTable[,4])
    A1_pos <- 7+2*(replacementPos-1)
    A2_pos <- 8+2*(replacementPos-1)
    ped2[A1_pos] <- forceHomozygoteTable[,9]
    ped2[A2_pos] <- forceHomozygoteTable[,10]
    ped <- rbind(ped1,ped2)
    write.table(ped,paste("step_3_chr",chr,".ped",sep=""),sep=" ",col.names=F,row.names=F,quote=F)
    omitRemaining <- logStrand[!logStrand[,4]%in%forceHomozygoteTable[,4],3]
    print(paste('Omitting',length(omitMissing),'because of missing',length(omitBlank),'because they are blank, and',length(omitNonIdentical),'true strand flips'))
    write.table(c(omitNonIdentical,omitBlank,omitMissing,omitRemaining),file=paste("step_3_chr",chr,"_exclusions",sep=""),sep='\t',row.names=F,col.names=F,quote=F)
    
    
    #running the shapeit command (with two people, the right one and a placeholder heterozygote
    #### SWITCH ShapeIt to MACH
    cmd4 <- paste(shapeit," --input-ped step_3_chr",chr,".ped step_2_chr",chr,".map -M", REF_PATH, "genetic_map_chr",chr,"_combined_b37.txt --input-ref ", REF_PATH, "ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz ", REF_PATH, "ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_4_chr",chr,"_shapeit_log --exclude-snp step_3_chr",chr,"_exclusions -O step_4_chr",chr,sep="")
    system(cmd4)
    
    
    #checking for errors and stopping if there are any. No point to continue otherwise
    log <- readLines(paste("step_4_chr",chr,"_shapeit_log.log",sep=""))
    if(substr(log[length(log)],1,5)=="ERROR"){
      stop(paste("At chr",chr," the shapeit failed. Check this file for explanation: step_4_chr",chr,"_shapeit.log",sep=""))
    }
    
    #removing the placeholder person again
    cmd5_1 <- paste("cut --delimiter=' ' -f 1-7 step_4_chr",chr,".haps > step_5_chr",chr,".haps",sep="")
    system(cmd5_1)
    cmd5_2 <- paste("head -n 3 step_4_chr",chr,".sample > step_5_chr",chr,".sample",sep="")
    system(cmd5_2)
    
    
    #detect max length of each chromosome
    cmd6 <- paste("zcat ", REF_PATH, "/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz | tail -n 1 | cut --delimiter=' '  -f 2",sep="")
    maxPos <- as.numeric(system(cmd6,intern=T))
    
    
    #iterate over 5e6 chunks
    starts <- seq(0,maxPos,5e6)
    for(i in 1:length(starts)){
      start <- starts[i]
      end <- start+5e6
      
##### SWITCH Impute2 to minimac3      
      cmd7 <- paste(impute2, " -m ", REF_PATH, "genetic_map_chr",chr,"_combined_b37.txt -h ", REF_PATH, "ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l ", REF_PATH, "ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g step_5_chr",chr,".haps -int ",start," ",end," -Ne 20000 -o step_7_chr",chr,"_",i,sep="")
      step_7_log <- system(cmd7)
      
      #test for memory-lack bug (step_7_log will be 137 if killed, otherwise 0)
      if(step_7_log == 137){
        #we divide the job in smaller bits 
        divisions <- 3
        for(j in 1:divisions){
          start_2 <- floor(starts[i] + (j-1)*(5e6/ divisions))
          end_2 <- floor(starts[i]+ (j)*(5e6/ divisions))
          print(paste("restart imputation with new subset to avoid memory-lack bug:",start_2,"to",end_2)   )
          
          cmd7 <- paste(impute2," -m ", REF_PATH, "genetic_map_chr",chr,"_combined_b37.txt -h ", REF_PATH, "ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l ", REF_PATH, "ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g step_5_chr",chr,".haps -int ",start_2," ",end_2," -Ne 20000 -o step_7_chr",chr,"_",i,"-",j,sep="")
          step_7_log_2 <- psystem(cmd7)
          if(step_7_log_2 == 137){print("the memory problem was still active after second round")
          }
        }
      }
    }
  }
}
