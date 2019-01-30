run_bulk_imputation <- function(
  uniqueIDs, 
  runDir, 
  shapeit="/Users/mitcheas/Impute/Tools/Shapeit/bin/shapeit",
  plink="/Users/mitcheas/Impute/Tools/Plink/plink",
  impute2="/Users/mitcheas/Impute/Tools/Impute2/impute2",
  minimac='/Users/mitcheas/Impute/Tools/Minimac3/bin/Minimac3',
  mach="/Users/mitcheas/Impute/Tools/Mach/mach1",
  gtool='/Users/mitcheas/Impute/gtool',
  sample_ref="/Users/mitcheas/Impute/Ref/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3.sample"
){
  library(tools)
  REF_PATH = '/Users/mitcheas/Impute/Ref/ALL_1000G_phase1integrated_v3_impute/'
  
  if(class(uniqueIDs)!="character")stop(paste("uniqueIDs must be character, not",class(uniqueIDs)))
  if(length(uniqueIDs)!=10)stop(paste("rawdata must be lengh 10, not",length(uniqueIDs)))
  
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
  
  if(class(impute2)!="character")stop(paste("impute2 must be character, not",class(impute2)))
  if(length(impute2)!=1)stop(paste("impute2 must be lengh 1, not",length(impute2)))
  if(!file.exists(impute2))stop(paste("Did not find impute2 at path:",impute2))
  
  if(class(sample_ref)!="character")stop(paste("sample_ref must be character, not",class(sample_ref)))
  if(length(sample_ref)!=1)stop(paste("sample_ref must be lengh 1, not",length(sample_ref)))
  if(!file.exists(sample_ref))stop(paste("Did not find sample_ref at path:",sample_ref))
  
  
  rawdata_files <- paste("~/imputations/imputation_folder_",uniqueIDs,"/",uniqueIDs,"_raw_data.txt",sep="")
  names(rawdata_files) <- uniqueIDs
  if(!all(file.exists(rawdata_files))){
    missing <- rawdata_files[!file.exists(rawdata_files)]
    stop(paste("Did not find these rawdata files:",paste(missing,collapse=", ")))
  }
  
  
  
  cat(paste0("Starting imputation running on these files:\nc('",paste(uniqueIDs,collapse="','"),"')\nGood luck!\n"))
  setwd(runDir)
  chromosomes <- c("X",as.character(1:22))
  # chromosomes <- c("22")
  
  for(uniqueID in uniqueIDs){
    print("")
    print(paste("Preparing files for",uniqueID))
    
    rawdata_file <- rawdata_files[uniqueID]
    
    #need to always check if the genes_for_good_cleaner should be run
    if(length(grep("genes for good",tolower(readLines(rawdata_file,n=5)))>0)){
      genes_for_good_cleaner(uniqueID, runDir)
    }
    
    #Load data using plink 1.9
    cmd1 <- paste0(plink," --noweb --23file ",rawdata_file," ",uniqueID," ",uniqueID," --recode --out step_1_",uniqueID)
    out1 <- system(cmd1)
    
    
    
    #If the standard command fails, we run an extensive error rescue. Hopefully shouldn't be used too often, but is nice for when people submit weird custom-setup data
    if(out1 == 3 ){
      special_error_check(uniqueID, runDir)
    }  
    
    #Rscript to omit duplicates
    map <- read.table(paste0("step_1_",uniqueID,".map"),sep='\t',stringsAsFactors=F,comment.char = "")
    exclude <- map[duplicated(map[,4]),2]
    print(paste('Removed',length(exclude),'SNPs that were duplicated'))
    write.table(exclude,file=paste0('step_2_',uniqueID,'_exclusions'),sep='\t',row.names=FALSE,col.names=F,quote=F)
    
    #loop over chromosomes
    for(chr in chromosomes){
      #First in loop - extract only one specific chromosome
      cmd2 <- paste(plink," --file step_1_",uniqueID," --chr ",chr," --make-bed --out step_2_",uniqueID,"_chr",chr," --exclude step_2_",uniqueID,"_exclusions",sep="")
      system(cmd2)
      
    }  
  }
  
  
  
  #loop over chromosomes
  for(chr in chromosomes){
    merge_files <- paste0("step_2_",sub("_raw_data.txt","",basename(rawdata_files)),"_chr",chr)
    merge_df <- data.frame(bed=paste0(merge_files,".bed"),bim=paste0(merge_files,".bim"),fam=paste0(merge_files,".fam"))
    merge_df <- merge_df[2:nrow(merge_df),]
    write.table(merge_df,file="step_2_merge_list.txt",sep="\t",quote=F,row.names=F,col.names=F)
    
    
    #check that all files are there
    missing <- vector()
    for(i in 1:nrow(merge_df)){
      for(j in 1:ncol(merge_df)){
        f <- as.character(merge_df[i,j])
        if(!file.exists(f)){
          missing <- c(missing, f)
        }
      }
    }
    if(length(missing)>0)stop(paste("Didn't find these",length(missing),"files:",paste(missing,collapse=", ")))
    
    
    
    #use plink to merge
    cmd1_a <- paste0(plink," --bfile ",merge_files[1]," --merge-list step_2_merge_list.txt --recode --out step_2m_chr",chr)
    out1_a <- system(cmd1_a)
    
    
    #handling nonbiallelic (just excluding them)
    if(out1_a==3){
      missnp <- read.table(paste0("step_2m_chr",chr,".missnp"),stringsAsFactors = F)[,1]
      if(length(missnp) > 500) {
        cat(paste("\n\nThe missnp>500 error was triggered. Outputting debug info\n\n"))
        debug_info <- list()
        for(uniqueID in uniqueIDs){
          cmd1_b <- paste0(plink," --bfile step_2_",uniqueID,"_chr",chr," --recode --out step_2_",uniqueID,"_chr",chr,"_missnp_hunt --extract step_2m_chr",chr,".missnp")
          system(cmd1_b)
          debug_ped <- readLines(paste0("step_2_",uniqueID,"_chr",chr,"_missnp_hunt.ped"))
          debug_gt <- strsplit(debug_ped," ")[[1]]
          debug_gt <- debug_gt[7:length(debug_gt)]
          debug_df <- data.frame(A1=debug_gt[c(T,F)], A2=debug_gt[c(F,T)])
          debug_df[,"snp"] <-read.table(paste0("step_2_",uniqueID,"_chr",chr,"_missnp_hunt.map"))[,2]
          debug_df[,"uniqueID"] <- uniqueID
          debug_info[[uniqueID]] <- debug_df
        }
        debug_all <- do.call(rbind,debug_info)
        for(msnp in sample(missnp,5)){
          s <- debug_all[debug_all[,"snp"]%in%msnp,]
          rownames(s) <- NULL
          print(msnp)
          print(s)
        }
        stop("Too many non-biallelic SNPs. This must be investigated. Check above debugging info")
        
      }
      for(uniqueID in uniqueIDs){
        #Go back to the previous files from previous step and take them, now excluding triallelics.
        cmd1_c <- paste0(plink," --bfile step_2_",uniqueID,"_chr",chr," --make-bed --out step_2_",uniqueID,"_chr",chr," --exclude step_2m_chr",chr,".missnp")
        system(cmd1_c)
      }
      #then retry merge
      cmd1_a <- paste0(plink," --bfile ",merge_files[1]," --merge-list step_2_merge_list.txt --recode --out step_2m_chr",chr)
      out1_a <- system(cmd1_a)
    }
    
    
    #check for position duplicates
    map <- read.table(paste0("step_2m_chr",chr,".map"),stringsAsFactors = F,comment.char = "")
    if(sum(duplicated(map[,4]))>10000)stop("Found way too many duplicate positions")
    exclude <- unique(map[duplicated(map[,4]),2])
    write.table(exclude,file=paste0('step_2_overall_exclusions_chr',chr),sep='\t',row.names=FALSE,col.names=F,quote=F)
    
    
    cmd1_b<-paste(plink," --file step_2m_chr",chr," --exclude step_2_overall_exclusions_chr",chr," --recode --out step_2_chr",chr,sep="")
    system(cmd1_b)
    
    
    
    #Then check for strand flips etc. 
    
    cmd3<-paste(shapeit," -check --input-ped step_2_chr",chr,".ped step_2_chr",chr,".map -M ", REF_PATH, "genetic_map_chr",chr,"_combined_b37.txt --input-ref",REF_PATH, "ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz",REF_PATH, "ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_2_chr",chr,"_shapeit_log",sep="")
#### SWITCH ShapeIt to MACH
    #cmd3<-paste(shapeit," -check --input-ped step_2_chr",chr,".ped step_2_chr",chr,".map -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_2_chr",chr,"_shapeit_log",sep="")
    system(cmd3)
    
    
    #Many homozygote SNPs will fail the check, because, well - of course, they don't have the ref-allele. So we make more detailed R script for sorting them
    logFile<-read.table(paste("step_2_chr",chr,"_shapeit_log.snp.strand",sep=""),sep='\t',stringsAsFactors=FALSE,header=F,skip=1,comment.char="")
    omitMissing<-logFile[logFile[,1] %in% 'Missing',3] #SNPs that were not found in 1kgenomes. Lot's of 23andme iXXXXX here.
    logStrand<-logFile[logFile[,1] %in% 'Strand',]
    omitNonIdentical<-logStrand[logStrand[,5] != logStrand[,6],3] #typically indels with different notation than 1kgenomes (<10 counts is usual)
    omitBlank<-logStrand[logStrand[,5]%in%'',3] #these are SNPs that are in map-file but contents is all-blank in input data. Safe to omit
    
    #These are super-annoying. We have to create another (fake) person with the alternative allele just for their sake. This next command takes all the homozygotes, minus the indels (which are too complicated to lift out from 23andme)
    forceHomozygoteTable<-logStrand[
      logStrand[,5] == logStrand[,6] & 
        nchar(logStrand[,9])==1 & 
        nchar(logStrand[,10])==1 &
        !logStrand[,5] %in% c("D","I") &
        !logStrand[,6] %in% c("D","I") 
      ,]
    
    #This removes any cases where there are more than two alleles involved
    forceHomozygoteTable<-forceHomozygoteTable[sapply(apply(forceHomozygoteTable[,c(5,6,9,10)],1,unique),length)==2,]
    
    #This removes any duplicates there might be
    forceHomozygoteTable<-forceHomozygoteTable[!duplicated(forceHomozygoteTable[,4]),]
    map<-read.table(paste("step_2_chr",chr,".map",sep=""),sep="\t",stringsAsFactors=F,comment.char = "")
    #This loads the ped file, and doubles it
    p_all<-strsplit(readLines(paste("step_2_chr",chr,".ped",sep=""))," ")
    ped2<-ped1<-p_all[[1]]
    ped2[1]<-"Temporary"
    ped2[2]<-"Non_person"
    if((length(ped1)-6) / 2 !=nrow(map))stop("mismatch between map and ped")
    replacementPos<-which(map[,2]%in%forceHomozygoteTable[,4])
    A1_pos<-7+2*(replacementPos-1)
    A2_pos<-8+2*(replacementPos-1)
    ped2[A1_pos]<-forceHomozygoteTable[,9]
    ped2[A2_pos]<-forceHomozygoteTable[,10]
    ped<-rbind(do.call(rbind,p_all),ped2)
    write.table(ped,paste("step_3_chr",chr,".ped",sep=""),sep=" ",col.names=F,row.names=F,quote=F)
    omitRemaining<-logStrand[!logStrand[,4]%in%forceHomozygoteTable[,4],3]
    print(paste('Omitting',length(omitMissing),'because of missing',length(omitBlank),'because they are blank, and',length(omitNonIdentical),'true strand flips'))
    write.table(c(omitNonIdentical,omitBlank,omitMissing,omitRemaining),file=paste("step_3_chr",chr,"_exclusions",sep=""),sep='\t',row.names=F,col.names=F,quote=F)
    
    
    #running the shapeit command (with eleven people, the ten right ones and a placeholder heterozygote
    #cmd4<-paste(shapeit," --input-ped step_3_chr",chr,".ped step_2_chr",chr,".map -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_4_chr",chr,"_shapeit_log --exclude-snp step_3_chr",chr,"_exclusions -O step_4_chr",chr,sep="")
    cmd4<-paste(shapeit," --input-ped step_3_chr",chr,".ped step_2_chr",chr,".map -M", REF_PATH, "genetic_map_chr",chr,"_combined_b37.txt --input-ref", REF_PATH, "ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz", REF_PATH, "ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_4_chr",chr,"_shapeit_log --exclude-snp step_3_chr",chr,"_exclusions -O step_4_chr",chr,sep="")
    system(cmd4)
    
    
    #checking for errors and stopping if there are any. No point to continue otherwise
    log<-readLines(paste("step_4_chr",chr,"_shapeit_log.log",sep=""))
    if(substr(log[length(log)],1,5)=="ERROR"){
      stop(paste("At chr",chr," the shapeit failed. Check this file for explanation: step_4_chr",chr,"_shapeit.log",sep=""))
    }
    
    #removing the placeholder person again
    left_limit <- 5 + length(merge_files) * 2
    cmd5_1<-paste0("cut --delimiter=' ' -f 1-",left_limit," step_4_chr",chr,".haps > step_5_chr",chr,".haps")
    system(cmd5_1)
    top_limit <- 2 + length(merge_files) 
    cmd5_2<-paste0("head -n ",top_limit," step_4_chr",chr,".sample > step_5_chr",chr,".sample")
    system(cmd5_2)
    
    
    
    #detect max length of each chromosome
    cmd6<-paste("zcat ", REF_PATH, "/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz | tail -n 1 | cut --delimiter=\\  -f 2",sep="")

    #cmd6<-paste("zcat /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz | tail -n 1 | cut --delimiter=\\  -f 2",sep="")
    maxPos<-as.numeric(system(cmd6,intern=T))
    
    
    #iterate over 5e6 chunks
    starts<-seq(0,maxPos,5e6)
    for(i in 1:length(starts)){
      start <- starts[i]
      end <- start+5e6
      cmd7<-paste(impute2," -m", REF_PATH, "genetic_map_chr",chr,"_combined_b37.txt -h ", REF_PATH, "ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l ", REF_PATH, "ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g step_5_chr",chr,".haps -int ",start," ",end," -Ne 20000 -o step_7_chr",chr,"_",i,sep="")

      #cmd7<-paste(impute2," -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g step_5_chr",chr,".haps -int ",start," ",end," -Ne 20000 -o step_7_chr",chr,"_",i,sep="")
      step_7_log<-system(cmd7)
      
      
      #test for memory-lack bug (step_7_log will be 137 if killed, otherwise 0)
      if(step_7_log == 137){
        #we divide the job in smaller bits 
        divisions<-3
        for(j in 1:divisions){
          start_2 <- floor(starts[i] + (j-1)*(5e6/ divisions))
          end_2 <- floor(starts[i]+ (j)*(5e6/ divisions))
          print(paste("restart imputation with new subset to avoid memory-lack bug:",start_2,"to",end_2)   )
          
          #cmd7<-paste(impute2," -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g step_5_chr",chr,".haps -int ",start_2," ",end_2," -Ne 20000 -o step_7_chr",chr,"_",i,"-",j,sep="")
          cmd7<-paste(impute2," -m ", REF_PATH, "genetic_map_chr",chr,"_combined_b37.txt -h ", REF_PATH, "ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l ", REF_PATH, "ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g step_5_chr",chr,".haps -int ",start_2," ",end_2," -Ne 20000 -o step_7_chr",chr,"_",i,"-",j,sep="")

          step_7_log_2<-system(cmd7)
          if(step_7_log_2 == 137){print("the memory problem was still active after second round")
          }
        }
      }
    }
  }
  
  
  #clean up pre-step-5 files to save space
  print("Performing file-deletion and clean-up")
  unlink(list.files(pattern="^step_1.+"))
  unlink(list.files(pattern="^step_2.+"))
  unlink(list.files(pattern="^step_3.+"))
  
  
  
  #then copy out each of the step_7 files into separate imputation folders
  sample_file<-grep("step_5",grep("\\.sample$",list.files(),value=T),value=T)[1] #just pick the first one - they should be identical
  samples<-read.table(sample_file,stringsAsFactors=F)
  allFiles1<-list.files(runDir)
  for(chr in chromosomes){
    for(uniqueID in uniqueIDs){
      print(paste("Cutting from",uniqueID,"chromosome",chr))
      # uniqueID<-sub("^.+/","",sub("_raw_data.txt$","",rawdata_file))
      outfolder <- paste0("~/imputations/imputation_folder_",uniqueID,"/")
      w<-which(samples[,1]%in%uniqueID) -2
      print(paste("Retrieving",uniqueID,"which is",w,"of",nrow(samples)-2))
      
      #cut and transfer sample file
      write.table(samples[c(1:2,w+2),],file=paste0(outfolder,"step_4_chr",chr,".sample"),quote=F,row.names=F,col.names = F)
      
      #cut and transfer gen files
      step7Files<-grep(paste0("^step_7_chr",chr,"_"),allFiles1,value=T)
      step7ResultsFiles<-grep("[0-9]$",step7Files,value=T)
      left <- 5 + (w-1) * 3 + 1
      middle <- 5 + (w-1) * 3 + 2
      right <- 5 + (w-1) * 3 + 3
      for(step7ResultsFile in step7ResultsFiles){
        cmd8<-paste0("cut --delimiter=' ' -f 1,2,3,4,5,",left,",",middle,",",right," ",step7ResultsFile," > ",outfolder,step7ResultsFile)
        system(cmd8)
      }
    }
    Sys.sleep(0.5) #take a break
    unlink(step7Files) #clean up files afterwards (or else we break the 30GB limit)
  }
}
