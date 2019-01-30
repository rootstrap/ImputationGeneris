special_error_check <- function(uniqueID,runDir,plink="/Users/mitcheas/Impute/Tools/Plink/plink")
{
  print("The special_error_check was activated")
  rawdata_file<-paste("imputations/imputation_folder_",uniqueID,"/",uniqueID,"_raw_data.txt",sep="")
  if(!file.exists(rawdata_file))stop(paste("error in special-error-check: didn't find file at",rawdata_file))
  
  special_error_status <- vector()
  line_count_cmd <- paste0("wc -l < ", path, "| awk '{print $1}'") 
  line_count_0 <- as.numeric(system(line_count_cmd,intern=T))
  
  #Common problem 1: mitochondrial SNPs (not used in any analysis anyway)
  cmd_special_1 <- paste("sed -i.bak1 '/\\tMT\\t/d'",rawdata_file)
  system(cmd_special_1)
  line_count_1 <- as.numeric(system(line_count_cmd,intern=T))
  
  if(line_count_1-line_count_0<0)special_error_status <- c(special_error_status, paste0("MT removals (",line_count_1-line_count_0,")"))
  
  #Common problem 2: Presence of triple dashes, that should just be double dashes
  md5_before <- md5sum(rawdata_file)
  cmd_special_2 <- paste0("sed -i.bak2 's/\\t---/\\t--/' ",rawdata_file)
  system(cmd_special_2)
  if(md5_before != md5sum(rawdata_file))c(special_error_status, "--- to --")
  
  #Common problem 3: Presence of indels that can't be handled by the plink -23file function
  #this needs to be handled in a very weird way, because clearly awk cant distinguish all line endings systematically
  cmd_special_3a <- paste0("awk '!(length($4) != 3)' ",rawdata_file, " > ",runDir,"/temp_indel_01.txt")
  system(cmd_special_3a)
  line_count_cmd <- paste0("wc -l < ", runDir,"/temp_indel_01.txt", "| awk '{print $1}'")
  line_count_3a <- as.numeric(system(line_count_cmd,intern=T))
  
  cmd_special_3b <- paste0("awk '!(length($4) != 2)' ",rawdata_file, " > ",runDir,"/temp_indel_02.txt")
  system(cmd_special_3b)
  line_count_cmd <- paste0("wc -l < ", runDir,"/temp_indel_02.txt", "| awk '{print $1}'")
  line_count_3b <- as.numeric(system(line_count_cmd,intern=T))
  
  
  if(line_count_3a > line_count_3b){
    file.rename(paste0(runDir,"/temp_indel_01.txt"),rawdata_file)
  }else{
    file.rename(paste0(runDir,"/temp_indel_02.txt"),rawdata_file)
  }
  line_count_cmd <- paste0("wc -l < ", path, "| awk '{print $1}'")
  line_count_3 <- as.numeric(system(line_count_cmd,intern=T))
  
  if(line_count_3-line_count_1<0)special_error_status <- c(special_error_status, paste0("INDEL removals (",line_count_3-line_count_1,")"))
  
  
  #Common problem 4: lack of sorting (first re-check if this is a problem after MT removal)
  cmd_special_3 <- paste(plink,"--noweb --23file ",rawdata_file," ",uniqueID," ",uniqueID," --recode --out step_1")
  sorting_check <- system(cmd_special_3,intern=T)
  if(length(grep("are out of order",sorting_check))>0){
    
    #sorting by chr then pos
    cmd_sort_1 <- paste0("sort -k2 -k3 -g -o ",runDir,"/temp01.txt ",rawdata_file)
    system(cmd_sort_1)
    
    #removing Y, xY, 23 chr (too risky to keep in after a sort)
    cmd_sort_2 <- paste0("sed -e '/\\tY\\t/d' -e '/\\t23\\t/d' -e '/\\tYX\\t/d' -e '/\\tXY\\t/d' ",runDir,"/temp01.txt > ",runDir,"/temp02.txt")
    system(cmd_sort_2)
    
    #switching X chr and the rest (because X gets sorted first)
    cmd_sort_3 <- paste0("grep -v \tX\t ",runDir,"/temp02.txt > ",runDir,"/temp03.txt")
    system(cmd_sort_3)
    cmd_sort_4 <- paste0("grep \tX\t ",runDir,"/temp02.txt >> ",runDir,"/temp03.txt")
    system(cmd_sort_4)
    
    #replace X with 23
    cmd_sort_5 <- paste0("sed 's/\\tX\\t/\\t23\t/' ",runDir,"/temp03.txt > ",rawdata_file)
    system(cmd_sort_5)
    
    line_count_4 <- as.integer(sub(" .+$","",system(line_count_cmd,intern=T)))
    special_error_status <- c(special_error_status,paste0("sorting required (",line_count_3-line_count_4," lines removed)"))
  }
  
  #common problem 5: Removing all front quotes, all back quotes and all quote-comma-quotes
  md5_before <- md5sum(rawdata_file)
  cmd_special_5 <- paste0("sed -i.bak3 -e 's/^\"//g' -e 's/\"$//g' -e 's/\",\"/\\t/g' -e 's/\"\\t\"/\\t/g' ",rawdata_file)
  system(cmd_special_5)
  if(md5_before != md5sum(rawdata_file))c(special_error_status, "removing weird quotes")
  
  
  #common problem 6: also when there is weird carriage returns    
  md5_before <- md5sum(rawdata_file)
  cmd_special_6 <- paste0("sed -i.bak4 's/\"\r//g' ",rawdata_file)
  system(cmd_special_6)
  if(md5_before != md5sum(rawdata_file))c(special_error_status, "removing weird carriage returns")
  
  
  #Common problem 7 - build version is wrong
  canaries <- rbind(
    c("rs3762954","662955"),
    c("rs390560","2601689"),
    c("rs10043332","3128346"),
    c("rs10070917","4955950"),
    c("rs11740668","404623"),
    c("rs999292","93218958"),
    c("rs13147822","107960572"),
    c("rs62574625","101218552"),
    c("rs11023374","14903636")
  )
  canaries <- data.frame(canaries,stringsAsFactors = F)
  colnames(canaries) <- c("snp","1kg_pos")
  canaries[,"1kg_pos"] <- as.numeric(canaries[,"1kg_pos"])
  
  
  map_file1 <- paste0(runDir,"/step_1_",uniqueID,".map")
  map_file2 <- paste0(runDir,"/step_1.map")
  if(file.exists(map_file1)){
    map_file <- map_file1
  }else if(file.exists(map_file2)){
    map_file <- map_file2
  }else{
    #This special case is when ALL prior plink runs have failed (probably due to sorting or whatever). 
    #In that case we re run plink, we can use the pre-sorting check command which really should work now
    print("re-running to map")
    system(cmd_special_3,intern=F)
    map_file <- map_file2
    if(!file.exists(map_file)){stop("Didn't find map file")}
  }
  map <- read.table(map_file,sep='\t',stringsAsFactors=F,comment.char = "")
  map <- map[!duplicated(map[,2]),]
  rownames(map) <- map[,2]
  canaries <- canaries[canaries[,"snp"]%in%rownames(map),]
  if(nrow(canaries)==0){
    c(special_error_status, "no snps for built check")
  }else{
    canaries[,"input_pos"] <- map[canaries[,"snp"],4]
    if(all(canaries[,"1kg_pos"] == canaries[,"input_pos"])){
      c(special_error_status, paste("passed built check with",nrow(canaries),"snps"))
    }else{
      c(special_error_status, paste("failed built check with",sum(canaries[,"1kg_pos"] != canaries[,"input_pos"]),"of",nrow(canaries),"snps"))
    }
  }
  
  #Common problem 8 - some providers have started putting # signs in the rsids (genes for good for example). Should remove those lines.
  md5_before <- md5sum(rawdata_file)
  cmd_special_8 <- paste0("sed -i.bak5 '/#/d' ",rawdata_file)
  system(cmd_special_8)
  if(md5_before != md5sum(rawdata_file))c(special_error_status, "hashtags in rsids")
  
  
  #then re-check and decide future action
  cmd1 <- paste0(plink," --noweb --23file ",rawdata_file," ",uniqueID," ",uniqueID," --recode --out step_1_",uniqueID)
  out1 <- system(cmd1)
  if(out1 == 0 & length(grep("failed built check",special_error_status))==0){
    print(paste0("ok, somehow the special errors section actually cleared up this one. These were the error messages: ",paste(special_error_status,collapse=", ")))
    #and move on
  }else{
    #however if still failing, we have to send a mail
    library("mailR")
    error1 <- system(cmd1,intern=T)
    message <- paste0(uniqueID," failed all attempts at starting imputation. It came with special error status:<b> ", paste(special_error_status,collapse=", "),". </b>The last error message was this: ",paste(error1,collapse="\n"),"\n\nThe files under analysis were these:\nc('",paste(uniqueIDs,collapse="','"),"')")
    send.mail(from = email_address,
              to = "asia.d.mitchell@gmail.com",
              subject = "Imputation has problem",
              body = message,
              html=T,
              smtp = list(
                host.name = "smtp.gmail.com", 
                port = 465, 
                user.name = email_address, 
                passwd = email_password, 
                ssl = TRUE),
              authenticate = TRUE,
              send = TRUE)
    stop("Sending error mail and giving up")
  }
  return(special_error_status)
}  
