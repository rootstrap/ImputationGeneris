genes_for_good_cleaner <- function(uniqueID,runDir,plink="tools/Plink/plink")
{
  print("The genes_for_good_cleaner was activated")
  rawdata_file <- paste("imputations/imputation_folder_",uniqueID,"/",uniqueID,"_raw_data.txt",sep="")
  if(!file.exists(rawdata_file))stop(paste("error in special-error-check: didn't find file at",rawdata_file))
  #Common problem 1 -  # signs in the rsids. Should remove those lines.
  cmd_special_8 <- paste0("sed -i.bak6 '/#/d' ",rawdata_file)
  system(cmd_special_8)
}
