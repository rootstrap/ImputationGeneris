# Set up environment & install software
### Run these commands in the root directory of this package

## 1. Make the following directories for output & temp file writing (data & imputations) and storage of imputation software (tools).
```bash
mkdir data
mkdir tools
mkdir imputations
```

## 2. Install fundamental packages & R
```bash
VAR=$PWD
export VAR
sudo apt-get update
sudo apt-get install r-base-dev
sudo apt-get install libcurl4-openssl-dev
sudo apt-get install default-jdk
```

## 3. Install R-packages
```bash
sudo -i
R
if (!requireNamespace("BiocManager"))
  install.packages("BiocManager")
BiocManager::install()
BiocManager::install(c('Mega2R'))
install.packages("rmarkdown")
install.packages("openxlsx")
install.packages("nlme")
install.packages("R.utils")
install.packages("mailR")
q()
y
exit
```

## 4. Install git
```bash
sudo apt-get install git
```

## 5. Download and unpack imputation related software. Confirm that the version is appropriate for the OS of the server.

#### Impute2
```bash
cd tools
wget https://mathgen.stats.ox.ac.uk/impute/impute_v2.3.2_x86_64_static.tgz
gunzip impute_v2.3.2_x86_64_static.tgz
tar -xvf impute_v2.3.2_x86_64_static.tar
mv imput* Impute2
cd $VAR
```

#### ShapeIt
```bash
cd tools
wget https://mathgen.stats.ox.ac.uk/genetics_software/shapeit/shapeit.v2.r837.GLIBCv2.12.Linux.static.tgz
tar zxvf shapeit.v2.r837.GLIBCv2.12.Linux.static.tgz
mv shapei* Shapeit
cd $VAR
```

#### Gtool
```bash
cd tools
mkdir Gtool
cd Gtool
wget http://www.well.ox.ac.uk/~cfreeman/software/gwas/gtool_v0.7.5_x86_64.tgz
tar zxvf gtool_v0.7.5_x86_64.tgz
mv gtoo* gtool
cd $VAR
```

#### Plink
```bash
cd tools
wget http://s3.amazonaws.com/plink1-assets/plink_linux_x86_64_20181202.zip
unzip plink_linux_x86_64_20181202.zip
mv plink* Plink
cd $VAR
```

## 6. I have already downloaded and preprocessed all reference data. It is located in the 'ref' directory. 
#### 'ref' directory to be uploaded...

## 7. Edit config file for cron job. Imputation is setup to run as a cron job so that it is automated. Parameters for the cron job are given in the file 'misc_files/config.r'. 

## 8. Setup a cron job so imputation is performed every hour (you can change this to occur more or less frequently). Imputation is only performed on files that have been processed after upload.
```bash
crontab -e 
50 * * * * Rscript scripts/imputation_cron_job.R > misc_files/cron_logs/`date +\%Y\%m\%d\%H\%M\%S`-impute-cron.log 2>&1
```

## 9. Link file upload to the script 'step1.sh'. This script runs an R script that preprocesses raw DNA data file from 23andme, Ancestry DNA, or My Heritage.
```bash
./step1.sh
```

