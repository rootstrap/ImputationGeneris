
# Maximum number of samples imputed in parallel
maxImputations <- 5

# Change this for the maximum number of imputations in queue
maxImputationsInQueue <- 10

# Intialize server role as 'hub'
serverRole <- "Hub"

# Change this to your hub IP address for cron job
hubAddress <- "172.31.47.91"

# Change this to your user name for ssh/scp user to your server
admin <- "ec2-user"

# Points to the Generis API server.
apiServer <- "https://api.me2pt0.com/api/v1/"

# S3 Bucket name
bucketName <- Sys.getenv("AWS_BUCKET_NAME")

# S3 Region
region <- Sys.getenv("AWS_DEFAULT_REGION")

# Running environment
env <- Sys.getenv("ENV")

homePath <- Sys.getenv("HOME_PATH")

get_logs_file <- function() {
  logs_folder <- paste0(homePath, 'logs/submission/')
  file_path <- paste0(logs_folder, 'submission_log.txt')
  if (!dir.exists(logs_folder)) { dir.create(logs_folder, recursive = TRUE) }
  if (!file.exists(file_path)) { file.create(file_path) }
  file_path
} 

# Default logs output
LOGS <- get_logs_file()

adminRecipient <- Sys.getenv("ADMIN_EMAIL")

emailAddress <- 'generis@imputation-server.com'

# Path to S3 output files folder.
remoteOutputFolder <- paste("/uploads/", env, "/dna_files/imputation_output/", sep = "")

if(!exists("maxImputations"))stop("Didn't find maxImputations")
if(!is.numeric(maxImputations))stop("maxImputations not numeric")
if(length(maxImputations)!=1)stop("maxImputations not length 1")
if(!exists("maxImputationsInQueue"))stop("Didn't find maxImputationsInQueue")
if(!is.numeric(maxImputationsInQueue))stop("maxImputationsInQueue not numeric")
if(length(maxImputationsInQueue)!=1)stop("maxImputationsInQueue not length 1")
if(!exists("serverRole"))stop("Didn't find serverRole")
if(!is.character(serverRole))stop("serverRole not character")
if(length(serverRole)!=1)stop("serverRole not length 1")
if(!serverRole%in%c("Hub","Node"))stop("serverRole not Hub or Node")
if(!exists("hubAddress"))stop("Didn't find hubAddress")
if(!is.character(hubAddress))stop("hubAddress not character")
if(length(hubAddress)!=1)stop("hubAddress not length 1")
if(!exists("admin"))stop("Didn't find admin")
if(!is.character(admin))stop("admin not character")
if(length(admin)!=1)stop("admin not length 1")

if(!is.character(apiServer))stop("apiServer not character")
if(length(apiServer)!=1)stop("apiServer not length 1")

if(!is.character(bucketName))stop("bucketName not character")
if(length(bucketName)!=1)stop("bucketName not length 1")

if(!is.character(region))stop("region not character")
if(length(region)!=1)stop("region not length 1")

if(!is.character(env))stop("env not character")
if(length(env)!=1)stop("env not length 1")

if(!is.character(remoteOutputFolder))stop("env not character")
if(length(remoteOutputFolder)!=1)stop("env not length 1")
