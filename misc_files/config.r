
# Maximum number of samples imputed in parallel
maxImputations <- 5

# Change this for the maximum number of imputations in queue
maxImputationsInQueue <- 10

# Intialize server role as 'hub'
serverRole <- "Hub"

# Change this to your hub IP address for cron job
hubAddress <- "54.187.201.139"

# Change this to your user name for ssh/scp user to your server
admin <- ''

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
#if(!exists("email_password"))stop("Didn't find email_password ")
#if(!is.character(email_password ))stop("email_password  not character")
#if(length(email_password )!=1)stop("email_password  not length 1")
#if(!exists("email_address"))stop("Didn't find email_address")
#if(!is.character(email_address))stop("email_address not character")
#if(length(email_address)!=1)stop("email_address not length 1")
#if(!exists("routinely_delete_this"))stop("Didn't find routinely_delete_this")
#if(!is.character(routinely_delete_this))stop("routinely_delete_this not character")
#if(!exists("paypal"))stop("Didn't find paypal")
#if(!is.character(paypal))stop("paypal not character")
#if(length(paypal)!=1)stop("paypal not length 1")


# #DONT NEED
# email_password <- ''
# 
# #DONT NEED
# email_address <-''
# 
# #DONT NEED
# routinely_delete_this <-''
# 
# #DONT NEED
# paypal <-''
