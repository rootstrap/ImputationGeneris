source("misc_files/config.r")

# HTML messages supported
send_email <- function(recipient = adminRecipient, subject = "Imputation process errored", message) {
  library("mailR")
  send.mail(from = emailAddress,
            to = recipient,
            subject = subject,
            body = message,
            html=T,
            smtp = list(
              host.name = "smtp.sendgrid.net", 
              port = 25, 
              user.name = Sys.getenv("SENDGRID_USER"), 
              passwd = Sys.getenv("SENDGRID_API_KEY"), 
              ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
}
