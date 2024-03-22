#' send emails from R
#' 
#' @description Send or save draft emails from R
#'
#' @param email_message your email body; use html code for breaks and hyperlinks 
#' @param email_to an email address or a list of addresses in format: c(";")
#' @param email_from email send from address, for example `OBICustomerSupport@med.umich.edu`
#' @param email_subject your email subject
#' @param cc an email address or a list of addresses in format: c(";")
#' @param attachment a path name for a file to be attached to the email
#' @param reply_to an email address to which all replies will be sent 
#' @param save_or_send default to "save", can be "save" or "send"
#'
#' @return a message in the console
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' send_email(email_message = "<p> This is a test.")
#' }

send_email <- function(email_message = "<p> This is an automatic message. </p> <p> OBI analytics team </p> ",
                       email_to = "obianalysts@umich.edu",
                       email_from = NULL,
                       email_subject = "test",
                       cc = NULL,
                       attachment = NULL, 
                       reply_to = NULL, 
                       save_or_send = "save") {
    # message
    cli::cli_alert_info("This function was only tested using outlook app on PC")
    
    # check if PC
    if (!Sys.info()["sysname"] == "Windows") {
      cli::cli_abort("this function only works on Windows, see package RDCOMClient requirement.")
    }
    
    # install pkg if needed
    if (!require("RDCOMClient")) {
      cli::cli_alert_info("installing RDCOMClient package for email sending")
      devtools::install_github("omegahat/RDCOMClient")
    }
    
    # email set up ---------------------------------------------------------------
    OutApp <- COMCreate("Outlook.Application")
    outMail = OutApp$CreateItem(0)
    
    
    outMail[["To"]] = email_to
    outMail[["Cc"]] = cc
    outMail[["subject"]] = email_subject
    outMail[["HTMLBody"]] = email_message
    outMail[["SentOnBehalfOfName"]] <- email_from
    
    if (!is.null(attachment)) {
      outMail[["attachments"]]$Add(attachment)
    }
    
    # send or save ---------------------------------------------------------------
    
    if (save_or_send == "save") {
      outMail$Save()
      cli::cli_alert_success("email saved in draft; check your outlook draft")
    } else if (save_or_send == "send") {
      outMail$Send()
      cli::cli_alert_success("email sent to ", email_to, " with cc ", cc)
    } else {
      cli::cli_alert_info("save_or_send must be either 'save' or 'send'")
    }

 
}

  
  
# archived: use Microsoft365R --------------------------------------------------------------------
#   # message
#   cli::cli_alert_info("This function is only possible with Outlook app")
#   
#   # install pkg if needed
#   if (!require("Microsoft365R")) {
#     cli::cli_alert_info("installing Microsoft365R package for email sending")
#     devtools::install_github("Azure/Microsoft365R")
#   }
#   
#   #check list format ------------------------------------------------------
#   
#   if (grepl(";", email_to)) {
#     email_to <- strsplit(email_to, ";")
#     email_to <- email_to[[1]]
#   }
#   
#   # email set up ---------------------------------------------------------------
#   
#   use_account <- get_business_outlook()
#   
#   outMail <- use_account$create_email(
#     content_type = "html",
#     body = email_message,
#     subject = email_subject,
#     to = email_to,
#     cc = cc,
#     reply_to = reply_to,
#     send_now = F
#   )
#   
#   if (!is.null(attachment)) {
#     outMail$add_attachment(attachment)
#   }
#   
#   # send or save ---------------------------------------------------------------
#   
#   if (save_or_send == "save") {
#     cli::cli_alert_success("email saved in draft; check your outlook draft")
#   } else if (save_or_send == "send") {
#     outMail$send()
#     cli::cli_alert_success("email sent")
#   } else {
#     cli::cli_alert_info("save_or_send must be either 'save' or 'send'")
#   }
#   
#   # clean token credentials 
#   # if your password or account is updated
#   # AzureAuth::clean_token_directory()
# }


