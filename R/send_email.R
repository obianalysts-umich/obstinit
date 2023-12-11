

#' send emails from R
#' @description
#' send or save draft emails from R
#' 
#'
#' @param email_message your email body; use html code for breaks and hyperlinks
#' @param email_to a email address or a list of email addresses separated by ";"
#' @param email_subject text
#' @param cc a email address or a list of email addresses separated by ";"
#' @param save_or_send default to "save", can be "save" or "send"
#'
#' @return a message in the console
#' @export
#' @import cli
#'
#' @examples
#' \dontrun{
#' send_email(email_message = "<p> This is an automatic message. </p> <p> OBI analytics team </p>")
#' }


send_email <- function(email_message = "<p> This is an automatic message. </p> <p> OBI analytics team </p> ",
                       email_to = "obianalysts@umich.edu",
                       email_subject = "test",
                       cc = NULL,
                       save_or_send = "save"
                       ) {
  # message
  cli::cli_alert_info("This function was only tested using outlook app")
  
  # check if PC
  if (!Sys.info()["sysname"] == "Windows"){
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

