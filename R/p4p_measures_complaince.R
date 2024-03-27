library(obstinit)
library(tidyverse)


obi_dt <- read_current_data(sas_processed_dt = FALSE)

pv_email_submission_rate <- function(
    obi_dt,
    contact_log_path = "/Volumes/nur-kanelow/OBI_abstracted_data/Current_Data/data/input/contactlog.csv") {
  # Read contact log, pts with emails are contacted
  contact_log <- read_csv(contact_log_path)

  # create flg for pts with emails
  obi_dt <- obi_dt |>
    mutate(pt_with_emails_flg = ifelse(patientid %in% contact_log$patientid, 1, 0))
}
