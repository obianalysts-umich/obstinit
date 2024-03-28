# this script include P4P measurements calculation 


#' Calculate the email submission rate for PRO
#'
#' @description
#' This function calculates the email submission rate  by
#' comparing the patient IDs in the OBI dataset with the patient IDs in the contact log.
#' It returns the number of patients, the number of patients who submitted their email,
#' and the percentage of patients who submitted their email.
#' compliance definition is at https://docs.google.com/spreadsheets/d/123r8ShMQuxl7GQQsX2Buuh9ynOyMRHOaSeKoFHiwx58/edit#gid=793851872
#'
#' @param obi_dt The input dataset containing patient information. If you only need a certain infant dob
#' range, you can filter the dataset before passing it to this function.
#' @param contact_log_path The file path to the contact log CSV file. Please
#' make sure that the contact log file should be from the same date as the OBI dataset.
#' @param by_site A logical value indicating whether to calculate the email submission rate by site.
#'
#' @return A data frame with the following columns:
#'   - n_pt: The total number of patients in the dataset.
#'   - n_submitted_email: The number of patients who submitted their email.
#'   - n_submitted_pct: The percentage of patients who submitted their email.
#'
#' @details
#' the reason why we use contact log instead of the variable *email_txt* in the OBI dataset
#' is that the variable *email_txt* in the OBI dataset was overwritten by data integration early in 2024.
#' It has been since fixed. But to be save, it's better to use contact log.
#' 
#' @family {P4P measures}
#'
#' @examples
#' /dontrun{
#' obi_dt <- read_current_data(sas_processed_dt = FALSE)
#' obi_dt_subset <- obi_dt |>
#'    filter(infant_dob_ymd >= "2023-01-01")
#' pv_email_submission_rate(obi_dt_subset, contact_log_path = "/path/to/contactlog.csv")
#' }

pv_email_submission_rate <- function(
    obi_dt,
    contact_log_path = "/Volumes/nur-kanelow/OBI_abstracted_data/Current_Data/data/input/contactlog.csv",
    by_site = TRUE) {
  # Read contact log, pts with emails are contacted
  contact_log <- read_csv(contact_log_path)

  # create flg for pts with emails
  obi_dt_consent <- obi_dt |>
    filter(is.na(pro_opt_out_e)) |>
    mutate(pt_with_emails_flg = ifelse(patientid %in% contact_log$patientid, 1, 0))

  # response rate -----------------------------
  if (by_site) {
    obi_dt_consent |>
      summarise(
        n_pt = n(),
        n_submitted_email = sum(pt_with_emails_flg),
        n_submitted_pct = round(n_submitted_email / n_pt, 2),
        .by = c(site_name)
      )
  } else {
    obi_dt_consent |>
      summarise(
        n_pt = n(),
        n_submitted_email = sum(pt_with_emails_flg),
        n_submitted_pct = round(n_submitted_email / n_pt, 2)
      )
  }
}


#' Calculate race and ethnicity missing measure
#'
#' @inheritParams pv_email_submission_rate 
#'
#' @details The function filters the input data table to include only records with infant date of birth
#'          on or after January 1, 2024. It then calculates the number of patients, the number of missing
#'          race and ethnicity values, and the percentage of missing values. If \code{by_site} is TRUE,
#'          the measures are calculated separately for each site.
#'
#' @examples
#' /dontrun{
#' # Calculate race and ethnicity measures for the entire dataset
#' race_ethnicity_measure(obi_dt)
#'
#' # Calculate race and ethnicity measures by site
#' race_ethnicity_measure(obi_dt, by_site = TRUE)
#' }
#'
#' @family {P4P measures}
#'
#' @importFrom dplyr filter summarise
#' @importFrom cli cli_alert_info
#' 
#' @return A summarized data table with the calculated measures.
#' 
race_ethnicity_measure <- function(obi_dt,
                                   by_site = TRUE){
  obi_dt_gt_2024 = obi_dt |>
    # race_ethnicity_cd only available after 2024-01-01
    filter(infant_dob_ymd >= "2024-01-01")
  
  cli::cli_alert_info("combined race_ethnicity_cd only available after 2024-01-01.")

  if (by_site) {
    obi_dt_gt_2024 |>
      summarise(
        n_pt = n(),
        n_NA_race_ethnicity = sum(is.na(race_ethnicity_cd)),
        n_missing_pct = round(n_NA_race_ethnicity / n_pt, 2),
        .by = c(site_name)
      )
  } else {
    obi_dt_gt_2024 |>
      summarise(
        n_pt = n(),
        n_NA_race_ethnicity = sum(is.na(race_ethnicity_cd)),
        n_missing_pct = round(n_NA_race_ethnicity / n_pt, 2)
      )
  }
}
