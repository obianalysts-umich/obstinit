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
#' @family {2024 P4P measures}
#'
#' @examples
#' /dontrun{
#' obi_dt <- read_current_data(sas_processed_dt = FALSE)
#' obi_dt_subset <- obi_dt |>
#'    filter(infant_dob_ymd >= "2023-01-01")
#' pv_email_submission_rate(obi_dt_subset, contact_log_path = "/path/to/contactlog.csv")
#' }
#' 
#' @export

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
        n_submitted_pct = round(n_submitted_email / n_pt, 3),
        .by = c(site_name, external_mdhhs_id)
      )
  } else {
    obi_dt_consent |>
      summarise(
        n_pt = n(),
        n_submitted_email = sum(pt_with_emails_flg),
        n_submitted_pct = round(n_submitted_email / n_pt, 3)
      )
  }
}


#' Calculate race and ethnicity missing measure
#'
#' @inheritParams pv_email_submission_rate 
#'
#' @details It then calculates the number of patients, the number of missing
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
#' @family {2024 P4P measures}
#'
#' @importFrom dplyr filter summarise
#' @importFrom cli cli_alert_info
#' 
#' @return A summarized data table with the calculated measures.
#' 
#' @export

race_ethnicity_measure <- function(obi_dt,
                                   by_site = TRUE){

  # filter to ≥ year 2024 cases
  obi_dt_2024 = obi_dt |>
    filter(infant_year >= 2024)
  
  cli::cli_alert_warning("cases are filtered to infant dob year ≥ 2024")

  if (by_site) {
    obi_dt |>
      summarise(
        n_pt = n(),
        n_no_doc_race_ethnicity = sum(race_ethnicity_cd == "{99}", na.rm = TRUE),
        n_missing_pct = round(n_no_doc_race_ethnicity / n_pt, 3),
        .by = c(site_name, external_mdhhs_site_id)
      )
  } else {
    obi_dt |>
      summarise(
        n_pt = n(),
        n_no_doc_race_ethnicity = sum(race_ethnicity_cd == "{99}", na.rm = TRUE),
        n_missing_pct = round(n_no_doc_race_ethnicity / n_pt, 3)
      )
  }
}


#' Function to calculate the average days to submit for each site
#'
#' This function takes OBI data as input and calculates the average days to submit for each site.
#' It selects the patient ID, site name, and maximum days from delivery to submission from the input data frame.
#' Then, it calculates the average days to submit for each site and returns the result as a data frame.
#'
#' @param obi_dt A data frame containing the necessary columns (patientid, site_name, deliv_to_submit_max_days_int)
#'
#' @return A data frame with the average days to submit for each site
#'
#' @examples
#' data <- data.frame(patientid = c(1, 2, 3),
#'                    site_name = c("Site A", "Site B", "Site A"),
#'                    deliv_to_submit_max_days_int = c(5, 7, 3))
#' average_days_to_submit(data)
#' 
#' @family {2024 P4P measures}
#'
#' @export

average_days_to_submit <- function(obi_dt) {
  obi_dt |>
    select(
      site_name,  
      deliv_to_submit_max_days_int  
    ) |>
    # average days of submission
    summarise(
      avg_days_to_submit = mean(deliv_to_submit_max_days_int, na.rm = TRUE),  # Calculating the average days to submit
      .by = c(site_name)  
    )
}


#' Function to calculate compliance with scheduled acetaminophen and oral NSAID
#'
#' This function takes OBI data as input and calculates what proportion of eligible births got scheduled acetaminophen and oral NSAID
#'
#' @param obi_dt A data frame containing the necessary columns (patientid, site_name, deliv_to_submit_max_days_int)
#'
#' @return A data frame with the proportion of eligible births with scheduled acetaminophen and oral NSAID per site
#'
#' @examples
#' data <- data.frame(patientid = c(1, 2, 3),
#'                    site_name = c("Site A", "Site B", "Site A"),
#'                    deliv_to_submit_max_days_int = c(5, 7, 3))
#' prop_scheduled_acetaminophen_and_NSAID(data)
#' 
#' @family {2024 P4P measures}
#'
#' @export

prop_scheduled_non_opioid_meds <- function(obi_dt,
                                           by_site = T) {
  # filter to ≥ year 2024 cases and opioid cohort
  obi_dt_2024 <- obi_dt |>
    filter(infant_year >= 2024)
  
  cli::cli_alert_warning("cases are filtered to infant dob year ≥ 2024")
  
  if (by_site) {
    obi_dt_2024 |>
      filter(opioid_denom_flg == 1,
             acetaminophen_ordered_e != 4 &
               ibuprofen_ordered_e != 4) |>
      summarise(
        n_pt = n(),
        n_scheduled_non_opioid = sum(
          acetaminophen_ordered_e == 1 |
            ibuprofen_ordered_e == 1,
          na.rm = TRUE
        ),
        scheduled_non_opioid_pct = round(n_scheduled_non_opioid / n_pt, 3),
        .by = c(site_name, external_mdhhs_site_id)
      )
  } else {
    obi_dt_2024 |>
      # drop patients who have contraindication to BOTH acetaminophen and NSAIDs
      filter(opioid_denom_flg == 1,
             acetaminophen_ordered_e != 4 &
               ibuprofen_ordered_e != 4) |>
      summarise(
        n_pt = n(),
        n_scheduled_non_opioid = sum(
          acetaminophen_ordered_e == 1 |
            ibuprofen_ordered_e == 1,
          na.rm = TRUE
        ),
        scheduled_non_opioid_pct = round(n_scheduled_non_opioid / n_pt, 3)
      )
  }
  
}


#' Function to calculate compliance with COMFORT guideline by mode of delivery
#'
#' This function takes OBI data as input and calculates what proportion of eligible births were compliant with the COMFORT guideline for their particular mode of delivery
#' Modes of delivery include vaginal with no laceration, vaginal with 3rd/4th degree laceration, and cesarean section
#' Max acceptable OME is 0 for vaginal births with no laceration, 75 for vaginal births with 3rd/4th degree laceration, and 113 for cesarean births
#'
#' @param obi_dt A data frame containing the necessary columns. DATA MUST FIRST BE RUN THROUGH OBSTINIT::CREATE_OBI_COHORT() to have the necessary opioid variables
#'
#' @return A data frame with the proportion of eligible births with opioid prescribing consistent with the COMFORT guideline by mode of delivery
#' 
#' @family {2024 P4P measures}
#'
#' @export

prop_births_mtg_COMFORT_compliance <- function(obi_dt,
                                               by_site = T) {
  # max acceptable OME by mode of delivery
  max_OME_vag <- 0
  max_OME_vag_lac <- 75
  max_OME_ces <- 113
  
  # filter to ≥ year 2024 cases and push through create_opioid_cohort
  obi_dt_2024 <- obi_dt |>
    filter(infant_year >= 2024,
           opioid_denom_flg == 1)
  
  cli::cli_alert_warning("cases are filtered to infant dob year ≥ 2024")
  
  if (by_site) {
    obi_dt_2024 |>
      # mode of delivery groups
      mutate(
        # max acceptable OME per COMFORT
        max_acceptable_OME = case_when(
          opioid_group == "Vaginal" ~ max_OME_vag,
          opioid_group == "Vaginal with laceration" ~ max_OME_vag_lac,
          opioid_group == "Cesarean" ~ max_OME_ces,
          TRUE ~ NA
        )
      ) |>
      summarise(
        n_pt = n(),
        n_mtg_COMFORT = sum(opioid_OME_total <= max_acceptable_OME, na.rm = TRUE),
        n_mtg_COMFORT_pct = round(n_scheduled_non_opioid / n_pt, 3),
        .by = c(site_name, external_mdhhs_site_id)
      )
  } else {
    obi_dt_2024 |>
      # mode of delivery groups
      mutate(
        # max acceptable OME per COMFORT
        max_acceptable_OME = case_when(
          opioid_group == "Vaginal" ~ max_OME_vag,
          opioid_group == "Vaginal with laceration" ~ max_OME_vag_lac,
          opioid_group == "Cesarean" ~ max_OME_ces,
          TRUE ~ NA
        )
      ) |>
      summarise(
        n_pt = n(),
        n_mtg_COMFORT = sum(opioid_OME_total <= max_acceptable_OME, na.rm = TRUE),
        n_mtg_COMFORT_pct = round(n_scheduled_non_opioid / n_pt, 3),
      )
  }
  
}
