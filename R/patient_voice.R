# patient voice related functions


#' create patient voice response data
#' @description
#' this function creates a data frame excluding opt out patients
#' - opt out from CDA
#' - opt out from patients
#' - limited to infant dob > 2023-5-10 (when PRO survey started)
#'
#' @param obi_dt OBI nightly export data
#' @param pro_survey_expired logical, if TRUE, only include patients who are after 16 weeks postpartum
#'
#' @return a data frame with variables: patientid, site_name, infant_dob_dt, pro_response
#' @export
#'
#' @examples
#' obi_dt <- read_current_data() |>
#'   create_obi_cohort(limit_to_locked = FALSE)
#' create_pv_response_dt(obi_dt)
#' @details
#' this function read in pro survey data from `P:/OBI_abstracted_data/Current_Data/data/input/pro_6_week_postpartum.csv`
#'

create_pv_response_dt <- function(
    obi_dt,
    pro_survey_expired = FALSE) {
  # PRO survey starting date
  start_date <- lubridate::as_date("2023-5-10")
  pro_expire_date <- lubridate::as_date(Sys.Date() - 16 * 7) # 16 weeks postpartum

  # Patient voice data
  if (Sys.info()["sysname"] == "Windows") {
    pro_survey_dt <- data.table::fread("P:/OBI_abstracted_data/Current_Data/data/input/pro_6_week_postpartum.csv")
  } else if (Sys.info()["sysname"] == "Darwin") {
    pro_survey_dt <- data.table::fread("/Volumes/nur-kanelow/OBI_abstracted_data/Current_Data/data/input/pro_6_week_postpartum.csv")
  }

  # exclude opt out pts -----------------------------------------------------

  ## 1. pts choose to opt out survey
  pt_opt_out_list <-
    pro_survey_dt[survey_informed_consent_b_0 == 0]$patientid

  ## 2. CDA decide these pts should not receive survey
  cda_opt_out_list <- obi_dt[pro_opt_out_e == 1]$patientid

  # combine
  opt_out_list <- c(pt_opt_out_list, cda_opt_out_list)

  # response rate -----------------------------
  # responded pts
  pro_pt_ids <- pro_survey_dt %>%
    filter(!patientid %in% opt_out_list) |>
    select(patientid) %>%
    mutate(pro_response = 1)

  # add variable pro_response flag to obi_dt
  obi_dt_pro_flg <- obi_dt %>%
    filter(!patientid %in% opt_out_list) |>
    left_join(pro_pt_ids, by = c("patientid")) %>%
    mutate(pro_response = ifelse(is.na(pro_response), 0, 1)) %>%
    filter(infant_dob_dt >= start_date) |>
    select(patientid, site_name, infant_dob_dt, pro_response)

  message("Patient voice survey started on ", start_date, ". Response rates for 2023 are calculated after the start date. ")
  

  if (pro_survey_expired == TRUE) {
    obi_dt_pro_flg |>
      filter(infant_dob_dt <= pro_expire_date)
  } else {
    obi_dt_pro_flg
  }
}
