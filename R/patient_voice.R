# patient voice related functions


#' create patient voice response data
#' @description
#' this function creates a data frame excluding opt out patients 
#' - opt out from CDA
#' - opt out from patients
#'
#' @param obi_dt OBI nightly export data 
#'
#' @return a data frame with variables: patientid, site_name, infant_dob_dt, pro_response
#' @export
#'
#' @examples
#' obi_dt <- read_current_data() |> 
#'   create_obi_cohort()
#' create_pv_response_dt(obi_dt)

create_pv_response_dt <- function(obi_dt) {
  
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
  
  # response flags -----------------------------
  pro_pt_ids <- pro_survey_dt %>%
    filter(!patientid %in% opt_out_list) |>
    select(patientid) %>%
    mutate(pro_response = 1)
  
  obi_dt %>%
    filter(!patientid %in% opt_out_list) |>
    left_join(pro_pt_ids, by = c("patientid")) %>%
    mutate(pro_response = ifelse(is.na(pro_response), 0, 1)) %>%
    filter(birth_year >= 2023) |> 
    select(patientid, site_name, infant_dob_dt, pro_response)
}