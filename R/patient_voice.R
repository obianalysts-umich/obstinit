# patient voice related functions 


#' create patient voice response data 
#'  
#' @description 
#' This function create OBI subset cohort of pts who received pro survey emails.
#' There are two additional vars added to OBI dataset 
#'  - pro_response: pts who completed pro survey pro_response == 1 
#'  - survey_informed_consent_b_0: pts choose to opt out ==0; Self opt out pts also consider complete the survey.
#' 
#' @param obi_dt OBI nightly export data; use current data, becasue expiration date is based on current date
#' @param pro_survey_expired logical, if TRUE, only include patients who are after 12 weeks postpartum 
#' 
#' @return a data frame with variables: patientid, site_name, infant_dob_dt, complete_pro_survey_flg, survey_informed_consent_b_0 
#' @export 
#' 
#' @examples 
#' \dontrun{
#' obi_dt <- read_current_data() |> 
#'   create_obi_cohort(limit_to_locked = FALSE) 
#' create_pv_response_dt(obi_dt) 
#' }
#'  
#' @details 
#' this function read in pro survey data from  
#' - survey response: `P:/OBI_abstracted_data/Current_Data/data/input/pro_6_week_postpartum.csv` 
#' - contact log pts who received emails: `P:/OBI_abstracted_data/Current_Data/data/input/contactlog.csv` 
#' 

create_pv_response_dt <- function(obi_dt, 
                                  pro_survey_expired = FALSE) { 
  # Patient voice data 
  if (Sys.info()["sysname"] == "Windows") { 
    pro_survey_dt <- data.table::fread("P:/OBI_abstracted_data/Current_Data/data/input/pro_6_week_postpartum.csv") 
    contact_log <- readr::read_csv("P:/OBI_abstracted_data/Current_Data/data/input/contactlog.csv") 
    
  } else if (Sys.info()["sysname"] == "Darwin") { 
    pro_survey_dt <- data.table::fread("/Volumes/nur-kanelow/OBI_abstracted_data/Current_Data/data/input/pro_6_week_postpartum.csv") 
    contact_log <- readr::read_csv("/Volumes/nur-kanelow/OBI_abstracted_data/Current_Data/data/input/contactlog.csv") 
  } 
  
  # pts that have an opt-out email address
  obi_dt <- obstinit::read_current_data(sas_processed_dt = FALSE)
  
  opt_out_list <- obi_dt |> 
    dplyr::mutate(
      email_txt = tolower(email_txt),
      pro_opt_out_e_new = dplyr::case_when(
        email_txt == "ptnoemail@optout.com" ~ 1,
        email_txt == "optout@noemail.com" ~ 1,
        email_txt == "obicustomersupport@med.umich.edu" ~ 1,
        email_txt == "obi-cda-support@med.umich.edu" ~ 1,
        grepl("optout", tolower(email_txt)) ~ 1,
        grepl("noemail", tolower(email_txt)) ~ 1,
        TRUE ~ NA
      )
    ) |> 
    dplyr::filter(pro_opt_out_e_new == 1) |> 
    dplyr::pull(patientid)
  
  
  # pts that have received pro survey 
  contact_log_pt = contact_log |>  
    dplyr::distinct(patientid) |> 
    dplyr::filter(!(patientid %in% opt_out_list))
  
  # select complete survey vars 
  pro_survey_dt_select = pro_survey_dt |>  
    dplyr::select(patientid, survey_informed_consent_b_0)  
  
  # merge contact log with pro survey data 
  pro_survey_sent_complete_dt = contact_log_pt |>  
    dplyr::mutate(complete_pro_survey_flg = ifelse(patientid %in% pro_survey_dt_select$patientid, 1, 0)) |> 
    dplyr::left_join(pro_survey_dt_select) 
  
  # add variable pro_response flag to obi_dt 
  obi_dt_pro_flg <- obi_dt |> 
    dplyr::filter(patientid %in% pro_survey_sent_complete_dt$patientid) |> 
    dplyr::left_join(pro_survey_sent_complete_dt, by = c("patientid")) |>  
    dplyr::select(patientid, site_name, external_mdhhs_site_id, infant_dob_dt, complete_pro_survey_flg, survey_informed_consent_b_0) 
  
  if (pro_survey_expired == TRUE) { 
    # PRO survey expiration date 
    pro_expire_date <- lubridate::as_date(Sys.Date() - 12 * 7) # 12 weeks postpartum 
    
    obi_dt_pro_flg |> 
      dplyr::filter(infant_dob_dt <= pro_expire_date) 
  } else { 
    obi_dt_pro_flg 
  } 
} 