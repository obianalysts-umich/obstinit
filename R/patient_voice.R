# patient voice related functions 


#' create patient voice response data 
#'  
#' @description 
#' combine patient contact log with survey response data 
#' - cohort: pts who received pro survey emails 
#' - added var:  
#'    - pro_response: pts who completed pro survey (opt out pts also consider complete) 
#'    - pts choose to opt out: survey_informed_consent_b_0 ==0 
#' 
#' @param obi_dt OBI nightly export data
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
  # PRO survey starting date 
  start_date <- lubridate::as_date("2023-5-10") 
  pro_expire_date <- lubridate::as_date(Sys.Date() - 12 * 7) # 12 weeks postpartum 
  
  # Patient voice data 
  if (Sys.info()["sysname"] == "Windows") { 
    pro_survey_dt <- data.table::fread("P:/OBI_abstracted_data/Current_Data/data/input/pro_6_week_postpartum.csv") 
    contact_log <- read_csv("P:/OBI_abstracted_data/Current_Data/data/input/contactlog.csv") 
    
  } else if (Sys.info()["sysname"] == "Darwin") { 
    pro_survey_dt <- data.table::fread("/Volumes/nur-kanelow/OBI_abstracted_data/Current_Data/data/input/pro_6_week_postpartum.csv") 
    contact_log <- read_csv("/Volumes/nur-kanelow/OBI_abstracted_data/Current_Data/data/input/contactlog.csv") 
  } 
  
  # pts that have received pro survey 
  contact_log_pt = contact_log |>  
    distinct(patientid)  
  
  # select complete survey vars 
  pro_survey_dt_select = pro_survey_dt |>  
    select(patientid, survey_informed_consent_b_0)  
  
  # merge contact log with pro survey data 
  pro_survey_sent_complete_dt = contact_log_pt |>  
    mutate(complete_pro_survey_flg = ifelse(patientid %in% pro_survey_dt_select$patientid, 1, 0)) |> 
    left_join(pro_survey_dt_select) 
  
  # add variable pro_response flag to obi_dt 
  obi_dt_pro_flg <- obi_dt %>% 
    filter(patientid %in% pro_survey_sent_complete_dt$patientid) |> 
    left_join(pro_survey_sent_complete_dt, by = c("patientid")) %>% 
    select(patientid, site_name, infant_dob_dt, complete_pro_survey_flg, survey_informed_consent_b_0) 
  
  if (pro_survey_expired == TRUE) { 
    obi_dt_pro_flg |> 
      filter(infant_dob_dt <= pro_expire_date) 
  } else { 
    obi_dt_pro_flg 
  } 
} 