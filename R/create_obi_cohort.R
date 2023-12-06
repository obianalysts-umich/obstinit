
#' Create OBI cohort of locked, complete cases for most analyses
#' @description
#' this function does 
#'    1. include case ≥2020-01-01 based on infant date of birth
#'    2. include case if case is complete using flg_complete
#'    3. create case_locked varaible using 90 days lock
#'    4. filter to complete and locked cases(limit_to_locked = TRUE)
#' 
#' 
#' @param df Data frame -- usually this is the data frame read into R using data.table::fread
#' @param limit_to_locked Logical -- if TRUE, only return cases that are locked
#' @import tidyverse
#' @export

create_obi_cohort = function(df,
                             limit_to_locked = T) {
  df1 = df %>% mutate(
    infant_dob_dt = lubridate::dmy_hms(infant_dob_dt),
    #case locks at MIDNIGHT AFTER THIS DATE
    case_lock_dt = lubridate::date(infant_dob_dt) + days(90),
    case_locked = ifelse(case_lock_dt < lubridate::today(), 1, 0),
    across(
      c(
        starts_with("opioid_e"),
        starts_with("opioid_dose"),
        starts_with("opioid_quantity_no"),
        starts_with("opioid_unit")
      ),
      ~ ifelse(discharge_opioid_e == 1, .x, NA)
    ),
    mdhhs_id = case_when(
      external_mdhhs_site_id == 4001 ~ "04001",
      external_mdhhs_site_id == 8001 ~ "08001",
      external_mdhhs_site_id == 9005 ~ "09005",
      TRUE ~ as.character(external_mdhhs_site_id)
    )
  )
  
  if (limit_to_locked == T) {
    df1 %>% filter(
      flg_complete == 1,
      case_locked == 1,
      infant_dob_dt >= lubridate::ymd_hms("2020-01-01 00:00:00")
    )
  }
  
  else if (limit_to_locked == F) {
    df1 %>% filter(flg_complete == 1,
                   infant_dob_dt >= lubridate::ymd_hms("2020-01-01 00:00:00"))
  }
  
}
