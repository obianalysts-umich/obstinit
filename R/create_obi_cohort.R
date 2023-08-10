
#' Create OBI cohort of locked, complete cases for most analyses
#' 
#' @param df Data frame -- usually this is the data frame read into R using data.table::fread
#' @import tidyverse
#' @export
#' @rdname create_obi_cohort

create_obi_cohort = function(df) {
  sys_lock_dt_90 = format(lubridate::today() - lubridate::days(91), "%m/%d/%Y")
  sys_lock_dt_120 = format(lubridate::today() - lubridate::days(121), "%m/%d/%Y")
  
  df %>% mutate(
    infant_dob_dt = lubridate::dmy_hms(infant_dob_dt),
    #case locks at MIDNIGHT AFTER THIS DATE
    case_lock_dt = data.table::fifelse(
      infant_dob_dt >= lubridate::ymd_hms("2023-01-01 00:00:00"),
      lubridate::date(infant_dob_dt) + days(90),
      lubridate::date(infant_dob_dt) + days(120)
    ),
    case_locked = ifelse(case_lock_dt < lubridate::today(), 1, 0)
  ) %>% filter(
    flg_complete == 1,
    case_locked == 1,
    infant_dob_dt >= lubridate::ymd_hms("2020-01-01 00:00:00")
  )
  
}
