
#' Create OBI cohort of locked, complete cases for most analyses
#' 
#' @param df Data frame -- usually this is the data frame read into R using data.table::fread
#' @import tidyverse
#' @export
#' @rdname create_obi_cohort

create_obi_cohort = function(df) {
  sys_lock_dt_90 = format(Sys.Date() - lubridate::days(91), "%m/%d/%Y")
  sys_lock_dt_120 = format(Sys.Date() - lubridate::days(121), "%m/%d/%Y")
  
  df %>% mutate(
    infant_dob_dt = lubridate::dmy_hms(infant_dob_dt),
    case_lock_dt = data.table::fifelse(
      infant_dob_dt >= lubridate::ymd_hms("2023-01-01 00:00:00"),
      infant_dob_dt + days(90),
      infant_dob_dt + days(120)
    ),
    case_locked = ifelse(case_lock_dt < Sys.Date(), 1, 0)
  ) %>% filter(
    flg_complete == 1,
    case_locked == 1,
    infant_dob_dt >= lubridate::ymd_hms("2020-01-01 00:00:00")
  )
  
}
