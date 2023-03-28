
library(tidyverse)
library(zoo)
library(magirttr)

obi = data.table::fread("P:/OBI_abstracted_data/Current_Data/data/output/sourcetables_OBI_export_recodes.csv")

ctrl_dt_str = function(df, date_var, date_gran, num_var, den_var) {
  df %>% filter(flg_complete == 1) %>% mutate(
    date_lub = lubridate::dmy_hms({
      {
        date_var
      }
    }),
    year_mon = zoo::as.yearmon(date_lub),
    year_qtr = zoo::as.yearqtr(date_lub)
  ) %>% group_by(ifelse(date_gran == "month", year_mon, year_qtr)) %>% summarize(
    num = sum({
      {
        num_var
      }
    }),
    denom = sum({
      {
        den_var
      }
    }),
    rate = num / denom,
    .groups = "drop"
  )
}

test = ctrl_dt_str(obi, infant_dob_dt, "quarter", cesarean, birth)

test_fun = function(df, date_var, date_gran) {
  df %>% filter(flg_complete == 1) %>% mutate(
    date_lub = lubridate::dmy_hms({
      {
        date_var
      }
    }),
    year_mon = zoo::as.yearmon(date_lub),
    year_qtr = zoo::as.yearqtr(date_lub)
  )
}

test = obi %>% test_fun(infant_dob_dt)
