
library(tidyverse)
library(zoo)
library(magrittr)

obi = data.table::fread("P:/OBI_abstracted_data/Current_Data/data/output/sourcetables_OBI_export_recodes.csv")

ctrl_dt_str = function(df, date_var) {
  df = df %>% filter(flg_complete == 1) %>% mutate(
    date_lub = lubridate::dmy_hms({
      {
        date_var
      }
    }),
    year_mon = zoo::as.yearmon(date_lub),
    year_qtr = zoo::as.yearqtr(date_lub)
  ) %>% group_by(year_mon) %>% summarize(
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

test = obi %>% test_fun(infant_dob_dt, cesarean, birth)

