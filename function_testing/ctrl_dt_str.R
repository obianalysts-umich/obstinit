
library(tidyverse)
library(zoo)
library(magrittr)

obi = data.table::fread("P:/OBI_abstracted_data/Current_Data/data/output/sourcetables_OBI_export_recodes.csv")

ctrl_dt_str = function(df,
                       date_var,
                       date_gran = c(year_mon, year_qtr),
                       num_var,
                       den_var) {
  df %>% filter(flg_complete == 1) %>% mutate(
    date_lub = lubridate::dmy_hms({
      {
        date_var
      }
    }),
    year_mon = zoo::as.yearmon(date_lub),
    year_qtr = zoo::as.yearqtr(date_lub),
    date_var = {
      {
        date_gran
      }
    }
  ) %>% group_by(date_var) %>% summarize(
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

test_dt = obi %>% ctrl_dt_str(infant_dob_dt, year_mon, cesarean, birth)
