
library(tidyverse)
library(zoo)
library(magrittr)
library(qicharts2)

obi = data.table::fread("P:/OBI_abstracted_data/Current_Data/data/output/sourcetables_OBI_export_recodes.csv")

structure_data = function(df,
                       date_var,
                       date_gran = c(year_mon, year_qtr),
                       num_var,
                       den_var) {
  ctrl_cohort = df %>% filter(flg_complete == 1) %>% mutate(
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

# qicharts2 package to get CL frozen @ 12 mo ------------------------------

  limits_pre = qic(
    num,
    n = denom,
    x = date_var,
    freeze = 12,
    data = ctrl_cohort,
    chart    = 'p'
  )

  ## get CL
  CL_pre = summary(limits_pre)[, 12]

  ## rep values
  CL = rep(CL_pre, nrow(ctrl_cohort))

  ## bind to original
  ctrl_w_CL = cbind(ctrl_cohort, CL)

}

test_dt = obi %>% structure_data(infant_dob_dt, year_mon, cesarean, birth)
