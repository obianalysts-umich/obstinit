
library(tidyverse)
library(data.table)
library(ggforce)

obi_cohort = data.table::fread(
  "P:/OBI_abstracted_data/Current_Data/data/output/sourcetables_OBI_export_recodes.csv"
) %>% mutate(infant_dob_dt = lubridate::dmy_hms(infant_dob_dt)) %>% filter(
  infant_dob_dt >= lubridate::ymd_hms("2020-01-01 00:00:00"),
  infant_dob_dt <= lubridate::ymd_hms("2022-11-30 23:59:59"),
  flg_complete == 1
)

ctrl_df = obi_cohort %>% obstinit::structure_data(infant_dob_dt, cesarean, birth)

plot_test = ctrl_df %>% obstinit::plot_ctrl_chart(ctrl_df)

test_that("plot looks right", {
  expect_snapshot(plot_ctrl_chart(ctrl_df))
})
