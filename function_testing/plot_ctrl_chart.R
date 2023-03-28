
library(tidyverse)
library(data.table)

source("C:/repos/Semi_annual_meetings/spring_2023/code/thea/data_processing.R")

obi_ctrl_cohort = obi %>% obstinit::structure_data(infant_dob_dt, year_mon, cesarean, birth)
qichart
