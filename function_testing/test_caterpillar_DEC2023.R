
library(tidyverse)
library(obstinit)

obi = obstinit::read_current_data()

obi_cohort = obi |> 
  obstinit::create_obi_cohort()

test_ces = obi_cohort |> 
  create_caterpillar_df(cesarean, birth)

test_dys = obi_cohort |> 
  create_caterpillar_df(overall_dystocia_compliance_num, overall_dystocia_den_all)

test_ces |> 
  plot_caterpillar_chart()
