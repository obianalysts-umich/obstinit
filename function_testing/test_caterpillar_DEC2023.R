
library(tidyverse)
library(obstinit)

obi = obstinit::read_current_data()

obi_cohort = obi |> 
  obstinit::create_obi_cohort()

test_ces = obi_cohort |> 
  obstinit::create_caterpillar_df(cesarean, birth)

test_dys = obi_cohort |> 
  obstinit::create_caterpillar_df(overall_dystocia_compliance_num, overall_dystocia_den_all)

test_plot = test_ces |> 
  obstinit::plot_caterpillar_chart()

test_plot |> 
  add_benchmark(y_int = 0.8, add_label = T, label_text = "test", label_xval = 10)
