
library(obstinit)
library(ggforce)

source("C:/repos/Semi_annual_meetings/spring_2023/code/thea/data_processing.R")

ctrl_df = obi_cohort %>% structure_data(infant_dob_dt, cesarean, birth) %>% mutate(test_alert = ifelse(
  p_chart_alert == "Above UCL",
  "Above UCL",
  ifelse(shift_line == "Shift", "Shift", "No alert")),
  text_color = case_when(test_alert == "Above UCL" ~ 1,
                         test_alert == "No alert" ~ -1,
                         test_alert == "Shift" ~ 0),
  point_color_2 = case_when(test_alert == "Above UCL" ~ OBI.color::prim_pink(),
                            test_alert == "Shift" ~ "#f8b434",
                            TRUE ~ OBI.color::prim_dark_blue()))


test = ggplot(aes(x = date_var), data = ctrl_df) +
  geom_link2(aes(y = rate, color = text_color), linewidth = 1) +
  scale_color_gradientn(colors = c(OBI.color::prim_dark_blue(), "#f8b434", OBI.color::prim_pink())) +
  geom_point(aes(y = rate, fill = point_color_2), size = 3, shape = 21, stroke = 0) + 
  scale_fill_identity() +
  theme_bw()
  
  test

  ggsave(test, filename = "C:/Users/Althea Bourdeau/Desktop/images/geom_link_test.png")
  