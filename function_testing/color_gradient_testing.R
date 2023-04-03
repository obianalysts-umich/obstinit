
library(obstinit)
library(ggforce)

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

                                                                                   
                                                                                   
ctrl_chart = ctrl_df %>% plot_ctrl_chart() +
  #scale_color_gradient(low = OBI.color::prim_med_blue(), high = "red")
  scale_color_steps2(low = OBI.color::prim_dark_blue(),
                     mid = "#f8b434",
                     high = OBI.color::prim_pink())

ctrl_chart

test = ggplot(aes(x = date_var), data = ctrl_df) +
  geom_link2(aes(y = rate, color = text_color), linewidth = 1) +
  scale_color_gradient2(low = OBI.color::prim_dark_blue(),
                        mid = "#f8b434",
                        high = OBI.color::prim_pink()) +
  geom_point(aes(y = rate, fill = point_color_2), size = 3, shape = 21, stroke = 0) + 
  scale_fill_identity() +
  theme_bw()
  
  test

  ggsave(test, filename = "C:/Users/Althea Bourdeau/Desktop/images/geom_link_test.png")
  