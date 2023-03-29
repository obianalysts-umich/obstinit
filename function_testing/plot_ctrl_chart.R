
library(tidyverse)
library(data.table)

source("C:/repos/Semi_annual_meetings/spring_2023/code/thea/data_processing.R")

obi_ctrl_cohort = obi %>% obstinit::structure_data(infant_dob_dt, year_mon, cesarean, birth, long = T)

# make note that data need to be in long format for this step

plot_ctrl_chart = function(df, CL, UCL, LCL) {
  # first get LCL and UCL for area ------------------------------------------

  area_LCL = df %>% filter(ctrl_chart_part == "LCL") %>% select(date_var, ctrl_chart_value) %>% rename(LCL_long = ctrl_chart_value)

  area_UCL = df %>% filter(ctrl_chart_part == "UCL") %>% select(date_var, ctrl_chart_value) %>% rename(UCL_long = ctrl_chart_value)

  ## join LCL and UCL together

  area_limits = left_join(area_LCL, area_UCL, by = c("date_var"))

  ## join LCL and UCL to original

  dt_for_plot = left_join(df, area_limits, by = c("date_var"))
}


test_dt_for_plot = plot_ctrl_chart(obi_ctrl_cohort)

# plot --------------------------------------------------------------------

  control_chart = ggplot(aes(x = date_var), data = dt_for_plot) +
    geom_ribbon(
      aes(x = date_var, ymin = LCL_long, ymax = UCL_long),
      data = dt_for_plot %>% filter(ctrl_chart_part %in% c("LCL", "UCL")),
      fill = "#CAC4CE",
      alpha = 0.4
    ) +
    geom_line(
      aes(y = ctrl_chart_value),
      data = dt_for_plot %>% filter(ctrl_chart_part == "CL"),
      color = OBI.color::prim_dark_blue(),
      linetype = "dashed",
      linewidth = 0.75,
      alpha = 0.5
    ) +
    #obi goal
    geom_hline(
      aes(yintercept = 0.247),
      color = OBI.color::prim_green(),
      linetype = "dashed",
      linewidth = 0.75
    ) +
    geom_text(aes(x = as.yearmon("Sep 2020"), y = 0.25),
              label = "OBI NTSV Cesarean rate goal: 24.7%",
              color = OBI.color::prim_green()) +
    geom_line(
      aes(y = ctrl_chart_value, color = p_chart_color, group = 1),
      data = dt_for_plot %>% filter(ctrl_chart_part == "rate"),
      linewidth = 0.8
    ) +
    scale_color_identity() +
    geom_point(
      aes(y = ctrl_chart_value, fill = p_chart_color),
      data = dt_for_plot %>% filter(ctrl_chart_part == "rate"),
      size = 3,
      shape = 21,
      stroke = 0
    ) +
    scale_fill_identity(
      guide = guide_legend("Control chart alert", reverse = T),
      labels = c("No alert", "High outlier", "Shift up")
    ) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = paste0("Shewhart P control chart: ", {{measure}}),
      subtitle = "Gray area represents 'in control' range",
      x = paste0("Infant DOB: month & year"),
      y = paste0({{measure}}, " rate"),
      caption = paste0("Includes complete cases 2020-01-01 - ", sys_lock_date_120)
    ) +
    theme(plot.caption = element_text(hjust = 0))

  return(control_chart)

}
