
#' Plot control chart
#'
#' @param df A data frame in long format, created by the structure_data function
#' @import tidyverse
#' @import data.table
#' @export
#' @rdname plot_ctrl_chart


plot_ctrl_chart = function(df) {
  # first get LCL and UCL for area ------------------------------------------
  area_LCL = df %>%
    filter(ctrl_chart_part == "LCL") %>%
    select(date_var, ctrl_chart_value) %>%
    rename(LCL_long = ctrl_chart_value)

  area_UCL <- df %>%
    filter(ctrl_chart_part == "UCL") %>%
    select(date_var, ctrl_chart_value) %>%
    rename(UCL_long = ctrl_chart_value)

  ## join LCL and UCL together

  area_limits = left_join(area_LCL, area_UCL, by = c("date_var"))

  ## join LCL and UCL to original

  dt_for_plot = left_join(df, area_limits, by = c("date_var"))

  # plot --------------------------------------------------------------------

  control_chart <- ggplot(aes(x = date_var), data = dt_for_plot) +
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
      labels = levels(factor(df$p_chart_alert))
    ) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent)

  return(control_chart)

}
