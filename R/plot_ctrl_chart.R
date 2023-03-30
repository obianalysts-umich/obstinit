
#' Plot control chart
#'
#' @param df A data frame in long format, created by the structure_data function
#' @import tidyverse
#' @import data.table
#' @export
#' @rdname plot_ctrl_chart


plot_ctrl_chart = function(df) {

  # assign collor based on value
  line_color <- c("0" = OBI.color::prim_dark_blue(), "1" = "#f8b434")

  dot_color_df <- df %>%
    distinct(p_chart_alert, point_color)
  dot_color <- dot_color_df$point_color
  names(dot_color) <- dot_color_df$p_chart_alert

  # plot --------------------------------------------------------------------

  # control_chart <-
    ggplot(aes(x = date_var),
           data = df) +
    geom_ribbon(
      aes(ymin = LCL, ymax = UCL),
      fill = "#CAC4CE",
      alpha = 0.4
    ) +
    geom_line(
      aes(y = CL),
      color = OBI.color::prim_dark_blue(),
      linetype = "dashed",
      linewidth = 0.75,
      alpha = 0.5
    ) +
    geom_line(
      aes(y = rate, color = as.factor(shift_line), group = 1),
      linewidth = 0.8
    ) +
    scale_colour_manual(values = line_color,
                        guide = guide_legend("")) +
    geom_point(
      aes(y = rate, fill = p_chart_alert),
      size = 3,
      shape = 21,
      stroke = 0
    ) +
    scale_fill_manual(values=dot_color,
                      guide = guide_legend("Control chart alert", reverse = T)) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent)

  return(control_chart)

}
