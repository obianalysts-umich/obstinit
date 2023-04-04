
#' Plot control chart
#'
#' @param df A data frame in wide format, created by the structure_data function
#' @param increase_is_bad If TRUE, this is a trend that would ideally be decreasing over time; if FALSE, ideally increasing
#' @import data.table
#' @export
#' @rdname plot_ctrl_chart


plot_ctrl_chart = function(df, increase_is_bad = T) {
  
  # assign line color based on value
  
  line_color_pal = c(if (increase_is_bad &
                         any(df$p_chart_alert == "Below UCL")) {
    OBI.color::prim_teal()
  },
  if (increase_is_bad == F &
      any(df$p_chart_alert == "Below UCL")) {
    "#b64083"
  },
  if (any(df$p_chart_alert == "Shift")) {
    "#f8b434"
  },
  if (increase_is_bad &
      any(df$p_chart_alert == "Above UCL")) {
    "#b64083"
  },
  if (increase_is_bad == F &
      any(df$p_chart_alert == "Below UCL")) {
    OBI.color::prim_teal()
  },
  if (any(df$p_chart_alert == "No alert")) {
    OBI.color::prim_dark_blue()
  })
  
  # plot --------------------------------------------------------------------
  
  ggplot(aes(x = date_var),
         data = df) +
    geom_ribbon(aes(ymin = LCL, ymax = UCL),
                fill = "#CAC4CE",
                alpha = 0.4) +
    geom_line(
      aes(y = CL),
      color = OBI.color::prim_dark_blue(),
      linetype = "dashed",
      linewidth = 0.75,
      alpha = 0.5
    ) +
    geom_point(
      aes(y = rate, fill = point_color),
      size = 3,
      shape = 21,
      stroke = 0
    ) +
    scale_fill_identity(guide = guide_legend("Control chart alert", reverse = T),
                        labels = levels(factor(df$p_chart_alert))) +
    geom_link2(aes(y = rate, color = line_value), linewidth = 0.8) +
    scale_color_gradientn(colors = line_color_pal, guide = "none") +
    theme_bw() +
    scale_y_continuous(labels = scales::percent)
  
}
