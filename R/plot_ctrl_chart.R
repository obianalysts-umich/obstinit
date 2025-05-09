
#' Plot control chart
#'
#' @param df A data frame in wide format, created by the structure_data function
#' @param point_size The size of the points on the plot, default value is 3
#' @param plot_center_line If TRUE, the center line will be added to the plot
#' @param quarterly If TRUE, the x axis will be formatted as quarterly; if FALSE, x axis is formatted monthly. Default value is FALSE (formatted monthly).
#' @import data.table
#' @importFrom ggforce geom_link2
#' @export
#' @rdname plot_ctrl_chart


plot_ctrl_chart <- function(df,
                            point_size = 3,
                            plot_center_line = T,
                            fmt_x_quarterly = F) {
  line_values = unique(df$col_ID)
  
  # sort value for line color assignment
  # make sure if violation happened before shift, color assignments are correct
  line_values_sort = line_values[order(line_values)]
  
  # assign labels for legend
  
  legend_lab = df |>
    select(point_color, p_chart_alert) |>
    group_by(point_color, p_chart_alert) |>
    slice_head() |>
    select(p_chart_alert) |>
    pull()
  
  # line color palette
  
  line_color_pal <- unique(df$point_color)
  
  # plot --------------------------------------------------------------------
  
  plot_1 <- ggplot(aes(x = date_var),
                   data = df) +
    geom_ribbon(aes(ymin = LCL, ymax = UCL),
                fill = "#CAC4CE",
                alpha = 0.4) +
    geom_point(
      aes(y = rate, fill = point_color),
      size = point_size,
      shape = 21,
      stroke = NA
    ) +
    scale_fill_identity(guide = guide_legend("Control chart alert"),
                        labels = legend_lab) +
    obstinit::theme_obi() +
    scale_y_continuous(labels = scales::percent)
  
  if (length(line_values) == 1) {
    plot_2 <- plot_1 +
      geom_line(aes(y = rate, color = point_color), linewidth = 0.8) +
      scale_color_identity(guide = "none")
  }
  else{
    plot_2 <- plot_1 +
      geom_link2(aes(y = rate, color = col_ID), linewidth = 0.8) +
      scale_color_gradientn(
        colors = line_color_pal,
        values = scales::rescale(line_values_sort),
        guide = "none"
      )
  }
  
  if (plot_center_line) {
    plot_3 <- plot_2 + geom_line(
      aes(y = CL),
      color = OBI.color::prim_dark_blue(),
      linetype = "dashed",
      linewidth = 0.75,
      alpha = 0.5
    )
  } else{
    plot_3 <- plot_2
  }
  
  if (fmt_x_quarterly) {
    plot_3 +
      zoo::scale_x_yearqtr(format = "%Y Q%q")
  }
  else{
    plot_3 +
      zoo::scale_x_yearmon(format = "%b %Y")
  }
}
