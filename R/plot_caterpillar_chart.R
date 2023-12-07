
#' Plot caterpillar chart
#' @description
#' this function plots a caterpillar chart using the dataframe created by create_caterpillar_df()
#' 
#' @param df Data frame created by create_caterpillar_df()
#' @param y_var Variable to be placed on the y axis - default is rate, which is the rate variable created by create_caterpillar_df()
#' @param x_var Variable to be placed on the x axis - default is site_name
#' @param point_shape Shape of the points - default is "diamond"
#' @param point_size Size of the points - default is 2
#' @param bar_width Width of the error bars - default is 3
#' @param dt_color Color for the data points and errorbars - default is OBI.color::prim_dark_blue()
#' @rdname plot_caterpillar_chart
#' @import tidyverse
#' @import ggplot2
#' @export

plot_caterpillar_chart = function(df,
                                  y_var = rate,
                                  x_var = site_name,
                                  point_shape = "diamond",
                                  point_size = 2,
                                  bar_width = 3,
                                  dt_color = OBI.color::prim_dark_blue()) {
  df |>
    ggplot(aes(x = reorder({
      {
        x_var
      }
    }, {
      {
        y_var
      }
    }), y = {
      {
        y_var
      }
    })) +
    geom_point(shape = point_shape,
               size = point_size,
               color = dt_color) +
    geom_errorbar(
      aes(ymin = LC,
          ymax = UC),
      linewidth = bar_width,
      width = 0,
      color = dt_color,
      alpha = 0.3
    ) +
    scale_y_continuous(labels = scales::percent) +
    theme_obi() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.major.x = element_blank()
    )
}
