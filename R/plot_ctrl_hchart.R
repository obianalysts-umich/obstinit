
#' Plot interactive ctrl chart using highcharter package
#' 
#' @param df A data frame - should be in the structure of obstinit::structure_data
#' @param title Title for plot
#' @export
#' @rdname plot_ctrl_hchart
#' @import tidyverse

plot_ctrl_hchart = function(df, title) {
  plot_1 = highcharter::hchart(df,
                               "arearange",
                               hcaes(x = date_var, low = LCL, high = UCL)) %>%
    highcharter::hc_add_series(df,
                               "line",
                               hcaes(x = date_var, y = rate, color = point_color)) %>%
    highcharter::hc_add_series(
      df,
      "line",
      hcaes(x = date_var, y = CL),
      lineWidth = 1,
      dashStyle = "longdash",
      marker = F,
      tooltip = list(headerFormat = "", pointFormat = "<b>Frozen 2020 rate:</b> {point.CL}%")
    ) %>%
    highcharter::hc_yAxis(labels = list(format = "{value}%")) %>%
    highcharter::hc_plotOptions(
      arearange = list(
        color = "lightgrey",
        marker = F,
        tooltip = list(headerFormat = "", pointFormat = "<b>Lower ctrl limit:</b> {point.LCL}% <br> <b>Upper ctrl limit:</b> {point.UCL}%")
      ),
      line = list(
        color = OBI.color::prim_dark_blue(),
        marker = list(symbol = "circle", radius = 5),
        tooltip = list(pointFormat = "<b>Alert: {point.p_chart_alert}</b> <br> <b>Rate</b>: {point.rate}% <br> <b>Num:</b> {point.num} <br> <b>Denom:</b> {point.denom}")
      )
    ) %>% highcharter::hc_title(text = {
      {
        title
      }
    })
  
  ## fix x axis formats
  
  if(class(df$date_var) == "Date"){plot_1 %>%
      highcharter::hc_xAxis(
        type = "datetime",
        dateTimeLabelFormats = list(day = "%b %Y"),
        title = list(text = "")
      )}
  else(plot_1 %>%
         highcharter::hc_xAxis(
           title = list(text = "")
         ))
  
}
