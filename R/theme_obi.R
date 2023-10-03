
#' Add OBI ggplot theme
#' 
#' @param x_axis_title_size Size for x axis title. Default is 12.
#' @param x_axis_text_size Size for x axis text. Default is 10.
#' @param y_axis_title_size Size for y axis title. Default is 12.
#' @param y_axis_text_size Size for y axis text. Default is 10.
#' @param title_size Size for title text. Default is 16.
#' @param title_hjust Position for title text. Default is 0.5 for centered, change to 0 for left-justified.
#' @param subtitle_size Size of subtitle text. Default is 12.
#' @param subtitle_hjust Position for subtitle text. Default is 0.5 for centered, change to 0 for left-justified.
#' @param caption_size Size for caption text. Default is 12.
#' @param caption_hjust Position for caption. Default is 0 for left-justified, change to 0.5 for centered.
#' @param name description
#' @param name description
#' @param name description
#' @export
#' @rdname theme_obi

theme_obi = function(x_axis_title_size = 12,
                     x_axis_text_size = 10,
                     y_axis_title_size = 12,
                     y_axis_text_size = 10,
                     title_size = 16,
                     title_hjust = 0.5,
                     subtitle_size = 12,
                     subtitle_hjust = 0.5,
                     caption_size = 12,
                     caption_hjust = 0) {
    theme_minimal() +
    theme(
      axis.title.x = element_text(color = OBI.color::prim_dark_blue(), size = x_axis_title_size),
      axis.text.x = element_text(color = OBI.color::prim_dark_blue(), size = x_axis_text_size),
      axis.title.y = element_text(color = OBI.color::prim_dark_blue(), size = y_axis_title_size),
      axis.text.y = element_text(color = OBI.color::prim_dark_blue(), size = y_axis_text_size),
      plot.title = element_text(
        color = OBI.color::prim_dark_blue(),
        size = title_size,
        hjust = title_hjust
      ),
      plot.subtitle = element_text(
        color = OBI.color::prim_dark_blue(),
        size = subtitle_size,
        hjust = subtitle_hjust
      ),
      plot.caption = element_text(
        color = OBI.color::prim_dark_blue(),
        size = caption_size,
        hjust = caption_hjust
      ),
      plot.background = element_rect(fill = "white", color = "white"),
      panel.background = element_rect(fill = "white", color = "white")
    )
}
