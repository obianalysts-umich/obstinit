
#' Add benchmark to plot
#' @description
#' this function adds a benchmark line (and optional label) to a ggplot object
#' 
#' @param plot ggplot object
#' @param y_int Yintercept for geom_hline
#' @param plot_linetype Type of line for benchmark; default is "dashed"
#' @param plot_linecolor Color of line for benchmark (and benchmark label, if included); default is OBI.color::prim_pink()
#' @param plot_linewidth Width of line for benchmark; default is "0.8
#' @param add_label Whether or not benchmark should be labelled; default is FALSE
#' @param label_text Text for benchmark label
#' @param label_size Size of label text; default is 5
#' @param label_xval x position for benchmark label
#' @param label_yval y position for benchmark label
#' @rdname add_benchmark
#' @import tidyverse
#' @import ggplot2
#' @export

add_benchmark = function(plot,
                         y_int,
                         plot_linetype = "dashed",
                         plot_linecolor = OBI.color::prim_pink(),
                         plot_linewidth = 0.8,
                         add_label = F,
                         label_text = NA,
                         label_size = 5,
                         label_xval = NA,
                         label_yval = NA) {
  h = plot +
    geom_hline(
      aes(yintercept = y_int),
      linetype = plot_linetype,
      color = plot_linecolor,
      linewidth = plot_linewidth
    )
  
  if (add_label) {
    h +
      geom_text(
        aes(x = label_xval, y = y_int + y_int * 0.02),
        label = label_text,
        size = 5,
        color = plot_linecolor
      )
  }
  else{
    return(h)
  }
}
