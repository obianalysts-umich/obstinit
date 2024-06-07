

#' Format GT table 
#' 
#' @param gt_obj A gt object to be formatted.
#' @param heading.title.font.size Size for table title. Default is 14.
#' @param heading.subtitle.font.size Size for table subtitle. Default is 12.
#' @param column_labels.font.size Size for column labels. Default is 12.
#' @param row_group.font.size Size for row group labels. Default is 12.
#' @param stub.font.size Size for stub labels. Default is 12.
#' @param table.font.size Size for table content. Default is 12.
#' @param footnotes.font.size Size for footnotes. Default is 10.
#' @export
#' @rdname gt_obi

gt_obi <- function(
    gt_obj,
    heading.title.font.size = 14,
    heading.subtitle.font.size = 12,
    column_labels.font.size = 12,
    row_group.font.size = 12,
    stub.font.size = 12,
    table.font.size = 12,
    footnotes.font.size = 10
    
) {
  
  gt_obj |>
    # formatting table header
    tab_style(
      style = list(
        cell_fill(color = OBI.color::prim_dark_blue()),
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_title()
    ) |> 
    tab_options(
      heading.title.font.size = heading.title.font.size,
      heading.subtitle.font.size = heading.subtitle.font.size
    ) |> 
    # formatting row group headings
    tab_style(
      style = cell_fill(color = "gray90"),
      locations = cells_row_groups()
    ) |>
    # formatting table body
    tab_options(
      column_labels.font.weight = "bold",
      row_group.font.weight = "bold",
      column_labels.font.size = column_labels.font.size,
      row_group.font.size = row_group.font.size,
      stub.font.size = stub.font.size,
      table.font.size = table.font.size,
      footnotes.font.size = footnotes.font.size,
      footnotes.border.bottom.color = "gray90"
    )
}
