#' Calculate cat II compliance
#'
#' @export
#' @param df Data frame
#' @import tidyverse
#' @rdname calculate_cat_II_comp

calculate_cat_II_comp <- function(df) {
  df |>
    filter(!is.na(cat2_compliance_flg)) |>
    summarize(
      num = sum(cat2_compliance_flg == 1, na.rm = T),
      denom = n(),
      rate = mean(cat2_compliance_flg == 1, na.rm = T)
    )
}
