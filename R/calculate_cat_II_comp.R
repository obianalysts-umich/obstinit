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
      cat_II_num = sum(cat2_compliance_flg == 1, na.rm = T),
      cat_II_denom = n(),
      cat_II_comp_rate = mean(cat2_compliance_flg == 1, na.rm = T)
    )
}
