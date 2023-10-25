
#' Calculate cat II compliance
#'
#' @export
#' @param df Data frame -- ideally, run through obstinit::create_cat_II_cohort first
#' @import tidyverse
#' @rdname calculate_cat_II_comp

calculate_cat_II_comp = function(df) {
  df %>%
    summarize(
      cat_II_num = sum(ifelse(fht_mgt_documentation_e == 1, 1, 0)),
      cat_II_denom = sum(birth),
      cat_II_comp_rate = cat_II_num / cat_II_denom
    )
}