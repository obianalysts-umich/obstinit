
#' Calculate cat II compliance
#'
#' @export
#' @param df Data frame -- usually this is the data frame read into R using data.table::fread
#' @import tidyverse
#' @rdname calculate_cat_II_comp

calculate_cat_II_comp = function(df) {
  df %>% filter(mode_of_delivery_cd == 4,
                ces_primary_indication_cd == 6,
                fht_category_e %in% c(1, 3)) %>% summarize(
                  cat_II_num = sum(ifelse(fht_mgt_documentation_e == 1, 1, 0)),
                  cat_II_denom = sum(birth),
                  cat_II_comp_rate = cat_II_num / cat_II_denom
                )
}