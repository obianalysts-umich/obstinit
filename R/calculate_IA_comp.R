
#' Calculate IA compliance
#'
#' @export
#' @param df Data frame -- usually this is the data frame read into R using data.table::fread
#' @import tidyverse
#' @rdname calculate_IA_comp

calculate_IA_comp = function(df) {
  df %>% filter(admit_labor_status_cd %in% c(3, 4),!ia_not_ordered_reason_e %in% c(1:6, 9:11)) %>% summarize(
    IA_num = sum(sum(admit_fm_IA, na.rm = T)),
    IA_denom = sum(birth),
    IA_comp_rate = IA_num / IA_denom
  )
}