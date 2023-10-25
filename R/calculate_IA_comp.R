
#' Calculate IA compliance
#'
#' @export
#' @param df Data frame -- usually this is the data frame read into R using data.table::fread
#' @import tidyverse
#' @rdname calculate_IA_comp

calculate_IA_comp = function(df) {
  df %>%
    mutate(to_drop = ifelse(ia_not_ordered_reason_e %in% c(1:6, 9:11), 1, 0)) %>%
    filter(
      planned_mode_of_delivery_cd == 1,
      admit_labor_status_cd %in% c(3, 4, 5),
      admit_fetal_monitor_type_e != "{99}",
      to_drop != 1
    ) %>%
    summarize(
      IA_num = sum(admit_fm_IA, na.rm = T),
      IA_denom = sum(birth),
      IA_comp_rate = IA_num / IA_denom
    )
}
