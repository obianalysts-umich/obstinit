
#' Create IA cohort
#'
#' @export
#' @param df Data frame -- usually this is the data frame read into R using data.table::fread
#' @param limit_to_IA_sites Whether cohort should be limited to 2023 IA sites; default is false
#' @import tidyverse
#' @rdname create_IA_cohort

calculate_IA_comp = function(df, limit_to_IA_sites = F) {
  df1 = df %>%
    mutate(to_drop = ifelse(ia_not_ordered_reason_e %in% c(1:6, 9:11), 1, 0)) %>%
    filter(
      planned_mode_of_delivery_cd == 1,
      admit_labor_status_cd %in% c(3, 4, 5),
      admit_fetal_monitor_type_e != "{99}",
      to_drop != 1
    )
  
  if (limit_to_IA_sites) {
    df1 %>%
      filter(QII_Choice_2023 == "IA Bundle")
  }
}