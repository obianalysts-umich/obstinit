
#' Create IA cohort
#'
#' @export
#' @param df Data frame -- usually this is obi_cohort created using obstinit::create_obi_cohort
#' @param limit_to_IA_sites Whether cohort should be limited to 2023 IA sites; default is TRUE
#' @import tidyverse
#' @rdname create_IA_cohort

create_IA_cohort = function(df, limit_to_IA_sites = T) {
  df1 <- df %>%
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
  } else{
    return(df1)
  }
}
