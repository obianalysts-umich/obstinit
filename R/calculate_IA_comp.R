
#' Calculate IA compliance
#'
#' @export
#' @param df Data frame -- ideally, run through obstinit::create_IA_cohort first
#' @import tidyverse
#' @rdname calculate_IA_comp

calculate_IA_comp = function(df) {
  df %>%
    summarize(
      IA_num = sum(admit_fm_IA, na.rm = T),
      IA_denom = sum(birth),
      IA_comp_rate = IA_num / IA_denom
    )
}
