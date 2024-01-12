
#' Calculate TeamBirth compliance
#'
#' @export
#' @param df Data frame -- note that in order to have the correct variables for numerator and denominator calculation, data frame MUST first be run through obstinit::create_teambirth_cohort
#' @import tidyverse
#' @rdname calculate_TB_comp

calculate_TB_comp = function(df) {
  df %>%
    summarize(
      TB_num = sum(tb_full_compliance, na.rm = T),
      TB_denom = sum(birth),
      TB_comp_rate = TB_num / TB_denom
    )
}