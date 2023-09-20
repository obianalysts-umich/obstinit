
#' Calculate TeamBirth compliance
#'
#' @export
#' @param df Data frame -- usually this is the data frame read into R using data.table::fread
#' @import tidyverse
#' @rdname calculate_TB_comp

calculate_TB_comp = function(df) {
  df %>% filter(planned_mode_of_delivery_cd == 1) %>% mutate(
    huddle_per_12h = ifelse(
      huddle_doc_b == 1,
      (huddle_doc_count_no / Admission_minus_delivery) * 12,
      0
    ),
    tb_full_compliance = ifelse(admit_huddle_mtg_compliance == 1 &
                                  huddle_per_12h >= 1, 1, 0)
  ) %>% summarize(
    TB_num = sum(tb_full_compliance),
    TB_denom = sum(birth),
    TB_comp_rate = TB_num / TB_denom
  )
}