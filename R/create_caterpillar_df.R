
#' Create a data frame for caterpillar plots
#' @description
#' this function creates a data frame to be used for caterpillar plots where each row is a site name. The df includes a numerator, denominator, and rate for the variable of interest, as well as a 95% CI (bounded by 0 and 1) for the rate.
#' 
#' @param df Data frame -- usually this is obi_cohort
#' @param num_var Numerator variable
#' @param denom_var Denominator variable
#' @param group_var Grouping variable; by default, this is site_name
#' @rdname create_caterpillar_df
#' @import tidyverse
#' @export

create_caterpillar_df = function(df, num_var, denom_var, group_var = site_name) {
  df |>
    group_by({
      {
        group_var
      }
    }) |>
    summarize(
      denom = sum({
        {
          denom_var
        }
      }, na.rm = T),
      num = sum({
        {
          num_var
        }
      }, na.rm = T),
      rate = num / denom
    ) |>
    obstinit::add_CI_values(rate, denom) |>
    mutate(LC = ifelse(LC < 0, 0, LC),
           UC = ifelse(UC > 1, 1, UC))
}
