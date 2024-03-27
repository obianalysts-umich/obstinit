#' Calculate summary statistics for a binary variable
#'
#' @description
#' This function calculates summary statistics for a binary variable in a given dataset.
#' It counts the number of occurrences for each level of the variable, calculates the numerator and denominator,
#' and computes the rate as a percentage.
#'
#' @param data The dataset containing the binary variable.
#' @param flg_var The name of the binary variable.
#'
#' @return A data frame with the following columns:
#'   - Measure: The name of the binary variable without the "_flg" suffix.
#'   - Numerator: The count of occurrences for the level 1 of the binary variable.
#'   - Denominator: The sum of counts for both levels (0 and 1) of the binary variable.
#'   - Rate: The percentage calculated as the numerator divided by the denominator.
#'
#' @examples
#' data <- data.frame(flg_var = c(0, 1, 1, 0, 1))
#' summary_num_denom_rate(data, flg_var)
summary_num_denom_rate <- function(
    data,
    flg_var) {
  data |>
    mutate({{ flg_var }} := factor({{ flg_var }}, levels = c(0, 1))) |>
    count({{ flg_var }}, .drop = FALSE) |>
    filter(!is.na({{ flg_var }})) |>
    pivot_wider(names_from = {{ flg_var }}, values_from = n) |>
    transmute(
      Measure = quo_name(enquo(flg_var)),
      Measure = str_remove(Measure, "_flg"),
      Numerator = `1`,
      Denominator = sum(c(`0`, `1`)),
      Rate = scales::percent(Numerator / Denominator, 1)
    )
}
