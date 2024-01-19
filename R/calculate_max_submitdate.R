
#' Calculate maximum submitdate
#' @description
#' this function creates a variable called "submitdate_max" that is the maximum of the other submitdate fields in the dataset. This is primarily used for tracking completeness and timeliness of abstracted data per the 2023 and 2024 OBI scorecards.
#' 
#' @param df Data frame -- usually this is obi_cohort created with obstinit::read_current_data() |> obstinit::create_obi_cohort()
#' @import tidyverse
#' @export
#' @rdname calculate_max_submitdate

calculate_max_submitdate <- function() {
  df |>
    select(!c(submitdate, Record)) |>
    mutate(across(starts_with("submit"), ~ lubridate::dmy_hms(.x))) |>
    rowwise() |>
    mutate(submitdate_max = max(
      c_across(submitdate.abs_delivery:submitdate_abs_visit_4),
      na.rm = T
    )) |>
    ungroup()
}